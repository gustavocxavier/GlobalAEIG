# H1b: LC proxies are relevant
# Compute ElasticNet

library(data.table)
library(lubridate)
library(glmnet)
library(tidyverse)
library(foreach)

# # source_python('functions.py')
# source('functions_eig.R')

## Load Data -------------------------------------------------------------------
# All Financial Variables Elastic Net

ccm_a <- readRDS("2_pipeline/2_out/1b_ccm_a.rds")
crsp_m <- readRDS(file = "2_pipeline/2_out/1a_cleaned_crsp_m.rds")

ccm_a %>%
  left_join(crsp_m %>% select(permno, monthlink, Ret=cumret)) %>%
  select(gvkey, permno, datadate,
                 IGt1 = d1_ia, me, IGt0 = d0_ia, # I/A change as Invest. Growth
                 # IGt1 = [01mIGc[m1, me, IGt0 = [01mIGc[m0, # CAPEX growth as Invest. Growth
                 q, Ret, SG, CFG, CF, cop, PG, EG, dROE, Ie, Id) -> ccm_a

ccm_a %>% filter(!is.na(IGt1)) %>% arrange(datadate)
ccm_a <- ccm_a %>% na.omit
ccm_a %>% arrange(datadate)

## Remove Duplicated Values
ccm_a %>%
  mutate(year = year(datadate)) %>% 
  mutate(id_year = paste(permno, year)) %>% # Remove duplicated permno
  filter(!duplicated(id_year)) %>%
  mutate(id_year = paste(gvkey, year)) %>%  # Remove duplicated gvkey
  filter(!duplicated(id_year, fromLast = T)) %>%
  mutate(id_year = NULL) %>% 
  select(gvkey:datadate, year, everything()) -> ccm_a

## Check if there is duplicated rows
ccm_a %>% group_by(permno, year) %>% count %>% filter(n>1)
ccm_a %>% group_by(gvkey, year) %>% count %>% filter(n>1)

ccm_a %>% arrange(datadate) %>% as.data.table

## Winsorize all predictive variables ------------------------------------------
# Hou, Xue, Mo and Zhang winsorize at the 1% and 99% levels BOTH side.
winsorize <- function (x, fraction=0.01) {
  # Source: https://www.r-bloggers.com/winsorization/
  #
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) { stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}
setDT(ccm_a)
ccm_a[, IGt1 := winsorize(IGt1), by=c("datadate")]
ccm_a[, IGt0 := winsorize(IGt0), by=c("datadate")]
ccm_a[, me   := winsorize(me  ), by=c("datadate")]
ccm_a[, q    := winsorize(q   ), by=c("datadate")]
ccm_a[, SG   := winsorize(SG  ), by=c("datadate")]
ccm_a[, CFG  := winsorize(CFG ), by=c("datadate")]
ccm_a[, CF   := winsorize(CF  ), by=c("datadate")]
ccm_a[, cop  := winsorize(cop ), by=c("datadate")]
ccm_a[, PG   := winsorize(PG  ), by=c("datadate")]
ccm_a[, dROE := winsorize(dROE), by=c("datadate")]
ccm_a[, EG   := winsorize(EG  ), by=c("datadate")]

nrow(ccm_a)
nrow(na.omit(ccm_a))
ccm_a <- na.omit(ccm_a)

### Using Elastic Net recursively ----------------------------------------------

sort(unique(year(ccm_a$datadate)))

predictENet <- function(YEAR, db, n_years_training=4, n_years_validation=1, weighted=FALSE) {
  
  nT <- n_years_training
  nV <- n_years_validation
  
  setDT(db)
  db <- db[is.finite(rowSums(db[, -(1:4)])),]
  
  trainingYEARS <- (YEAR-nV-nT):(YEAR-1-nV)
  validationYEARS <- (YEAR-nV):(YEAR-1)
  
  
  ## Train Sample
  db %>% arrange(datadate) %>% filter(year(datadate) %in% trainingYEARS) -> trainSample
  X.train <- model.matrix(IGt1 ~ ., data = trainSample[,-c((1:4),6)])[,-1]
  y.train <- trainSample$IGt1
  
  if (weighted == T) { w.train <- trainSample$me } else { w.train <- NULL }
  
  ## Validation Sample
  db %>% arrange(datadate) %>% filter(year(datadate) %in% validationYEARS) -> validationSample
  X.validation <- model.matrix(IGt1 ~ ., data = validationSample[,-c((1:4),6)])[,-1]
  y.validation <- validationSample$IGt1
  
  ## Test Sample
  db %>% arrange(datadate) %>% filter(year(datadate) %in% YEAR) -> testSample
  X.test <- model.matrix(IGt1 ~ ., data = testSample[,-c((1:4),6)])[,-1]
  y.test <- testSample$IGt1
  
  grids <- expand.grid(alpha = seq(from = 0.1, to = 0.9, by = 0.1),
                       lambda =  round(seq(from = 0.01, to = 0.99, length.out = 10),2))
  grids <- grids[order(grids$alpha),]
  
  grids$MSE <- foreach(i = 1:nrow(grids), .combine = c) %do% {
    # cat(paste0("Fiting ", i, " from ", nrow(grids), "\n"))
    fit <- glmnet(x = X.train, y = y.train, family = "gaussian",
                  weights = w.train, # It is worth to include market-value as weight?
                  alpha = grids$alpha[i], lambda = grids$lambda[i])
    mean( ( y.validation - predict(fit, newx = X.validation, s = grids$lambda[i]) )^2 )
  }
  # grids[which.min(grids$MSE),]
  
  optimal_alpha <- grids[which.min(grids$MSE),"alpha"]
  optimal_lambda <- grids[which.min(grids$MSE),"lambda"]
  
  fit <- glmnet(x = X.test, y = y.test, family = "gaussian",
                alpha = optimal_alpha, lambda = optimal_lambda)
  
  # coef(fit, s = optimal_lambda)
  
  testSample %>% select(gvkey:IGt1) -> testSample
  
  testSample$EIG <- predict(fit, newx = X.test, s = optimal_lambda)
  # eigENet <- rbind(output, testSample)
  
  testSample %>% mutate(SE = (IGt1 - EIG)^2) -> testSample
  
  output <- list(optimal_tuning=grids[which.min(grids$MSE),],
                 model = fit,
                 prediction = testSample)
  return(output)
}
# debug(predictENet)
# f1975 <- predictENet(1975, db = ccm_a, n_years_training = 3, weighted = TRUE)
# f1989 <- predictENet(1989, db = ccm_a, n_years_training = 3, weighted = TRUE)
# f1975$optimal_tuning
# f1975$prediction
# coef(f1975$model)

require(pbapply)
op <- pboptions(type = "timer", char="=")
# EIG_ENet = pblapply(1975:2018, predictENet, db = ccm_a, n_years_training = 4, weighted = FALSE)
# TODO: Error in { : task 1 failed - "y is constant; gaussian glmnet fails at standardization step"
# ccm_a so esta com dados a partir de 1989
(ccm_a %>% select(year) %>% arrange(year) %>% as.matrix)[1,1]+4
EIG_ENet = pblapply(1989:2018, predictENet, db = ccm_a, n_years_training = 4, weighted = FALSE)
saveRDS(EIG_ENet, "2_pipeline/2_out/3a_EIG_ENet.rds")
EIG_ENet <- readRDS("2_pipeline/2_out/3a_EIG_ENet.rds")


EIG_ENet_serie <- lapply(EIG_ENet, "[[", 3)
EIG_ENet_serie <- data.table::rbindlist(EIG_ENet_serie, fill = T)

lc <- as_tibble(readRDS("2_pipeline/2_out/2a_life_cycle_faff.rds"))
# TODO: Trazer codigo do EIG original para o pipeline?
# HMXZ <- readRDS("~/Data/eig/hmxz.rds")
HMXZ <- readRDS("2_pipeline/2_out/2c_hmxz.rds")
HMXZ[, SE := (d1_ia - EIG)^2]

mean(HMXZ$SE)
mean(HMXZ[month(date)==6,]$SE)
mean(EIG_ENet_serie$SE)

saveRDS(EIG_ENet_serie, "2_pipeline/2_out/3a_EIG_ENet_serie.rds")

EIG_ENet <- readRDS("2_pipeline/2_out/3a_EIG_ENet.rds")
library(glmnet)
EIG_ENet_coef <- lapply(EIG_ENet, "[[", 2)
EIG_ENet_coef <- lapply(EIG_ENet_coef, coef)
EIG_ENet_coef <- lapply(EIG_ENet_coef, t)
EIG_ENet_coef <- lapply(EIG_ENet_coef, as.matrix)
EIG_ENet_coef <- lapply(EIG_ENet_coef, as.data.frame)
EIG_ENet_coef <- data.table::rbindlist(EIG_ENet_coef, fill = T)
VI <- data.frame(round(apply(EIG_ENet_coef, 2, mean),3))
VI$variables <- rownames(VI)
colnames(VI)[1] <- c("coefficients")
VI %>%
  mutate(coefficients = abs(coefficients)) %>%
  filter(variables!="(Intercept)") %>% 
  arrange(-coefficients) %>% as.data.table

## Incluce Life-Cycle Proxies as predictors -----------------------------------

## Include RETA
# Calculate RETA as the ratio of retained earnings to total assets
lcycle <- readRDS("0_data/wrds/raw_comp_a_lifecycle_data.rds")
lcycle[is.na(re), re := 0] # Set missing retained earnings figures to zero
lcycle[, reta := re/at] # Calculate Reta

ccm_a %>% left_join(lcycle %>% select(gvkey, datadate, reta),
                    by = c("gvkey", "datadate")) -> LC_ccm_a

## Include DCS Life-Cycle Proxy
# LC <- as_tibble(readRDS("~/Data/EIG/life_cycle.rds"))
# LC %>% select(gvkey, datadate, LC) %>% 
#   mutate(LCintro  = if_else(LC2 == 1, 1, 0)) %>% 
#   mutate(LCgrowth = if_else(LC2 == 2, 1, 0)) %>% 
#   mutate(LCshadec = if_else(LC2 == 3, 1, 0)) %>%
#   mutate(LCmature = if_else(LC2 == 4, 1, 0)) %>%
#   select(gvkey, datadate, LC = LC2, LCintro, LCgrowth, LCshadec, LCmature) -> LC

LC <- readRDS("2_pipeline/2_out/2a_life_cycle_faff.rds")
LC %>% select(gvkey, datadate, faff, DCS) %>% 
  mutate(LCintro  = if_else(faff == 1, 1, 0)) %>% 
  mutate(LCgrowth = if_else(faff == 2, 1, 0)) %>% 
  mutate(LCshadec = if_else(faff == 3, 1, 0)) %>%
  mutate(LCmature = if_else(faff == 4, 1, 0)) %>%
  mutate(DCSintro  = if_else(DCS == 1, 1, 0)) %>% 
  mutate(DCSgrowth = if_else(DCS == 2, 1, 0)) %>% 
  mutate(DCSshadec = if_else(DCS == 3, 1, 0)) %>%
  mutate(DCSmature = if_else(DCS == 4, 1, 0)) %>%
  select(gvkey, datadate,
         LC_faff=faff, LC_DCS=DCS,
         LCintro, LCgrowth, LCshadec, LCmature,
         DCSintro, DCSgrowth, DCSshadec, DCSmature) -> LC

LC_ccm_a %>% left_join(LC, by=c("gvkey", "datadate")) %>% as.data.table -> LC_ccm_a

LC_ccm_a[is.na(LCintro  ), LCintro   := 0]
LC_ccm_a[is.na(LCgrowth ), LCgrowth  := 0]
LC_ccm_a[is.na(LCshadec ), LCshadec  := 0]
LC_ccm_a[is.na(LCmature ), LCmature  := 0]
LC_ccm_a[is.na(DCSintro  ), LCintro   := 0]
LC_ccm_a[is.na(DCSgrowth ), LCgrowth  := 0]
LC_ccm_a[is.na(DCSshadec ), LCshadec  := 0]
LC_ccm_a[is.na(DCSmature ), LCmature  := 0]
LC_ccm_a <- LC_ccm_a[complete.cases(LCintro,
                                    LC_faff, LC_DCS, 
                                    LCgrowth, LCshadec, LCmature,
                                    DCSintro, DCSgrowth, DCSshadec, DCSmature)]
LC_ccm_a[,LC_faff := as.numeric(as.character(LC_faff))]



# LC_ccm_a[year>=1990]

# LC_ccm_a %>% arrange(year)
glimpse(LC_ccm_a)

require(pbapply)
op <- pboptions(type = "timer", char="=")
# debug(predictENet)
## Definir o range de previsao a partir do menor ano + 5 (3 train + 1 valid + 1 test)
years_sample <- (head(sort(LC_ccm_a$year),1)+5):tail(sort(LC_ccm_a$year),1)
# years_sample <- 1979:2018

# LC_ccm_a2 <- select(LC_ccm_a, -(LCintro:LCmature))
LC_ccm_a2 <- select(LC_ccm_a, -(DCSintro:DCSmature))
LC_EIG_ENet = pblapply(years_sample, predictENet, db = LC_ccm_a2, n_years_training = 3, weighted = TRUE)
LC_EIG_ENet_serie <- lapply(LC_EIG_ENet, "[[", 3)
LC_EIG_ENet_serie <- data.table::rbindlist(LC_EIG_ENet_serie, fill = T)

t.value_dif <- function(x,y) {
  return(t.test(x,y)$statistic)
}
EIG_ENet_serie %>%
  inner_join(select(LC_EIG_ENet_serie, gvkey, datadate, LC_EIG=EIG, LC_SE=SE),
             by = c("gvkey","datadate")) %>% 
  summarise(Mean_SE=round(mean(SE),3),
            Mean_LC_SE=round(mean(LC_SE),3),
            Diff = round(mean(SE - LC_SE),3),
            t.value=round(t.value_dif(SE, LC_SE),3))

# diff_test <- EIG_ENet_serie %>%
#   inner_join(select(LC_EIG_ENet_serie, gvkey, datadate, LC_EIG=EIG, LC_SE=SE),
#              by = c("gvkey","datadate"))


# TODO Add other proxies variable as AdjustedAge EBIT and AGrth 

## Table - Variable Importance -------------------------------------------------
LC_EIG_ENet_coef <- lapply(LC_EIG_ENet, "[[", 2)
LC_EIG_ENet_coef <- lapply(LC_EIG_ENet_coef, coef)
LC_EIG_ENet_coef <- lapply(LC_EIG_ENet_coef, t)
LC_EIG_ENet_coef <- lapply(LC_EIG_ENet_coef, as.matrix)
LC_EIG_ENet_coef <- lapply(LC_EIG_ENet_coef, as.data.frame)
LC_EIG_ENet_coef <- data.table::rbindlist(LC_EIG_ENet_coef, fill = T)
LC_VI <- data.frame(round(apply(LC_EIG_ENet_coef, 2, mean),3))
LC_VI$variables <- rownames(LC_VI)
colnames(LC_VI)[1] <- c("coefficients")
LC_VI <- as.data.table(LC_VI)[variables!="(Intercept)"][order(coefficients^2, decreasing = T)]

## Result in Latex -------------------------------------------------------------
library(xtable)

VI <- as.data.table(VI)[variables!="(Intercept)"][order(coefficients^2, decreasing = T)]
temp <- nrow(LC_VI)-nrow(VI)
temp <- data.frame(variables=rep("",temp), coefficients=rep(0,temp))
temp <- rbind(VI[,c(2,1)], temp)
temp <- cbind(temp, temp[,1])
temp[,3] <- "" ; names(temp)[3] <- " "
temp <- cbind(temp, LC_VI[,c(2,1)])
temp # ; xtable(temp)
print(xtable(temp), file="3_output/results/3a_EIG_LifeCycle_based_VI_tex.txt")
print(xtable(temp), file="3_output/results/3a_EIG_LifeCycle_based_VI_htm.html", type="html")

temp <- EIG_ENet_serie %>%
  inner_join(select(LC_EIG_ENet_serie, gvkey, datadate, LC_EIG=EIG, LC_SE=SE),
             by = c("gvkey","datadate")) %>% 
  summarise(Mean_SE=round(mean(SE),3),
            Mean_LC_SE=round(mean(LC_SE),3),
            Diff = round(mean(SE - LC_SE),3),
            t.value=round(t.value_dif(SE, LC_SE),3))
temp ; xtable(temp)
print(xtable(temp), file="3_output/results/3a_EIG_LifeCycle_based_MSE_tex.txt")
print(xtable(temp), file="3_output/results/3a_EIG_LifeCycle_based_MSE_htm.html", type="html")


