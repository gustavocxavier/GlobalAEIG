# Report H2a: EIG Growth > EIG Mature
#




## Include EIG_ENet as Expected Growth proxy
library(tidyverse)
library(data.table)
library(glmnet)
library(xtable)
library(stargazer)
library(readxl)

NW <- function (x, Lag=NULL) {
  require(lmtest)
  require(sandwich)
  coeftest(x, vcov=NeweyWest(x, lag = Lag, prewhite = FALSE))
}


# Adjust standard errors
robust.se <- function(x, Lag=NULL) {
  require(lmtest)
  require(sandwich)
  coeff <- coeftest(x, vcov=NeweyWest(x, lag = Lag, prewhite = FALSE))
  output <- list()
  output$t.stat <- coeff[,"t value"]
  output$p.val <- coeff[,"Pr(>|t|)"]
  return(output)
}

lc <- as_tibble(readRDS("2_pipeline/2_out/2a_life_cycle_faff.rds"))
crsp4 <- readRDS("2_pipeline/2_out/1b_crsp4.rds")

# HMXZ <- readRDS("~/Data/eig/hmxz.rds")
HMXZ <- readRDS("2_pipeline/2_out/2c_hmxz.rds")

# EIG_ENet_a <- readRDS("~/Data/EIG/EIG_ENet_serie.rds")
crsp4 <- readRDS("2_pipeline/2_out/1b_crsp4.rds")
EIG_ENet <- readRDS("2_pipeline/2_out/3a_EIG_ENet.rds")
EIG_ENet[[1]] # TODO Corrigir year que esta por datadate quando deveria ser o ano fiscal
EIG_ENet_coef <- lapply(EIG_ENet, "[[", 2)
EIG_ENet_coef <- lapply(EIG_ENet_coef, coef)
EIG_ENet_coef <- lapply(EIG_ENet_coef, t)
EIG_ENet_coef <- lapply(EIG_ENet_coef, as.matrix)
EIG_ENet_coef <- lapply(EIG_ENet_coef, as.data.frame)
EIG_ENet_coef <- data.table::rbindlist(EIG_ENet_coef, fill = T)
names(EIG_ENet_coef)[1] <- "b0"
names(EIG_ENet_coef)[-1] <- paste0("b", toupper(names(EIG_ENet_coef)[-1]))
# EIG_ENet_coef$year <- 1975:2018
EIG_ENet_coef$year <- (2018-nrow(EIG_ENet_coef)+1):2018
EIG_ENet_coef <- as_tibble(EIG_ENet_coef)

crsp4 %>% mutate(year = year(monthlink)) %>% 
  left_join(EIG_ENet_coef, by="year") %>% 
  group_by(permno) %>% 
  mutate(IGt0 = dplyr::lag(d1_ia)) %>% 
  ungroup %>% 
  mutate(EIG = b0 + bIGT0 * IGt0 + bQ*q + bRET*Ret + bSG*SG + bCFG*CFG + bCF*CF +
           bCOP*cop + bPG*PG + bEG*EG + bDROE*dROE + bIE*Ie + bID*Id) -> EIG_ENet_m

# saveRDS(EIG_ENet_m,"~/Data/EIG/EIG_ENet_m.rds")



# ccm_a <- ccm_a %>% inner_join(EIG_ENet %>% select(gvkey, datadate, EIG),
#                               by = c("gvkey", "datadate")) %>%
#   mutate(IGt1 = EIG)

#eg_m <- HMXZ %>% select(permno, gvkey, date, eg=EIG) %>% na.omit %>% as_tibble

eg_m <- EIG_ENet_m %>% select(permno, gvkey, date, eg=EIG) %>% na.omit

# ## Future Realized Growth as d1_ia
# eg_m <- crsp4 %>% select(permno, gvkey, date, eg=d1_ia) %>% na.omit %>% as_tibble

## Future Realized Growth as log(CAPEXt,CAPEXt_1)

## Merge Life cycle Proxy and EIG (HMXZ)
lc <- lc %>% mutate(date = datadate) %>% select(gvkey, date, LC=faff) %>% as_tibble
eg_m <- eg_m %>% left_join(lc, by = c("gvkey", "date"))

### Criar carteiras ------------------------------------------------------------
#
# Codigo em desenvolvimento baseado no criaCarteiras
# https://github.com/kleberformiga/criaCarteiras/blob/master/R/criaCartFatRisco.R
#

## Carregar dados crsp
# crsp <- readRDS("~/data/WRDS/crsp4.rds")
# crsp_retadj <- readRDS("~/data/WRDS/cleaned_crsp_m.rds")
# crsp_retadj <- crsp_retadj %>% select(permno, date, retadj)
# crsp <- crsp %>% left_join(crsp_retadj, by = c("permno", "date"))
# crsp

# TODO: py_ccm4 <- arrow::read_feather("~/Data/eig/ccm4.feather")
py_ccm4 <- arrow::read_feather("~/Data/eig/ccm4.feather")
py_ccm4 <- py_ccm4 %>%
  select(permno, date=jdate, szport, retadj, wt) %>%
  mutate(date = as.Date(date))
eg_m <- eg_m %>% left_join(py_ccm4, by = c("permno", "date")) %>%  drop_na

## TODO: Fazer série de AEIG
## AEIG ----------------------------------------------------------
as_tibble(eg_m) %>% group_by(date) %>%
  filter(is.finite(eg)) %>% 
  summarise(AEIG = weighted.mean(eg, wt)) %>%
  arrange(date) %>% as.data.table -> AEIG

## Retorno Growth Agregado -----------------------------------------------------
as_tibble(eg_m) %>% group_by(LC, date) %>%
  summarise(AEIG = weighted.mean(eg, wt)) %>%
  filter(LC == 2) %>% arrange(date) %>% ungroup %>% 
  select(date, AEIGgrowth = AEIG) %>% as.data.table -> AEIGgrowth


## Retorno Madura Agregado -----------------------------------------------------
as_tibble(eg_m) %>% group_by(LC, date) %>%
  summarise(AEIG = weighted.mean(eg, wt)) %>%
  filter(LC == 3) %>% arrange(date) %>% ungroup %>% 
  select(date, AEIGmature = AEIG) %>% as.data.table -> AEIGmature

## Retorno de Mercado ----------------------------------------------------------
as_tibble(eg_m) %>% group_by(date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) %>% as.data.table -> MKT

 lmdata <- MKT %>% left_join(AEIG) %>%
  mutate(AEIG = dplyr::lag(AEIG)) %>%
  select(date, MKT=vwret, AEIG) %>% na.omit
lmdata[, MKT3M := Reduce(`+`, shift(MKT, 0:3))]
lmdata[, MKT6M := Reduce(`+`, shift(MKT, 0:6))]
lmdata[, MKT1Y := Reduce(`+`, shift(MKT, 0:6))]
lmdata[, MKT2Y := Reduce(`+`, shift(MKT, 0:24))]
lmdata[, MKT3Y := Reduce(`+`, shift(MKT, 0:36))]
lmdata[, MKT5Y := Reduce(`+`, shift(MKT, 0:60))]
lmdata
summary(lm(MKT   ~ AEIG, data = na.omit(lmdata)))
summary(lm(MKT3M ~ AEIG, data = na.omit(lmdata)))
summary(lm(MKT6M ~ AEIG, data = na.omit(lmdata)))
summary(lm(MKT1Y ~ AEIG, data = na.omit(lmdata)))
summary(lm(MKT2Y ~ AEIG, data = na.omit(lmdata)))
summary(lm(MKT3Y ~ AEIG, data = na.omit(lmdata)))
summary(lm(MKT5Y ~ AEIG, data = na.omit(lmdata)))

 ## Retorno Growth Agregado -----------------------------------------------------
as_tibble(eg_m) %>% group_by(LC, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  filter(LC == 2)
  arrange(date)

## Retorno Madura Agregado -----------------------------------------------------
as_tibble(eg_m) %>% group_by(LC, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  filter(LC == 3)
  arrange(date)


# Importar ccm4 from python

## All 
DT <- as.data.table(eg_m)
DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_5portAll

DT <- as.data.table(eg_m)
DT[                      eg<=quantile(eg,0.1), portfolio := 1, by=date]
DT[eg>quantile(eg,0.1) & eg<=quantile(eg,0.2), portfolio := 2, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.3), portfolio := 3, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.4), portfolio := 4, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.5), portfolio := 5, by=date]
DT[eg>quantile(eg,0.5) & eg<=quantile(eg,0.6), portfolio := 6, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.7), portfolio := 7, by=date]
DT[eg>quantile(eg,0.7) & eg<=quantile(eg,0.8), portfolio := 8, by=date]
DT[eg>quantile(eg,0.8) & eg<=quantile(eg,0.9), portfolio := 9, by=date]
DT[eg>quantile(eg,0.9),                        portfolio := 10, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_10portAll

## Growth Ports
DT <- as.data.table(eg_m)[LC == 2] # Introduction and Growth
DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_5portGrowth

## Growth Ports
DT <- as.data.table(eg_m)[LC == 2] # Introduction and Growth
DT[                      eg<=quantile(eg,0.3), portfolio := 1, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.7), portfolio := 2, by=date]
DT[eg>quantile(eg,0.7),                        portfolio := 3, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) %>% ungroup -> eg_3portGrowth

DT <- as.data.table(eg_m)[LC == 2]
DT[                      eg<=quantile(eg,0.1), portfolio := 1, by=date]
DT[eg>quantile(eg,0.1) & eg<=quantile(eg,0.2), portfolio := 2, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.3), portfolio := 3, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.4), portfolio := 4, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.5), portfolio := 5, by=date]
DT[eg>quantile(eg,0.5) & eg<=quantile(eg,0.6), portfolio := 6, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.7), portfolio := 7, by=date]
DT[eg>quantile(eg,0.7) & eg<=quantile(eg,0.8), portfolio := 8, by=date]
DT[eg>quantile(eg,0.8) & eg<=quantile(eg,0.9), portfolio := 9, by=date]
DT[eg>quantile(eg,0.9),                        portfolio := 10, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_10portGrowth

## Mature Ports
DT <- as.data.table(eg_m)[LC == 3] # Mature
DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_5portMature

DT <- as.data.table(eg_m)[LC == 3]
DT[                      eg<=quantile(eg,0.1), portfolio := 1, by=date]
DT[eg>quantile(eg,0.1) & eg<=quantile(eg,0.2), portfolio := 2, by=date]
DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.3), portfolio := 3, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.4), portfolio := 4, by=date]
DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.5), portfolio := 5, by=date]
DT[eg>quantile(eg,0.5) & eg<=quantile(eg,0.6), portfolio := 6, by=date]
DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.7), portfolio := 7, by=date]
DT[eg>quantile(eg,0.7) & eg<=quantile(eg,0.8), portfolio := 8, by=date]
DT[eg>quantile(eg,0.8) & eg<=quantile(eg,0.9), portfolio := 9, by=date]
DT[eg>quantile(eg,0.9),                        portfolio := 10, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) -> eg_10portMature

DT <- as.data.table(eg_m)[LC == 3]
DT[                      eg<=quantile(eg,0.3), portfolio := 1, by=date]
DT[eg>quantile(eg,0.3) & eg<=quantile(eg,0.7), portfolio := 2, by=date]
DT[eg>quantile(eg,0.7),                        portfolio := 3, by=date]
as_tibble(DT) %>% group_by(portfolio, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, wt)) %>%
  arrange(date) %>% ungroup -> eg_3portMature

## Analyse portfolios Deciles --------------------------------------------------
# TODO: Export results to 3_output/results

eg_10portAll %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3), .groups = 'drop') %>% select(retorno) %>% data.frame
eg_10portGrowth %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3), .groups = 'drop') %>% select(retorno) %>% data.frame
eg_10portMature %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3), .groups = 'drop') %>% select(retorno) %>% data.frame

## Analyse portfolios Quintiles
eg_5portAll %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3), .groups = 'drop') %>% select(retorno) %>% data.frame
eg_5portGrowth %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3), .groups = 'drop') %>% select(retorno) %>% data.frame
eg_5portMature %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3), .groups = 'drop') %>% select(retorno) %>% data.frame

## Difference using deciles -----
inner_join(eg_10portGrowth %>% ungroup %>% filter(portfolio==10) %>% select(date, highG=vwret),
           eg_10portGrowth %>% ungroup %>% filter(portfolio==1) %>% select(date, lowG=vwret),
           by = "date") %>%
  mutate(eigGrowth = highG - lowG) -> eigGrowth

inner_join(eg_10portMature %>% ungroup %>% filter(portfolio==10) %>% select(date, highM=vwret),
           eg_10portMature %>% ungroup %>% filter(portfolio==1) %>% select(date, lowM=vwret),
           by = "date") %>%
  mutate(eigMature = highM - lowM) -> eigMature

eigLC <- inner_join(eigGrowth, eigMature, by = "date") %>% 
  mutate(eigDifference = eigGrowth - eigMature)

summary(eigLC$eigDifference*100)
sd(eigLC$eigDifference*100)
t.test(eigLC$eigDifference*100)

## Difference using quintiles -----
inner_join(eg_5portGrowth %>% ungroup %>% filter(portfolio==5) %>% select(date, highG=vwret),
           eg_5portGrowth %>% ungroup %>% filter(portfolio==1) %>% select(date, lowG=vwret),
           by = "date") %>%
  mutate(eigGrowth = highG - lowG) -> eigGrowth

inner_join(eg_5portMature %>% ungroup %>% filter(portfolio==5) %>% select(date, highM=vwret),
           eg_5portMature %>% ungroup %>% filter(portfolio==1) %>% select(date, lowM=vwret),
           by = "date") %>%
  mutate(eigMature = highM - lowM) -> eigMature

eigLC <- inner_join(eigGrowth, eigMature, by = "date") %>% 
  mutate(eigDifference = eigGrowth - eigMature)

summary(eigLC[,-1]*100)

summary(eigLC$eigDifference*100)
sd(eigLC$eigDifference*100)
t.test(eigLC$eigDifference*100)

# Output result quintile_difference --------------------------------------------
eigLC %>% summarise(Growth=mean(eigGrowth),
                    Mature=mean(eigMature),
                    Diff  =mean(eigDifference)) -> df1
eigLC %>% summarise(Growth=sd(eigGrowth),
                    Mature=sd(eigMature),
                    Diff  =sd(eigDifference)) -> df2
quintile_difference <- cbind(rbind(df1, df2), rbind(t.test(eigLC$eigDifference*100)$statistic,0))
print(xtable(quintile_difference, digits = 3), file="3_output/results/3c_quintile_difference_tex.txt", type="latex")
print(xtable(quintile_difference, digits = 3), file="3_output/results/3c_quintile_difference_htm.html", type="html")

# ## Difference using 30% and 70% -----
# inner_join(eg_3portGrowth %>% ungroup %>% filter(portfolio==3) %>% select(date, highG=vwret),
#            eg_3portGrowth %>% ungroup %>% filter(portfolio==1) %>% select(date, lowG=vwret),
#            by = "date") %>% 
#   mutate(eigGrowth = highG - lowG) -> eigGrowth
# 
# inner_join(eg_3portMature %>% ungroup %>% filter(portfolio==3) %>% select(date, highM=vwret),
#            eg_3portMature %>% ungroup %>% filter(portfolio==1) %>% select(date, lowM=vwret),
#            by = "date") %>% 
#   mutate(eigMature = highM - lowM) -> eigMature
# 
# eigLC <- inner_join(eigGrowth, eigMature, by = "date") %>% 
#   mutate(eigDifference = eigGrowth - eigMature)
# 
# summary(eigLC$eigDifference*100)
# sd(eigLC$eigDifference*100)
# t.test(eigLC$eigDifference*100)


## Controling by Factor Models ------------------------------------------------
# Report H2b: Sentiment explain H2a
#

qmodel <- read.csv("0_data/q5_factors_monthly_2019a.csv", sep = ",") %>%
  as.data.table(qmodel)

qmodel$rn <- NULL
qmodel$R_F <- NULL
qmodel

ff <- read.csv("0_data/F-F_Research_Data_5_Factors_2x3.csv", sep = ",",skip = 3) %>% 
  as.data.table

ff <- ff %>%
  mutate(year  = as.numeric(substr(X, 1, 4))) %>% 
  mutate(month = as.numeric(substr(X, 5, 6))) %>% 
  select(year, month, everything()) %>% 
  select(-X)

qmodel

qmodelGrowth <- eigGrowth %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigGrowth) %>%
  left_join(qmodel) %>% select(-year, -month) %>% 
  as.data.table

qmodelMature <- eigMature %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigMature) %>%
  left_join(qmodel) %>% select(-year, -month) %>% 
  as.data.table

inner_join(eg_5portAll %>% ungroup %>% filter(portfolio==5) %>% select(date, high=vwret),
           eg_5portAll %>% ungroup %>% filter(portfolio==1) %>% select(date, low=vwret),
           by = "date") %>%
  mutate(eigAll = high - low) -> eigAll
qmodelAll <- eigAll %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigAll) %>%
  left_join(qmodel) %>% select(-year, -month) %>% 
  as.data.table

ffGrowth <- eigGrowth %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigGrowth) %>%
  left_join(ff) %>% select(-year, -month) %>% 
  as.data.table

ffMature <- eigMature %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigMature) %>%
  left_join(ff) %>% select(-year, -month) %>% 
  as.data.table

ffAll <- eigAll %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigAll) %>%
  left_join(ff) %>% select(-year, -month) %>% 
  as.data.table

m1 <- lm(Rp ~ ., data = qmodelGrowth)
m2 <- lm(Rp ~ ., data = qmodelMature)
m3 <- lm(Rp ~ ., data = qmodelAll)
m4 <- lm(Rp ~ ., data = ffGrowth)
m5 <- lm(Rp ~ ., data = ffMature)
m6 <- lm(Rp ~ ., data = ffAll)


robust_se1 <- robust.se(m1)
robust_se2 <- robust.se(m2)
robust_se3 <- robust.se(m3)
robust_se4 <- robust.se(m4)
robust_se5 <- robust.se(m5)
robust_se6 <- robust.se(m6)


stargazer(m1, m2, m3,
          title = "EIG across the life-cycle stages and q-factor models",
          se = list(robust_se1$t.stat,robust_se2$t.stat,robust_se3$t.stat),
          p = list(robust_se1$p.val, robust_se2$p.val,robust_se3$p.val),
          dep.var.labels.include = F,
          column.labels = c("Growth", "Mature", "All"),
          font.size="small",
          style="qje", omit.stat="f",
          df = FALSE, intercept.bottom = FALSE,
          type="latex", out = "3_output/results/3c_q_model.txt")

stargazer(m4, m5, m6,
          title = "EIG across the life-cycle stages and Fama-French 5 factor model",
          se = list(robust_se3$t.stat,robust_se4$t.stat,robust_se5$t.stat),
          p = list(robust_se3$p.val, robust_se4$p.val,robust_se5$p.val),
          dep.var.labels.include = F,
          column.labels = c("Growth", "Mature", "All"),
          font.size="small",
          style="qje", omit.stat="f",
          df = FALSE, intercept.bottom = FALSE,
          type="latex", out = "3_output/results/3c_ff_model.txt")


##  EIG and Investor Sentiment -------------------------------------------------
# Report H2b: Sentiment explain H2a
# 

sent <- read_xlsx("0_data/Investor_Sentiment_Data_20190327_POST.xlsx",
                  sheet = 2, range = cell_cols("A:B"), na = ".") %>% na.omit


names(sent)[2] <- "S"

sent <- sent %>%
  mutate(year  = as.numeric(substr(yearmo, 1, 4))) %>% 
  mutate(month = as.numeric(substr(yearmo, 5, 6))) %>% 
  select(year, month, S) %>% as.data.table

qmodelGrowth <- eigGrowth %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigGrowth) %>%
  left_join(qmodel) %>% 
  left_join(sent) %>% select(-year, -month)

qmodelMature <- eigMature %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigMature) %>%
  left_join(qmodel) %>% 
  left_join(sent) %>% select(-year, -month)

qmodelAll <- eigAll %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigAll) %>%
  left_join(qmodel) %>% 
  left_join(sent) %>% select(-year, -month)

ffGrowth <- eigGrowth %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigGrowth) %>%
  left_join(ff) %>% 
  left_join(sent) %>% select(-year, -month)

ffMature <- eigMature %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigMature) %>%
  left_join(ff) %>% 
  left_join(sent) %>% select(-year, -month)

ffAll <- eigAll %>%
  mutate(year  = year(date)) %>% 
  mutate(month = month(date)) %>% 
  select(year, month, Rp = eigAll) %>%
  left_join(ff) %>% 
  left_join(sent) %>% select(-year, -month)

m1 <- lm(Rp ~ ., data = qmodelGrowth)
m2 <- lm(Rp ~ ., data = qmodelMature)
m3 <- lm(Rp ~ ., data = qmodelAll)
m4 <- lm(Rp ~ ., data = ffGrowth)
m5 <- lm(Rp ~ ., data = ffMature)
m6 <- lm(Rp ~ ., data = ffAll)

robust_se1 <- robust.se(m1)
robust_se2 <- robust.se(m2)
robust_se3 <- robust.se(m3)
robust_se4 <- robust.se(m4)
robust_se5 <- robust.se(m5)
robust_se6 <- robust.se(m6)

stargazer(m1, m2, m3,
          title = "EIG across the life-cycle stages and Investor Sentiment",
          se = list(robust_se1$t.stat,robust_se2$t.stat,robust_se3$t.stat),
          p = list(robust_se1$p.val, robust_se2$p.val,robust_se3$p.val),
          dep.var.labels.include = F,
          column.labels = c("Growth", "Mature", "All"),
          font.size="small",
          style="qje", omit.stat="f",
          df = FALSE, intercept.bottom = FALSE,
          type="latex", out = "3_output/results/3c_isent_q_model.txt")

stargazer(m4, m5, m6,
          title = "EIG across the life-cycle stages and Investor Sentiment 2",
          se = list(robust_se3$t.stat,robust_se4$t.stat,robust_se5$t.stat),
          p = list(robust_se3$p.val, robust_se4$p.val,robust_se5$p.val),
          dep.var.labels.include = F,
          column.labels = c("Growth", "Mature", "All"),
          font.size="small",
          style="qje", omit.stat="f",
          df = FALSE, intercept.bottom = FALSE,
          type="latex", out = "3_output/results/3c_isent_ff_model.txt")

## Test c/ quintis
# lc   <- readRDS("~/Data/WRDS/raw_comp_life_cycle.rds")
# HMXZ <- readRDS("~/Data/eig/hmxz.rds")
# 
# ## Merge Life cycle Proxy and EIG (HMXZ)
# lc <- lc %>% mutate(date = datadate) %>% select(gvkey, date, LC) %>% as_tibble
# eg_m <- HMXZ %>% select(permno, gvkey, date, eg=EIG) %>% na.omit %>% as_tibble
# eg_m <- eg_m %>% left_join(lc, by = c("gvkey", "date"))
# 
# ## Pode filtrar para analisar individualmente
# # eg_m <- eg_m %>% filter(LC == 1) # Introduction and Growth
# # eg_m <- eg_m %>% filter(LC <= 2) # Introduction and Growth
# # eg_m <- eg_m %>% filter(LC == 2) # Only growths
# # eg_m <- eg_m %>% filter(LC == 3) # Mature
# # eg_m <- eg_m %>% filter(LC == 4) # Shake-Out
# # eg_m <- eg_m %>% filter(LC >= 3) # no_growths
# 
# # eg_m <- eg_m %>% filter(LC <= 2) # Introduction and Growth
# # ## retorno
# # ## 1  -0.447
# # ## 2  -0.240
# # ## 3  -0.479
# # ## 4   0.831
# # ## 5   0.511 ## Relacao quase inversa
# 
# # eg_m <- eg_m %>% filter(LC == 3) # Mature
# # ## retorno
# # ## 1   0.511
# # ## 2   1.054
# # ## 3   0.883
# # ## 4   0.657
# # ## 5   0.535 ## Relacao quase inversa5   0.535 ## Relacao quase inversa
# 
# DT <- as.data.table(eg_m)
# DT[                      eg<=quantile(eg,0.2), portfolio := 1, by=date]
# DT[eg>quantile(eg,0.2) & eg<=quantile(eg,0.4), portfolio := 2, by=date]
# DT[eg>quantile(eg,0.4) & eg<=quantile(eg,0.6), portfolio := 3, by=date]
# DT[eg>quantile(eg,0.6) & eg<=quantile(eg,0.8), portfolio := 4, by=date]
# DT[eg>quantile(eg,0.8),                        portfolio := 5, by=date]
# as_tibble(DT) %>% group_by(portfolio, date) %>%
#   summarise(ewret = mean(retadj),
#             vwret = weighted.mean(retadj, wt)) %>%
#   arrange(date) -> eg_5portAll
# eg_5portAll %>% group_by(portfolio) %>% summarise(retorno = round(mean(vwret)*100,3)) %>% select(retorno) %>% data.frame