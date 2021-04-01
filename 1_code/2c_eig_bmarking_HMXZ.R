## >> COMPUTE EIG FAMA-MECBATH APPROACH ## ####################################
library(data.table)
library(tidyverse)
source('1_code/functions.R')
computeAverageSlopes <- function(x, rollingWindow=180, minimum=30) {
  frollmean(x, c(rep(NA,minimum-1),
                 minimum:rollingWindow,
                 rep(rollingWindow,length(x)-rollingWindow)
  ), adaptive=TRUE, na.rm = T)
}

## Cross-sectional forecasting ## #############################################
# We winsorize all variables at the 1st and 99th percentiles of their
# distributions each month.

# Winsorize all variables at the 1st and 99th percentiles of their distributions
# each month.
crsp4 <- readRDS("2_pipeline/2_out/1b_crsp4.rds")

DT <- crsp4 %>% select(-rdq) %>% filter(date>"1969-12-31") %>% as.data.table
DT[, d1_ia  := winsorizar(d1_ia),  by = c("date")]
DT[, cop    := winsorizar(cop),    by = c("date")]
DT[, q      := winsorizar(q),      by = c("date")]
DT[, dROE   := winsorizar(dROE),   by = c("date")]
DT <- DT[complete.cases(d1_ia, cop, q, dROE)]
crsp5 <- DT %>% as_tibble %>% select(permno:date, fiscaldate, d1_ia, cop, q, dROE, me)
rm(DT)

# saveRDS(crsp5, "~/Data/EIG/igmodel_winsorized.rds")

# crsp5 %>% select(d1_ia, q, cop, dROE) %>% drop_na
# summary(lm(d1_ia ~ q + cop + dROE, data = crsp5, weights = me))
# summary(lm(d1_ia ~ q, data = crsp5, weights = me))
# summary(lm(d1_ia ~ cop, data = crsp5, weights = me))
# summary(lm(d1_ia ~ dROE, data = crsp5, weights = me))

# x <- summary(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))
# x$coefficients["q","t value"]
# summary(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))

# Ver
# http://arelbundock.com/blog/2020-08-17-nesting-datatable.html

setDT(crsp5)

crsp5 %>% group_by(gvkey) %>% 
  mutate(d1_ia   = dplyr::lag(d1_ia, 12)) %>% 
  mutate(q       = dplyr::lag(q, 12)) %>% 
  mutate(cop     = dplyr::lag(cop, 12)) %>% 
  mutate(dROE    = dplyr::lag(dROE, 12)) %>% 
  mutate(me      = dplyr::lag(me,12)) %>% na.omit -> crsp6

setDT(crsp6)

bHMXZ <- crsp5[,list(
  intercept = round(coef(lm(d1_ia ~ q + cop + dROE, weights = me))[1],3),
  q    = round(coef(lm(d1_ia ~      q + cop + dROE, weights = me))["q"],3),
  cop  = round(coef(lm(d1_ia ~      q + cop + dROE, weights = me))["cop"],3),
  dROE = round(coef(lm(d1_ia ~      q + cop + dROE, weights = me))["dROE"],3),
  R2   = round(summary(lm(d1_ia ~   q + cop + dROE, weights = me))$r.squared,3)
), by=date][order(date)]

# Start date to correct the error above
# Error in summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["q", : subscript out of bounds
start_date <- (crsp5 %>% group_by(date) %>% count %>% filter(n>1) %>% as.data.frame)[1,1]

tHMXZ <- crsp5[date>=start_date,.(
  intercept= round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients[1,"t value"],3),
  q        = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["q","t value"],3),
  cop      = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["cop","t value"],3),
  dROE     = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["dROE","t value"],3)
), by=date][order(date)]

# tHMXZ <- crsp5[,.(
#   intercept=round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["(Intercept)",   "t value"],3),
#   q    = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["q",   "t value"],3),
#   cop  = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["cop", "t value"],3),
#   dROE = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE","t value"],3)
# ), by=date][order(date)]

result_monthly_cs_reg <- rbind(round(bHMXZ[, .(q, cop, dROE)][, lapply(.SD, mean, na.rm=TRUE)],3),
                               round(tHMXZ[, .(q, cop, dROE)][, lapply(.SD, mean, na.rm=TRUE)],3))
result_monthly_cs_reg <- as.data.frame(result_monthly_cs_reg)
rownames(result_monthly_cs_reg) <- c("Slopes", "t-stat")
result_monthly_cs_reg <- cbind(result_monthly_cs_reg, R2=c(round(mean(bHMXZ$R2),3),""))
result_monthly_cs_reg

bHMXZ[, b0    := computeAverageSlopes(intercept)]
bHMXZ[, bQ    := computeAverageSlopes(q)]
bHMXZ[, bCOP  := computeAverageSlopes(cop)]
bHMXZ[, bDROE := computeAverageSlopes(dROE)]
# bHMXZ
# bHMXZ %>% data.frame %>% head(30)
bHMXZ <- na.omit(bHMXZ)

## Prever EIG out-of-sample ---------------------------------------------------
# EIGt+1 = b0+ bq,t*qt + bCop,t x Copt
# + bMOM,t x MOMt
HMXZ <- crsp5 %>%
  inner_join(bHMXZ[,.(date, b0, bQ, bCOP, bDROE)], by = "date") %>%
  setDT
HMXZ[, EIG := b0 + bQ * q + bCOP * cop + bDROE * dROE]
# HMXZ[, EIG := bQ * q + bCOP * cop + bDROE * dROE]

summary(HMXZ)
# HMXZ <- HMXZ %>% filter(date>="1972-06-30")

HMXZ %>% group_by(date) %>%
  summarise(correl = cor(d1_ia, EIG)) %>% 
  summarise(mean(correl))

HMXZ[,deciles := cut(EIG, quantile(EIG, probs = 0:10/10), labels = FALSE, include.lowest = TRUE), by=date]
HMXZ %>% group_by(deciles) %>% summarise(EIG  = mean(EIG,   ), d1_ia = mean(d1_ia), .groups = 'drop')

corPearson <- cor.test(HMXZ$d1_ia, HMXZ$EIG, method ="pearson")
Sys.setenv(LANG = "en")
corSpearman <- cor.test(HMXZ$d1_ia, HMXZ$EIG, method ="spearman") # Rank
result_monthly_cs_reg <- cbind(result_monthly_cs_reg,
                               Pearson=round(c(corPearson$estimate, corPearson$p.value),3),
                               Rank=round(c(corSpearman$estimate, corSpearman$p.value),3))
result_monthly_cs_reg

saveRDS(na.omit(HMXZ), file = "2_pipeline/2_out/2c_hmxz.rds")

# # TUDO COMENTADO DAQUI PRA BAIXO
# # ## Criar data.table com as estatisticas t corrigidas
# # DT <- as.data.table(crsp5)
# # # DT <- crsp5 %>% mutate(year = year(date)) %>% filter(year>1969) %>% as.data.table
# # tHMXZ <- DT[,list(
# #   # intercept=coef(lm(IGC ~ Q + CF + MOM))[1],
# #   q    = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["q",   "t value"],3),
# #   cop  = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["cop", "t value"],3),
# #   dROE = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE","t value"],3)
# # ), by=date][order(date)]
# 
# 
# ## Estatistica t corrigida por heterodasticidade e autocorrelacao --------------
# 
# ## Teste
# # x <- lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me)
# # coeftest(x, vcov=NeweyWest(x, lag = 4, prewhite = FALSE))
# 
# ## Funcao para rodar dentro do data.table
# # NW <- function (x, Lag=4) {
# #   require(lmtest)
# #   require(sandwich)
# #   coeftest(x, vcov=NeweyWest(x, lag = Lag, prewhite = FALSE))
# # }
# # coef(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["q"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["q","Estimate"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["q","t value"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["cop","t value"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["dROE","t value"]
# 
# ## Criar data.table com as estatisticas t corrigidas
# DT <- crsp5 %>%
#   mutate(year = year(date)) %>% 
#   filter(year>1969) %>% as.data.table
# tHMXZ <- DT[,list(
#   # intercept=coef(lm(IGC ~ Q + CF + MOM))[1],
#   q    = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["q",   "t value"],3),
#   cop  = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["cop", "t value"],3),
#   dROE = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE","t value"],3)
# ), by=date][order(date)]
# 
# tHMXZ
# 




# TODO Verificar ==> Error in NW (Cod: 152511)
# # # Error in NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE", "t value"] : 
# # #   ?ndice fora de limites
# # crsp5 %>%
# #   mutate(year = year(date)) %>% 
# #   filter(me>0 & year<1969) %>% arrange(year) #%>% summarise(q = mean(q), dROE = mean(dROE))
# 
# 
# #      q  cop   dROE   R2
# # -0.029 0.516 0.771  0.06
# round(bHMXZ[, .(q, cop, dROE, R2)][, lapply(.SD, mean, na.rm=TRUE)],3)
# round(tHMXZ[, .(q, cop, dROE)][, lapply(.SD, mean, na.rm=TRUE)],3)
# 
# ## Calcular coeficientes m?dios dos ?ltimos 180 meses (m?nimo 30 meses) -------
# # b0, bq, bCF e bMOM
# 
# # bHMXZ[, bq    := .(Reduce(`*`, shift(q, 1:180)))]
# # bHMXZ[, bcop  := .(Reduce(`*`, shift(cop, 1:180)))]
# # bHMXZ[, bdROE := .(Reduce(`*`, shift(dROE, 1:180)))]
# 
# bHMXZ[, bQ    := computeAverageSlopes(q)]
# bHMXZ[, bCOP  := computeAverageSlopes(cop)]
# # o dROE nao tem as primeiras observacores, por isso coloquei +40 para iniciar
# # com o minimo de 30
# bHMXZ[, bDROE := computeAverageSlopes(dROE, 180, 30+44)]
# bHMXZ %>% data.frame %>% head(182)
# 
# ## Prever EIG out-of-sample ---------------------------------------------------
# # EIGt+1 = b0+ bq,t*qt + bCop,t x Copt
# # + bMOM,t x MOMt
# HMXZ <- crsp5 %>%
#   left_join(bHMXZ[,.(date, bQ, bCOP, bDROE)], by = "date") %>%
#   setDT
# HMXZ[, EIG := bQ * q + bCOP * cop + bDROE * dROE]
# 
# saveRDS(na.omit(HMXZ), file = "~/Data/eig/hmxz.rds")
# 
# HMXZ <- readRDS("~/Data/eig/hmxz.rds")
# eg_m <- HMXZ %>% select(gvkey, date, eg=EIG) %>% na.omit
# arrow::write_feather(eg_m, "~/Data/EIG/eg_m.feather")
# 
# # HMXZ
# # 1) CS (WLS?)
# # 2) TS average slope 120 month roling window (30 minimum) to compute TS average slope
# # 3) CS out-of sample WLS
# 
# # LWY (2020)
# # 1) Estima coeficientes em annual CS (each june)
# # 2) All historical TS average slope up to year t combine with most recent predictors
# # 3) out-of-sample prediction (last predictors+average slopes)
# 
# rm(ccm_a, ccm_q, ccmlink, crsp, crsp2, crsp3, DT, x)
# 
# ## Cross-sectional forecasting ## #############################################
# # We winsorize all variables at the 1st and 99th percentiles of their
# # distributions each month.
# 
# # Winsorize all variables at the 1st and 99th percentiles of their distributions
# # each month. 
# 
# DT <- as.data.table(select(crsp4,-rdq))
# DT[, d1_ia  := winsorizar(d1_ia),  by = c("date")]
# DT[, cop  := winsorizar(cop),  by = c("date")]
# DT[, q    := winsorizar(q),    by = c("date")]
# DT[, dROE := winsorizar(dROE), by = c("date")]
# crsp5 <- DT %>% na.omit %>% as_tibble %>% 
#   select(permno:date, fiscaldate, d1_ia, cop, q, dROE, me)
# rm(DT)
# 
# # crsp5 %>% select(d1_ia, q, cop, dROE) %>% drop_na
# # summary(lm(d1_ia ~ q + cop + dROE, data = crsp5, weights = me))
# # summary(lm(d1_ia ~ q, data = crsp5, weights = me))
# # summary(lm(d1_ia ~ cop, data = crsp5, weights = me))
# # summary(lm(d1_ia ~ dROE, data = crsp5, weights = me))
# 
# # x <- summary(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))
# # x$coefficients["q","t value"]
# # summary(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))
# 
# # Ver
# # http://arelbundock.com/blog/2020-08-17-nesting-datatable.html
# 
# DT <- as.data.table(crsp5)
# bHMXZ <- DT[,list(
#   # intercept=coef(lm(IGC ~ Q + CF + MOM))[1],
#   q    = round(coef(lm(d1_ia ~ q + cop + dROE, weights = me))["q"],3),
#   cop  = round(coef(lm(d1_ia ~ q + cop + dROE, weights = me))["cop"],3),
#   dROE = round(coef(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE"],3),
#   R2   = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$r.squared,3)
# ), by=date][order(date)]
# 
# 
# 
# ## Estatistica t --------------
# # tHMXZ <- as.data.table(crsp5)[,.(
# #   # intercept=coef(lm(IGC ~ Q + CF + MOM))[1],
# #   q    = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["q","t value"],3),
# #   cop  = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["cop","t value"],3)#,
# #   #dROE = round(summary(lm(d1_ia ~ q + cop + dROE, weights = me))$coefficients["dROE","t value"],3)
# # ), by=date][order(date)]
# # round(tHMXZ[, .(q, cop)][, lapply(.SD, mean, na.rm=TRUE)],3)
# 
# ## Estatistica t corrigida por heterodasticidade e autocorrelacao --------------
# 
# ## Teste
# # x <- lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me)
# # coeftest(x, vcov=NeweyWest(x, lag = 4, prewhite = FALSE))
# 
# ## Funcao para rodar dentro do data.table
# # NW <- function (x, Lag=4) {
# #   require(lmtest)
# #   require(sandwich)
# #   coeftest(x, vcov=NeweyWest(x, lag = Lag, prewhite = FALSE))
# # }
# # coef(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["q"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["q","Estimate"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["q","t value"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["cop","t value"]
# # NW(lm(d1_ia ~ q + cop + dROE, data = head(crsp5,200), weights = me))["dROE","t value"]
# 
# ## Criar data.table com as estatisticas t corrigidas
# DT <- crsp5 %>%
#   mutate(year = year(date)) %>% 
#   filter(year>1969) %>% as.data.table
# tHMXZ <- DT[,list(
#   # intercept=coef(lm(IGC ~ Q + CF + MOM))[1],
#   q    = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["q",   "t value"],3),
#   cop  = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["cop", "t value"],3),
#   dROE = round(NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE","t value"],3)
# ), by=date][order(date)]
# #### Verificar ==> Error in NW ####
# # # Error in NW(lm(d1_ia ~ q + cop + dROE, weights = me))["dROE", "t value"] : 
# # #   ?ndice fora de limites
# # crsp5 %>%
# #   mutate(year = year(date)) %>% 
# #   filter(me>0 & year<1969) %>% arrange(year) #%>% summarise(q = mean(q), dROE = mean(dROE))
# 
# 
# #      q  cop   dROE   R2
# # -0.029 0.516 0.771  0.06
# round(bHMXZ[, .(q, cop, dROE, R2)][, lapply(.SD, mean, na.rm=TRUE)],3)
# round(tHMXZ[, .(q, cop, dROE)][, lapply(.SD, mean, na.rm=TRUE)],3)
# 
# ## Calcular coeficientes m?dios dos ?ltimos 180 meses (m?nimo 30 meses) -------
# # b0, bq, bCF e bMOM
# 
# # bHMXZ[, bq    := .(Reduce(`*`, shift(q, 1:180)))]
# # bHMXZ[, bcop  := .(Reduce(`*`, shift(cop, 1:180)))]
# # bHMXZ[, bdROE := .(Reduce(`*`, shift(dROE, 1:180)))]
# 
# 
# 
# 
# 
# bHMXZ[, bQ    := computeAverageSlopes(q)]
# bHMXZ[, bCOP  := computeAverageSlopes(cop)]
# # o dROE nao tem as primeiras observacores, por isso coloquei +40 para iniciar
# # com o minimo de 30
# bHMXZ[, bDROE := computeAverageSlopes(dROE, 180, 30+44)]
# # bHMXZ %>% data.frame %>% head(182)
# 
# ## Prever EIG out-of-sample ---------------------------------------------------
# # EIGt+1 = b0+ bq,t*qt + bCop,t x Copt
# # + bMOM,t x MOMt
# HMXZ <- crsp5 %>%
#   left_join(bHMXZ[,.(date, bQ, bCOP, bDROE)], by = "date") %>%
#   setDT
# HMXZ[, EIG := bQ * q + bCOP * cop + bDROE * dROE]
# 
# saveRDS(na.omit(HMXZ), file = "~/Data/eig/hmxz.rds")
# 
# HMXZ <- readRDS("WRDS\\hmxz.rds")
# eg_m <- HMXZ %>% select(gvkey, date, eg=EIG) %>% na.omit
# arrow::write_feather(eg_m, "WRDS\\eg_m.feather")
# 
# # HMXZ
# # 1) CS (WLS?)
# # 2) TS average slope 120 month roling window (30 minimum) to compute TS average slope
# # 3) CS out-of sample WLS
# 
# # LWY (2020)
# # 1) Estima coeficientes em annual CS (each june)
# # 2) All historical TS average slope up to year t combine with most recent predictors
# # 3) out-of-sample prediction (last predictors+average slopes)
# 
# rm(ccm_a, ccm_q, ccmlink, crsp, crsp2, crsp3, DT, x)
# 
# ## @@@@ Estimar EIG de LWY ---------------------------------------------------------
# ##### EIG LWY - Variables
# ## - MOM 12 months cumulative return
# ## - Cash Flow
# ##      Variables: ( Depreciation(DP)
# ##                  + Income Before Extraordinary Itens (IB) ) / AT
# ## - Tobin's Q
# ##      Variable: Market Equity + Long Term Debt + Short Term Debt
# ##
# ##     Tobin's q is the market equity (from CRSP) plus long-term debt (Compustat
# ##     annual item DLTT) and short-term debt (item DLC) scaled by book assets (item
# ##     AT)
# ##
# ## - IGt = (CAPXt - CAPXt-1) / CAPXt_1
# ## 
# ## All winsorized
# ##
# ##### EIG LWY Estimation
# ## (1) Compute CS by year each june
# ## (2) Compute avarege (TS) coefficients
# ## (3) Compute EIG as out-of-sample predicted value, using historical avarege
# ## slopes times last variable of predictors month, cf e q