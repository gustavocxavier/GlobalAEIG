## Results Chapter 3 -----------------------------------------------------------

setwd("C:/Dropbox/Code/investmentPlan/GitHub/GlobalAEIG")
## Load packages ---------------------------------------------------------------
library(dplyr)
library(data.table)
library(broom)

## Load data -------------------------------------------------------------------
ws_stock <- readRDS("0_data/trws/raw_ws_stock.rds")

## Organize monthly data -------------------------------------------------------
db_m <- melt(ws_stock, id.vars=c("ticker", "year"),
             measure.vars=patterns("m"),
             variable.name="month", value.name="P")
db_m[, month := as.numeric(substr(month, 2, 3))]
db_m <- db_m[order(ticker, year, month)]
db_m[P==0, P := NA]
db_m <- na.omit(db_m)
db_m[, R := log(P/shift(P)), by = ticker]
db_m[ticker=="PETR4"]

## Organize ws_funda -----------------------------------------------------------
## See TRWS.R to know how to get this data
ws_funda <- readRDS("0_data/trws/raw_ws_funda.rds")

ws_funda <- ws_funda %>%
  select(-cusip, -isin, ws_id=item6105, -ibes_ticker, -type) %>%
  na.omit

# ws_funda %>% group_by(nation) %>%
#   count %>% filter(n>400) %>% arrange(-n) %>%
#   data.frame
# TODO: Checar dados TRWS x Economatica
summary(ws_funda$cfo)
ws_funda[, cfi := -cfi]
ws_funda[ticker=="PETR4"]

## Life Cycle - DCS ------------------------------------------------------------
# Dickinson (2011) classification scheme (DCS) - 8 groups (DCS8g)
ws_funda <- ws_funda %>%
  mutate(DCS8g = case_when((cfo <= 0 & cfi <= 0 & cff >  0) ~ 1,
                           (cfo >  0 & cfi <= 0 & cff >  0) ~ 2,
                           (cfo >  0 & cfi <= 0 & cff <= 0) ~ 3,
                           (cfo <= 0 & cfi <= 0 & cff <= 0) ~ 4,
                           (cfo >  0 & cfi >  0 & cff >  0) ~ 5,
                           (cfo >  0 & cfi >  0 & cff <= 0) ~ 6,
                           (cfo <= 0 & cfi >  0 & cff >  0) ~ 7,
                           (cfo <= 0 & cfi >  0 & cff <= 0) ~ 8,
                           TRUE ~ 0)) # %>%
# mutate(DCS8g_name = case_when((oancf <= 0 & ivncf <= 0 & fincf >  0) ~ "1 Introduction",
#                               (oancf >  0 & ivncf <= 0 & fincf >  0) ~ "2       Growth",
#                               (oancf >  0 & ivncf <= 0 & fincf <= 0) ~ "3       Mature",
#                               (oancf <= 0 & ivncf <= 0 & fincf <= 0) ~ "4    Shake-Out",
#                               (oancf >  0 & ivncf >  0 & fincf >  0) ~ "5    Shake-Out",
#                               (oancf >  0 & ivncf >  0 & fincf <= 0) ~ "6    Shake-Out",
#                               (oancf <= 0 & ivncf >  0 & fincf >  0) ~ "7      Decline",
#                               (oancf <= 0 & ivncf >  0 & fincf <= 0) ~ "8      Decline",
#                               TRUE ~ "        Failed")) %>%
# mutate(DCS8g_name = as.factor(DCS8g_name))

# Dickinson (2011) classification scheme (DCS) - 4 groups
ws_funda <- ws_funda %>%
  mutate(DCS = case_when(DCS8g <= 3 ~ DCS8g,
                         DCS8g >= 4 ~ 4)) %>%
  mutate(lcycle = case_when((DCS == 1) ~ "Introduction",
                            (DCS == 2) ~ "Growth",
                            (DCS == 3) ~ "Mature",
                            (DCS == 4) ~ "Shake-Out/Decline")) %>%
  mutate(lcycle = as.factor(lcycle))

## Investment Growth -----------------------------------------------------------
ws_funda <- ws_funda %>% arrange(nation, ticker, date) %>%
  group_by(ws_id) %>%
  mutate(IG = log(capex / dplyr::lag(capex))) %>% ungroup %>%
  filter(is.finite(IG)) %>% as.data.table
# ws_funda

## Momentum --------------------------------------------------------------------

# tmp <- db_m %>% group_by(ticker, year) %>% summarise(MOM=sum(return, na.rm = T))
# tmp <- tmp %>% mutate(year = year+1)
# db_m <- db_m %>% left_join(tmp)
# tmp
db_m[, MOM := Reduce(`+`, shift(R, 0:12))]

## Expected Investment Growth Monthly ------------------------------------------
ws_funda[, year := year(date)]

## Q de Tobin
ws_funda <- ws_funda %>%
  left_join(select(ws_stock, year, ws_id, ev))

ws_funda[, log_Q := log(ev/at)]
ws_funda <- ws_funda[is.finite(log_Q),]
ws_funda <- ws_funda[order(ticker, date)]
ws_funda[, IGt1 := shift(IG, n=1L, type="lead"), by=c("ws_id")]
ws_funda

tmp <- ws_funda %>% select(nation, year, ticker, ws_id, IGt1, IG, cfo, log_Q) %>% na.omit
eig_m <- db_m %>%
  left_join(tmp, by = c("year", "ticker")) %>% na.omit %>%
  select(nation, year, month, ticker, ws_id, IGt1, IG, everything()) %>%
  select(-P, -R) %>%
  left_join(select(ws_stock, year, ws_id, ev))
eig_m

## Fama-MacBeth

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

eig_m[, IGt1  := winsorize(IGt1),  by = c("nation", "year", "month")]
eig_m[, IG    := winsorize(IG),    by = c("nation", "year", "month")]
eig_m[, MOM   := winsorize(MOM),   by = c("nation", "year", "month")]
eig_m[, cfo   := winsorize(cfo),   by = c("nation", "year", "month")]
eig_m[, log_Q := winsorize(log_Q), by = c("nation", "year", "month")]
eig_m_bkp <- eig_m
eig_m <- eig_m[complete.cases(IGt1, IG, MOM, cfo, log_Q, ev)]
eig_m <- eig_m[IGt1 != IG & IGt1 != 0]
eig_m <- eig_m[is.finite(IGt1)]
eig_m <- eig_m[is.finite(IG)]
eig_m <- eig_m[is.finite(MOM)]
eig_m <- eig_m[is.finite(cfo)]
eig_m <- eig_m[is.finite(log_Q)]
eig_m <- eig_m[is.finite(ev)]

nations <- ws_funda %>%
  filter(nation != "UNITED STATES") %>%
  group_by(nation) %>%
  count %>% filter(n>1000) %>% arrange(-n) %>%
  data.frame
nations <- sort(unique(nations$nation))

bEIG <- eig_m[nation %in% nations,list(
  intercept = round(coef(   lm(IGt1 ~ IG + MOM + cfo + log_Q, weights = ev))[1],3),
  IG        = round(coef(   lm(IGt1 ~ IG + MOM + cfo + log_Q, weights = ev))["IG"],3),
  MOM       = round(coef(   lm(IGt1 ~ IG + MOM + cfo + log_Q, weights = ev))["MOM"],3),
  cfo       = round(coef(   lm(IGt1 ~ IG + MOM + cfo + log_Q, weights = ev))["cfo"],3),
  log_Q     = round(coef(   lm(IGt1 ~ IG + MOM + cfo + log_Q, weights = ev))["log_Q"],3)
), by=c("nation", "year", "month")][order(nation, year, month)]

computeAverageSlopes <- function(x, rollingWindow=52, minimum=30) {
  frollmean(x, c(rep(NA,minimum-1),
                 minimum:rollingWindow,
                 rep(rollingWindow,length(x)-rollingWindow)
  ), adaptive=TRUE, na.rm = T)
}
bEIG[, b0     := computeAverageSlopes(intercept), by=nation]
bEIG[, bIG    := computeAverageSlopes(IG)       , by=nation]
bEIG[, bMOM   := computeAverageSlopes(MOM)      , by=nation]
bEIG[, bcfo   := computeAverageSlopes(cfo)      , by=nation]
bEIG[, blog_Q := computeAverageSlopes(log_Q)    , by=nation]
# bEIG
# bEIG %>% data.frame %>% head(30)
bEIG <- na.omit(bEIG)

eig2 <- eig_m %>%
  inner_join(bEIG[,.(nation, year, month, b0, bIG, bMOM, bcfo, blog_Q)], by = c("nation", "year", "month")) %>%
  setDT
eig2[, EIG := b0 + bIG * IG + bMOM * MOM + bcfo * cfo + blog_Q *log_Q]
## Portfolio Analysis ----------------------------------------------------------

## AEIG All Stocks -------------------------------------------------------------
as_tibble(eig2) %>% group_by(nation, year, month) %>%
  filter(is.finite(EIG)) %>%
  summarise(AEIG = weighted.mean(EIG, ev)) %>%
  arrange(nation, year, month) %>% as.data.table -> AEIG
# AEIG %>% filter(nation=="BRAZIL") %>% data.frame

## AEIG conditioned to life cycle ----------------------------------------------
as_tibble(eig2) %>%
  left_join(ws_funda %>% select(year, ws_id, lcycle=DCS)) %>%
  group_by(lcycle, nation, year, month) %>%
  filter(is.finite(EIG)) %>%
  summarise(AEIG = weighted.mean(EIG, ev)) %>%
  arrange(nation, year, month) %>% as.data.table -> AEIGlc
AEIGlc %>% group_by(lcycle) %>% count

## Wide market return ----------------------------------------------------------
mkt <- db_m %>%
  select(year, month, ticker, R) %>%
  left_join(select(ws_funda, year, ticker, nation, mv)) %>%
  select(nation, year, month, everything()) %>% na.omit %>%
  arrange(nation, year, month)
mkt

mkt %>% filter(nation=="BRAZIL")
mkt %>% filter(nation == "UNITED KINGDOM")

as_tibble(mkt) %>% group_by(nation, year, month) %>%
  summarise(ewret = mean(R),
            vwret = weighted.mean(R, mv)) %>% ungroup %>%
  arrange(nation, year, month) %>% as.data.table -> mkt

## Load world bank Data --------------------------------------------------------
library(WDI)
new_wdi_cache <- WDIcache()

wb_countries <- WDI(country = 'all', start = 2010, end = 2010) %>%
  mutate(nation = toupper(country)) %>%
  select(iso2c, nation) %>%
  filter(nation %in% nations) %>%
  as_tibble

wb_data = WDI(start=1994, end=2019,
              # country='BR',
              # country='all',
              country=wb_countries$iso2c,
              indicator= c(
                ## Control Variables
                interest_rate = 'FR.INR.DPST',              # Deposit interest rate (%)
                inflation = 'FP.CPI.TOTL.ZG',               # Inflation, consumer prices (annual %)
                risk_premium = 'FR.INR.RISK',               # Risk premium on lending (lending rate minus treasury bill rate, %)
                # risk_premium = 'FR.INR.RISK',               # Risk premium on lending (lending rate minus treasury bill rate, %)

                ## Market Development Measures
                stockMktCapGDP = 'GFDD.DM.01',              # Stock market capitalization to GDP (%)
                stockMktCapGDP_2 = 'CM.MKT.LCAP.GD.ZS',     # Market capitalization of listed domestic companies (% of GDP)
                listedDomesticCompanies = 'CM.MKT.LDOM.NO', # Listed domestic companies, total
                volTradedGDP = 'CM.MKT.TRAD.GD.ZS',         # Stocks traded, total value (% of GDP)
                stockTradedGDP = 'GFDD.DM.02',              # Stock market total value traded to GDP (%)
                legalRights = 'IC.LGL.CRED.XQ'              # Strength of legal rights index (0=weak to 12=strong)
              )) %>%
  as_tibble

wb_data$risk_premium <- NULL # Too many NA

saveRDS(wb_data, file = "2_pipeline/2_out/wb_data.rds")

wb_data <- readRDS("2_pipeline/2_out/wb_data.rds")

## Load OECD data --------------------------------------------------------------

## Get Interest Rate
oecd_interest <- read.csv("0_data/oecd/oecd_interest_rate_data.csv")
# https://stats.oecd.org/viewhtml.aspx?datasetcode=MEI_FIN&lang=en#
oecd_interest <- as_tibble(oecd_interest)
filter_interest <- unique(oecd_interest$Subject)[4]
colnames(oecd_interest)[1] <- "SUBJECT"
oecd_interest <- oecd_interest %>%
  filter(SUBJECT == "IRSTCI") %>%
  select(LOCATION, Country, Time, Value)

## Get Inflation (CPI)
oecd_cpi <- read.csv("0_data/oecd/oecd_prices_cpi_data.csv")
# https://stats.oecd.org/viewhtml.aspx?datasetcode=PRICES_CPI&lang=en#
oecd_cpi <- as_tibble(oecd_cpi)
colnames(oecd_cpi)[1] <- "LOCATION"
# oecd_cpi %>% filter(stringr::str_detect(Subject, "CPI.*All items")) %>% View
oecd_cpi <- oecd_cpi %>%
  filter(SUBJECT == "CPALTT01") %>%
  filter(MEASURE == "GY") %>% # IXOB for index / GY for Percentage change
  select(LOCATION, Country, Time, Value)

oecd_cpi      # Inflation
oecd_interest # Interest Rate

## Solution for Missing Values in World Bank Data ------------------------------

## Merge stockMktCapGDP with stockMktCapGDP_2 (Correlation = 0.9733146)
wb_data %>% select(stockMktCapGDP, stockMktCapGDP_2) %>% na.omit %>% cor
setDT(wb_data)
wb_data[is.na(stockMktCapGDP) & !is.na(stockMktCapGDP_2), stockMktCapGDP := stockMktCapGDP_2]
wb_data <- wb_data %>% select(-stockMktCapGDP_2) %>% as_tibble

## Merge volTradedGDP with stockTradedGDP (Correlation = 0.9499892)
wb_data %>% select(volTradedGDP, stockTradedGDP) %>% na.omit %>% cor
setDT(wb_data)
wb_data[is.na(volTradedGDP) & !is.na(stockTradedGDP), volTradedGDP := stockTradedGDP]
wb_data <- wb_data %>% select(-stockTradedGDP) %>% as_tibble

wb_data %>% select(volTradedGDP, stockTradedGDP) %>% na.omit %>% cor

## Check all NA
wb_data %>% group_by(country) %>%
  summarise(
    na_interest_rate    = sum(is.na(interest_rate   )),
    na_inflation        = sum(is.na(inflation       )),
    na_stockMktCapGDP   = sum(is.na(stockMktCapGDP  )),
    na_volTradedGDP     = sum(is.na(volTradedGDP    )),
    na_legalRights      = sum(is.na(legalRights     ))
  ) %>% as.data.frame

wb_data %>% filter(country=="Vietnam") %>% as.data.frame
wb_data %>% filter(country=="Israel") %>% as.data.frame
wb_data %>% filter(country=="Denmark") %>% as.data.frame
wb_data %>% filter(country=="Finland") %>% as.data.frame
wb_data %>% filter(country=="Sweden") %>% as.data.frame
wb_data %>% filter(country=="Brazil") %>% as.data.frame

## Merge with OECD data for high-income countries
# Try isreal here



## Replace legalRights NA for the median of the country
wb_data <- wb_data %>% group_by(country) %>%
  mutate(
    legalRights = case_when(
      is.na(legalRights) ~ median(legalRights, na.rm = T),
      TRUE ~ legalRights
    )
  ) %>%
  mutate(legalRights = round(legalRights))
attr(wb_data[["legalRights"]], "label") = "Strength of legal rights index (0=weak to 12=strong)"

## Drop Total Number of listed domestic companies variable
wb_data$listedDomesticCompanies <- NULL

## Fill the rest using "downup" option of the tidyr fill function (i.e. first
## down and then up)


# fill(volTradedGDP, .direction = "downup")
#
# wb %>% filter(iso2c=='VN') %>% as.data.frame %>%
#   fill(stockMktCapGDP, .direction = "downup") %>%

## In sample prediction --------------------------------------------------------
lmdata <- mkt %>% left_join(AEIG) %>%
  group_by(nation) %>%
  mutate(AEIG = dplyr::lag(AEIG)) %>%
  select(year, month, AEIG, MKT=vwret) %>% na.omit
setDT(lmdata)
lmdata[, MKT3M := Reduce(`+`, shift(MKT, -( 0:2))), by=nation]
lmdata[, MKT6M := Reduce(`+`, shift(MKT, -( 0:5))), by=nation]
lmdata[, MKT1Y := Reduce(`+`, shift(MKT, -(0:11))), by=nation]
lmdata[, MKT2Y := Reduce(`+`, shift(MKT, -(0:23))), by=nation]
lmdata[, MKT3Y := Reduce(`+`, shift(MKT, -(0:35))), by=nation]
lmdata[, MKT5Y := Reduce(`+`, shift(MKT, -(0:59))), by=nation]
lmdata

# ## Test Correlation of Wide Market Return and IBOVESPA
# lmdata %>% filter(nation=="BRAZIL") %>% as.data.table
# tmp <- read.csv("0_data/BVSP.csv") %>%
#   mutate(bovespa = log(Adj.Close/dplyr::lag(Adj.Close))) %>%
#   mutate(year = year(Date), month = month(Date)) %>%
#   as.data.table %>%
#   select(year, month, bovespa) %>%
#   left_join(lmdata %>% filter(nation=="BRAZIL")) %>% na.omit
# cor(tmp[,c(3,5,6)])

# ## Test Correlation between MKT return and AEIG
# lmdata %>% group_by(nation) %>%
#   summarise(correl = cor(MKT, AEIG),
#             abscor = abs(cor(MKT, AEIG))) %>%
#   arrange(-abscor) %>% as.data.frame
#
# lmdata %>% filter(nation == "UNITED KINGDOM") %>% as.data.frame

## In Sample Analysis AEIG DM x EM ---------------------------------------------
lmdata <- mkt %>%
  left_join(AEIG) %>%
  group_by(nation) %>% mutate(AEIG = dplyr::lag(AEIG)) %>%
  select(year, month, AEIG, MKT=vwret) %>% na.omit
setDT(lmdata)
lmdata[, MKT3M := Reduce(`+`, shift(MKT, -( 0:2))), by=nation]
lmdata[, MKT6M := Reduce(`+`, shift(MKT, -( 0:5))), by=nation]
lmdata[, MKT1Y := Reduce(`+`, shift(MKT, -(0:11))), by=nation]
lmdata[, MKT2Y := Reduce(`+`, shift(MKT, -(0:23))), by=nation]
lmdata[, MKT3Y := Reduce(`+`, shift(MKT, -(0:35))), by=nation]
lmdata[, MKT5Y := Reduce(`+`, shift(MKT, -(0:59))), by=nation]
#lmdata

classification <- readxl::read_xlsx("2_pipeline/2_out/3d_EM_DM.xlsx",
                                    col_names = c("nation", "classification"),
                                    skip = 1)
# classification

resultsAEIG <- lmdata %>% group_by(nation) %>%
  # do(fitNation = tidy(lm(MKT   ~ AEIG, data = .))) %>%
  # do(fitNation = tidy(lm(MKT3M ~ AEIG, data = .))) %>%
  # do(fitNation = tidy(lm(MKT6M ~ AEIG, data = .))) %>%
  do(fitNation = tidy(lm(MKT1Y ~ AEIG, data = .))) %>%
  # do(fitNation = tidy(lm(MKT2Y ~ AEIG, data = .))) %>%
  # do(fitNation = tidy(lm(MKT3Y ~ AEIG, data = .))) %>%
  tidyr::unnest(fitNation)

resultsAEIG %>%
  left_join(classification) %>%
  filter(term=="AEIG") %>% filter(abs(statistic)<2) %>% data.frame

resultsAEIG %>%
  left_join(classification) %>%
  filter(term=="AEIG") %>% filter(abs(statistic)>2) %>%
  group_by(classification) %>% count



library(broom)
results <- lmdata %>% group_by(nation) %>%
  do(fitNation = tidy(lm(MKT ~ AEIG, data = .))) %>%
  tidyr::unnest(fitNation) %>%
  filter(term=="AEIG") %>% mutate(h = 1)
tmp <- lmdata %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT3M   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 3); results <- rbind(results, tmp)
tmp <- lmdata %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT6M   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 6); results <- rbind(results, tmp)
tmp <- lmdata %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT1Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 12); results <- rbind(results, tmp)

resultsAEIG <- results ; rm(results)

classification <- readxl::read_xlsx("2_pipeline/2_out/3d_EM_DM.xlsx",
                                    col_names = c("nation", "classification"),
                                    skip = 1)
#classification

resultsAEIG <- resultsAEIG %>% left_join(classification)
resultsAEIG %>% filter(abs(statistic)>2) %>% data.frame
resultsAEIG %>% filter(abs(statistic)>2) %>% group_by(h, classification) %>% count


## In Sample Analysis AEIG Growth Conditioned ----------------------------------
lmdataG <- mkt %>% left_join(AEIGlc %>% filter(lcycle == 2)) %>%
  group_by(nation) %>%
  mutate(AEIG = dplyr::lag(AEIG)) %>%
  select(year, month, AEIG, MKT=vwret) %>% na.omit
setDT(lmdataG)
lmdataG[, MKT3M := Reduce(`+`, shift(MKT, -( 0:2))), by=nation]
lmdataG[, MKT6M := Reduce(`+`, shift(MKT, -( 0:5))), by=nation]
lmdataG[, MKT1Y := Reduce(`+`, shift(MKT, -(0:11))), by=nation]
lmdataG[, MKT2Y := Reduce(`+`, shift(MKT, -(0:23))), by=nation]
lmdataG[, MKT3Y := Reduce(`+`, shift(MKT, -(0:35))), by=nation]
lmdataG[, MKT5Y := Reduce(`+`, shift(MKT, -(0:59))), by=nation]
#lmdataG

results <- lmdataG %>% group_by(nation) %>%
  do(fitNation = tidy(lm(MKT ~ AEIG, data = .))) %>%
  tidyr::unnest(fitNation) %>%
  filter(term=="AEIG") %>% mutate(h = 1)
tmp <- lmdataG %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT3M   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 3); results <- rbind(results, tmp)
tmp <- lmdataG %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT6M   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 6); results <- rbind(results, tmp)
tmp <- lmdataG %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT1Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 12); results <- rbind(results, tmp)

tmp <- lmdataG %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT2Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 24); results <- rbind(results, tmp)
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
#   0 (non-NA) cases

tmp <- lmdataG %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT3Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 36); results <- rbind(results, tmp)
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
#   0 (non-NA) cases

tmp <- lmdataG %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT5Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 60); results <- rbind(results, tmp)
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
#   0 (non-NA) cases

resultsAEIGg <- results ; rm(results)

classification <- readxl::read_xlsx("2_pipeline/2_out/3d_EM_DM.xlsx",
                                    col_names = c("nation", "classification"),
                                    skip = 1)
#classification

resultsAEIGg <- resultsAEIGg %>% left_join(classification)
resultsAEIGg %>% filter(abs(statistic)>2) %>% data.frame
resultsAEIGg %>% filter(abs(statistic)>2) %>% group_by(h, classification) %>% count

## In Sample Analysis AEIG Mature Conditioned ----------------------------------
lmdataM <- mkt %>% left_join(AEIGlc %>% filter(lcycle==3)) %>%
  group_by(nation) %>%
  mutate(AEIG = dplyr::lag(AEIG)) %>%
  select(year, month, AEIG, MKT=vwret) %>% na.omit
setDT(lmdataM)
lmdataM[, MKT3M := Reduce(`+`, shift(MKT, -( 0:2))), by=nation]
lmdataM[, MKT6M := Reduce(`+`, shift(MKT, -( 0:5))), by=nation]
lmdataM[, MKT1Y := Reduce(`+`, shift(MKT, -(0:11))), by=nation]
lmdataM[, MKT2Y := Reduce(`+`, shift(MKT, -(0:23))), by=nation]
lmdataM[, MKT3Y := Reduce(`+`, shift(MKT, -(0:35))), by=nation]
lmdataM[, MKT5Y := Reduce(`+`, shift(MKT, -(0:59))), by=nation]
#lmdataM

results <- lmdataM %>% group_by(nation) %>%
  do(fitNation = tidy(lm(MKT ~ AEIG, data = .))) %>%
  tidyr::unnest(fitNation) %>%
  filter(term=="AEIG") %>% mutate(h = 1)
tmp <- lmdataM %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT3M   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 3); results <- rbind(results, tmp)
tmp <- lmdataM %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT6M   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 6); results <- rbind(results, tmp)
tmp <- lmdataM %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT1Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 12); results <- rbind(results, tmp)
tmp <- lmdataM %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT2Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 24); results <- rbind(results, tmp)
tmp <- lmdataM %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT3Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 36); results <- rbind(results, tmp)
tmp <- lmdataM %>% group_by(nation) %>% do(fitNation = tidy(lm(MKT5Y   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% filter(term=="AEIG") %>% mutate(h = 60); results <- rbind(results, tmp)

resultsAEIGm <- results ; rm(results)

classification <- readxl::read_xlsx("2_pipeline/2_out/3d_EM_DM.xlsx",
                                    col_names = c("nation", "classification"),
                                    skip = 1)
#classification

resultsAEIGm <- resultsAEIGm %>% left_join(classification)
resultsAEIGm %>% filter(abs(statistic)>2) %>% data.frame
resultsAEIGm %>% filter(abs(statistic)>2) %>%
  group_by(h, classification) %>%
  count %>%
  arrange(classification)

## Add Adj R2 ------------------------------------------------------------------
tmp  <- lmdata %>% group_by(nation) %>% do(fitNation = glance(lm(MKT   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 1)
tmp2 <- lmdata %>% group_by(nation) %>% do(fitNation = glance(lm(MKT3M ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 3) ; tmp <- rbind(tmp, tmp2)
tmp2 <- lmdata %>% group_by(nation) %>% do(fitNation = glance(lm(MKT6M ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 6) ; tmp <- rbind(tmp, tmp2)
tmp2 <- lmdata %>% group_by(nation) %>% do(fitNation = glance(lm(MKT1Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 12); tmp <- rbind(tmp, tmp2)
rm(tmp2)
resultsAEIG <- resultsAEIG %>%
  left_join(tmp, by=c("nation", "h")) %>%
  select(h, emdm = classification , nation, estimate, statistic, p.value, adj.r2 = adj.r.squared, nobs) %>%
  arrange(h, emdm, nation)

tmp  <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 1)
tmp2 <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT3M ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 3) ; tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT6M ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 6) ; tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT1Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 12); tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT2Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 24); tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT3Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 36); tmp <- rbind(tmp, tmp2)
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
#   0 (non-NA) casesrm(tmp2)
#
tmp2 <- lmdataG %>% group_by(nation) %>% do(fitNation = glance(lm(MKT5Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 60); tmp <- rbind(tmp, tmp2)
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
#   0 (non-NA) casesrm(tmp2)
#
resultsAEIGg <- resultsAEIGg %>%
  left_join(tmp, by=c("nation", "h")) %>%
  select(h, emdm = classification , nation, estimate, statistic, p.value, adj.r2 = adj.r.squared, nobs) %>%
  arrange(h, emdm, nation)

tmp  <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT   ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 1)
tmp2 <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT3M ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 3) ; tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT6M ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 6) ; tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT1Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 12); tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT2Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 24); tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT3Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 36); tmp <- rbind(tmp, tmp2)
tmp2 <- lmdataM %>% group_by(nation) %>% do(fitNation = glance(lm(MKT5Y ~ AEIG, data = .))) %>% tidyr::unnest(fitNation) %>% select(nation, adj.r.squared, nobs) %>% mutate(h = 60); tmp <- rbind(tmp, tmp2)
rm(tmp2)
resultsAEIGm <- resultsAEIGm %>%
  left_join(tmp, by=c("nation", "h")) %>%
  select(h, emdm = classification , nation, estimate, statistic, p.value, adj.r2 = adj.r.squared, nobs) %>%
  arrange(h, emdm, nation)

## Tabelas ---------------------------------------------------------------------

# H3a Mature > Growth
results <- rbind(resultsAEIGg %>% mutate(lcycle = "Growth"),
                 resultsAEIGm %>% mutate(lcycle = "Mature"))

results <- results %>%
  select(h, lcycle, emdm, nation, everything()) %>%
  arrange(h, lcycle, emdm, nation)

tmp <- results %>%
  filter(abs(statistic)>=1.96) %>%
  group_by(h, lcycle) %>% count %>% select(h, lcycle, signif=n)
tmp2 <- results %>%
  filter(abs(statistic)<1.96) %>%
  group_by(h, lcycle) %>% count %>% select(h, lcycle, non_signif=n)

tmp %>% left_join(tmp2) %>% mutate(total = signif + non_signif)

results %>%
  group_by(h, lcycle) %>%
  summarise(adj.r2 = round(abs(mean(statistic, na.rm = T)),2)) %>%
  as.data.frame

results %>%
  filter(abs(statistic)>=1.96) %>%
  group_by(h, lcycle) %>%
  summarise(adj.r2 = round(abs(mean(adj.r2)*100),2)) %>%
  as.data.frame


# H3b DM > EM
resultsAEIG %>% filter(abs(statistic)>2) %>% group_by(h, emdm) %>% count
resultsAEIG %>% group_by(h, emdm) %>% summarise(adj.r2 = abs(mean(adj.r2)*100))

# ## Export Tables ---------------------------------------------------------------
# xlsx::write.xlsx(results,     file = "3_output/results/mature_vs_growth.xlsx")
# xlsx::write.xlsx(resultsAEIG, file = "3_output/results/dm_vs_em.xlsx")
