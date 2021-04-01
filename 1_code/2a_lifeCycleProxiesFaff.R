library(lubridate)
library(data.table)
library(MASS)
library(tidyverse)

# The data are obtained from Compustat and CRSP, covering the
# period 1973 to 2014.

# financial (SIC 6000- 6999) and utility (SIC 4900-4949) industries are excluded
# from the sample because the former have relatively low physical capital
# investment, while the latter are under government regulation.

# a firm is included in the sample only if it has CRSP share codes 10 or 11 and
# is incorporated in US (FIC = USA)
# Share codes 10 and 11 refer to ordinary common shares with no special status or
# no special status necessary

# We require firms to provide valid information on their total assets, earnings
# before extraordinary items, retained earn- ings, sales growth, market
# capitalization, changes in cash holdings, investment, cash flow, and external
# finance. Some additional data exclusions are necessary: firms with market
# capitalization of equity less than USD 10 million or firms with abnormal
# assets or sales growth (greater than 1) are also excluded.10 To control for
# the effect of outliers, all variables are winsorized at the 1st and 99th
# percentile. The final sample consists of more than 12,000 firms, producing
# over 100,000 firm-year observations.

# AGE is CRSP listed firm age
# RETA is the ratio of retained earnings to total assets
# 

# comp_a %>% group_by(year(datadate)) %>% count %>% data.frame %>% summarise(sum = sum(n))
# comp_a %>% filter(at < 10000) %>% select(gvkey) %>% unique
# glimpse(comp_a)

## Dickson (2011)

## Load data from WRDS Server ## ###############################################

lcycle <- readRDS("0_data/wrds/raw_comp_a_lifecycle_data.rds")

# Set missing retained earnings figures to zero
lcycle[is.na(re), re := 0]

# Calculate RETA as the ratio of retained earnings to total assets
lcycle[, reta := re/at]

# EBIT = EBIT/AT
lcycle[, ebit_at := ebit / at]

# Calculate Market Value of Equity at Year End
# use prcc_c at the calendar year end for a fair cross sectional comparison
# csho item is presented in millions of shares.
lcycle[, meComp := prcc_c*csho]

# Keep only rows with valid at, meComp and ebit 
lcycle <- lcycle[complete.cases(at, meComp, ebit)]

# Calculate Compustat Age
lcycle <- lcycle[order(datadate, gvkey)]
lcycle[, age := year(datadate)-year(first(datadate)), by=gvkey]
lcycle[gvkey=="001000"]

# Calculate AssetGrowth
lcycle[, AGrth := (at - shift(at))/shift(at), by = gvkey]

# Calculate SalesGrowth
lcycle[, SGrth := (sale - shift(sale))/shift(sale), by = gvkey]

# Dickinson (2011) classification scheme (DCS) - 8 groups (DCS8g)
lcycle <- lcycle %>%
  mutate(DCS8g = case_when((oancf <= 0 & ivncf <= 0 & fincf >  0) ~ 1,
                                 (oancf >  0 & ivncf <= 0 & fincf >  0) ~ 2,
                                 (oancf >  0 & ivncf <= 0 & fincf <= 0) ~ 3,
                                 (oancf <= 0 & ivncf <= 0 & fincf <= 0) ~ 4,
                                 (oancf >  0 & ivncf >  0 & fincf >  0) ~ 5,
                                 (oancf >  0 & ivncf >  0 & fincf <= 0) ~ 6,
                                 (oancf <= 0 & ivncf >  0 & fincf >  0) ~ 7,
                                 (oancf <= 0 & ivncf >  0 & fincf <= 0) ~ 8,
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
lcycle %>% 
  mutate(DCS = case_when(DCS8g <= 3 ~ DCS8g,
                         DCS8g >= 4 ~ 4)) %>% 
  mutate(DCS_name = case_when((DCS == 1) ~ "Introduction",
                              (DCS == 2) ~ "Growth",
                              (DCS == 3) ~ "Mature",
                              (DCS == 4) ~ "Shake-Out/Decline")) %>% 
  mutate(DCS_name = as.factor(DCS_name)) -> lcycle

lcycle %>% 
  group_by(DCS) %>% count %>% ungroup %>% mutate(f = n/sum(n))

lcycle %>% filter(DCS!=0) %>%
  group_by(DCS) %>% count %>% ungroup %>% mutate(f = n/sum(n))

lcycle %>%  filter(DCS!=0) %>% filter( meComp > 10 & SGrth < 1 & AGrth < 1) %>% 
  group_by(DCS) %>% count %>% ungroup %>% mutate(f = n/sum(n))

# Filter
lcycle %>% filter( meComp > 10 & SGrth < 1 & AGrth < 1) -> lcycle

# Only valid observations
lcycle %>%
  filter(complete.cases(DCS, age, reta, ebit_at, AGrth, meComp, at)) %>% 
  mutate(finite_test = DCS + age + reta + ebit_at + AGrth + meComp + at) %>% 
  filter(is.finite(finite_test)) %>% dplyr::select(-finite_test) -> lcycle2

# Winsorize
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
summary(lcycle2 %>% dplyr::select(age, reta, ebit_at, AGrth))
lcycle2 %>% 
  mutate(reta = winsorize(reta)) %>% 
  mutate(ebit_at = winsorize(ebit_at)) -> lcycle2
lcycle2 <- lcycle2[complete.cases(reta, ebit_at)]
summary(lcycle2 %>% dplyr::select(age, reta, ebit_at, AGrth))


# Train Data
lcycle2 %>% filter(DCS!=0) -> traindata

# Multiclass linear discriminant analysis (MLDA)

summary(traindata %>% dplyr::select(age, reta, ebit_at, AGrth))
lda.fit <- lda(DCS ~ age + reta + ebit_at + AGrth, data=traindata)
# lda.fit <- lda(DCS ~ age + re + ebit + AGrth, data=lcycle)
lda.fit
lda.predict <- predict(lda.fit, newdata = lcycle2)
unique(lda.predict$class)
# str(lda.predict)
lcycle2[, faff := lda.predict$class]

lcycle2 %>% 
  group_by(faff) %>% count %>% ungroup %>% mutate(f = n/sum(n))

lcycle2 %>% filter(DCS!=0) %>%
  group_by(DCS) %>% count %>% ungroup %>% mutate(f = n/sum(n))


lcycle2 %>% mutate(overlap = DCS==faff) %>%
  group_by(DCS) %>% summarise(overlap=sum(overlap), total=n(),
                              percentual=sum(overlap) / n())

lcycle2 %>% mutate(overlap = DCS==faff) %>%
  group_by(faff) %>% summarise(overlap=sum(overlap), total=n(),
                              percentual=sum(overlap) / n())

saveRDS(lcycle2, "2_pipeline/2_out/2a_life_cycle_faff.rds")

# #convert to data frame 
# newdata <- data.frame(type = lcycle[,"DCS"], lda = lda.predict$x)
# library(ggplot2)
# ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)
# 
# 
# #convert to data frame 
# newdata <- data.frame(type = wine[,1], lda = wine.lda.values$x)
# library(ggplot2)
# ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)
# 
# 
# glm.fit <- glm()
# 
# 
# nrow(lcycle)
# lcycle2 <- na.omit(lcycle)
# nrow(lcycle2)
# nrow(lcycle) - nrow(lcycle2)
# 
# 
# glimpse(lcycle2)
# lcycle2 <- as_tibble(lcycle2)
# lcycle2 %>% filter(oancf==0 | ivncf == 0 | fincf == 0)
# lcycle2 %>% filter(oancf==0 & ivncf == 0 & fincf == 0)
# 
# ## base antiga 
# # lc <- readRDS("Data/WRDS/raw_comp_life_cycle.rds")
# # lc<- readRDS("Data/WRDS/raw_comp_a_lifecycle_data.rds")
# # lcycle2 <- lc %>% select(-LC, -LC_name)
# 
# lcycle3 <- lcycle2 %>%
#   mutate(LC = case_when((oancf <= 0 & ivncf <= 0 & fincf >  0) ~ 1,
#                         (oancf >  0 & ivncf <= 0 & fincf >  0) ~ 2,
#                         (oancf >  0 & ivncf <= 0 & fincf <= 0) ~ 3,
#                         (oancf <= 0 & ivncf <= 0 & fincf <= 0) ~ 4,
#                         (oancf >  0 & ivncf >  0 & fincf >  0) ~ 5,
#                         (oancf >  0 & ivncf >  0 & fincf <= 0) ~ 6,
#                         (oancf <= 0 & ivncf >  0 & fincf >  0) ~ 7,
#                         (oancf <= 0 & ivncf >  0 & fincf <= 0) ~ 8,
#                         TRUE ~ 0)) %>% 
#   mutate(LC_name = case_when((oancf <= 0 & ivncf <= 0 & fincf >  0) ~ "1 Introduction",
#                              (oancf >  0 & ivncf <= 0 & fincf >  0) ~ "2       Growth",
#                              (oancf >  0 & ivncf <= 0 & fincf <= 0) ~ "3       Mature",
#                              (oancf <= 0 & ivncf <= 0 & fincf <= 0) ~ "4    Shake-Out",
#                              (oancf >  0 & ivncf >  0 & fincf >  0) ~ "5    Shake-Out",
#                              (oancf >  0 & ivncf >  0 & fincf <= 0) ~ "6    Shake-Out",
#                              (oancf <= 0 & ivncf >  0 & fincf >  0) ~ "7      Decline",
#                              (oancf <= 0 & ivncf >  0 & fincf <= 0) ~ "8      Decline",
#                              TRUE ~ as.character(NA))) %>% 
#   mutate(LC_name = as.factor(LC_name))
# 
# lcycle3 %>%
#   mutate(year = year(datadate)) %>%
#   group_by(year) %>% 
#   count %>% data.frame
# 
# lcycle3 %>%
#   mutate(year = year(datadate)) %>%
#   group_by(LC_name) %>% 
#   summarise(n = n()) %>%
#   mutate(freq = round( (n/sum(n))*100 ,0) )
# 
# lcycle3 <- lcycle3 %>% 
#   mutate(LC2 = case_when(LC <= 3 ~ LC,
#                          LC >= 4 ~ 4)) %>% 
#   mutate(LC2_name = case_when(LC <= 3 ~ as.character(LC_name),
#                               LC >= 4 ~ "4    Shake-Out/Decline")) %>%
#   mutate(LC2_name = as.factor(LC2_name))
# 
# saveRDS(lcycle3, "~/Data/EIG/life_cycle.rds")
# 
# rm(lcycle, lcycle2, lcycle3)