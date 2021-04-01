## Acquiring a Proxy for a Firm's Age
# 
# Often the first trading date is the IPO date, but not always, and IPO date may
# be far after the company was founded.
# 
# Compustat has an IPO date but is it not well populated. And unfortunately,
# there is no direct data item that will reflect the "age" of a company in
# Compustat either.
# 
# So there are two approximations that might help you approximate a firm's age:
# 
# 1. Compute the years of accounting data available in Compustat, but this is
# imprecise.
# 
# 2. Use the  CRSP header file ( MSFHDR  ) which provides the variables BEGDAT
# (date of beginning stock data) and ENDDAT (date of ending stock data). The
# difference will give you the number of days each stock was included in CRSP
# database.
# 
# Some additional complications will appear in a few cases when a firm (PERMCO)
# had multiple securities (PERMNOs). In those cases, you would need to take the
# oldest BEGDAT and the latest ENDDAT.
# 
# 
# Source: https://wrds-www.wharton.upenn.edu/pages/support/support-articles/crsp/stock/acquiring-proxy-firms-age/
# 

# install.packages("fastDummies")
# install.packages("plyr")

library(plyr)
library(data.table)
library(dplyr)
library(tibble)

# library(tidyverse)

# AdjAgeCRSP -------------------------------------------------------------------


# Load the raw crsp data downloaded with R -------------------------------------
crsp_m <- as_tibble(readRDS(file = "0_data/wrds/raw_crsp_m.rds"))
crsp_names <- readRDS(file = "0_data/wrds/raw_crsp_names.rds")
msfhdr <- readRDS(file = "0_data/wrds/raw_msfhdr.rds")


# CRSP Market Equity -----------------------------------------------------------

setDT(crsp_m)
crsp_m[, me := abs(prc) * shrout]
crsp_m <- crsp_m[!is.na(me),]

# keep last market-value by year
crsp_m[, year := year(date)]
crsp_m <- crsp_m[order(date)]
crsp_m[, last_me := last(me), by=c("permno", "year")]
crsp_a <- unique(crsp_m[, .(permno, permco, year, me=last_me)])

crsp_a[, me := round(me)]
# crsp_a[, last_me := shift(me, 1L), by=c("permno")]
# crsp_a[is.na(me) & !is.na(last_me)]
# crsp_a[is.na(me) & !is.na(last_me), me := last_me]
## Remove NA
crsp_a[is.na(me)]
crsp_a <- crsp_a[,
                 lapply(.SD, function(x) tail(x[!is.na(x)],1)),
                 by = c("permco", "permno", "year"),
                 .SDcols = "me"]

# CRSP Year -------------------------------------------------------------------
setDT(msfhdr)
crsp_age <- msfhdr[, . (permno, permco, begdat, enddat)]
crsp_age[, begdat := year(begdat)]
crsp_age[, enddat := year(enddat)]
crsp_age[, begyear := first(begdat), by=permco]
crsp_age[, endyear := last(enddat), by=permco]
crsp_age[, age := endyear - begyear + 1, by=permco]
crsp_age <- unique(crsp_age[,.(permno, permco, age)])

crsp_age


# Adj Year Industry ------------------------------------------------------------
# Industry and size adjusted firm age
# Finally, we use CRSP listed firm age adjusted for industry and size effects
# (AdjAGEi) as our final life-cycle proxy. As mentioned, the time required for
# firms to mature varies across industries and firms can exist long before they
# become listed. To address this problem, we adjust the firm age for the
# cross-sectional age differences across industries. To the extent that a larger
# firm tends to exist longer, we also adjust the firm age for size to control
# for the cross-sectional age differences before listing. Specifically, we
# generate industry indicator variables (InDumj) based on 2-digit SIC codes
# (i.e. InDumj takes a value of one if firm i is in industry j and zero
# otherwise). For each industry based on 2-digit SIC codes, we also sort firms
# into quintiles based on their size and create size indicator variables
# (SizeDumj,k) for each kth quintile (i.e. SizeDumj,k takes a value of one if
# the firm i is in industry j with size in kth quintile and zero otherwise).
# We then regress AGE on InDum and SizeDum and use the percentile rank of the
# residual value (AdjAGEi) from the regression as a proxy for life-cycle.

crsp_names[, siccd2digit := stringr::str_sub(siccd,1,2)]
crsp_age_sic <- crsp_names %>%
  select(permno, permco, sic=siccd2digit) %>% unique %>% 
  left_join(crsp_age, by = c("permco", "permno")) %>% 
  filter(sic!=0)

# fazer um crsp anual com tamanho
# importar o begdat e fazer a idade de cada ano
# importar também o siccd 2-digit


crspAdjAGE <- crsp_a %>% 
  left_join(crsp_age_sic, by = c("permco", "permno")) %>%
  filter(!is.na(sic))

crspAdjAGE[, size := cut(me, quantile(me, probs = 0:5/5),
                         labels = FALSE, include.lowest = TRUE), by=year]

crspAdjAGE <- crspAdjAGE %>%
  select(permco:year, size, sic, age) %>% 
  fastDummies::dummy_cols(select_columns = c("size", "sic")) %>% 
  as_tibble
crspAdjAGE

library(plyr)
plyr::ddply(crspAdjAGE,.(year), transform, e=residuals(
  lm(age ~ size_1 + size_2 + size_3 + size_4 + size_5 + sic_10 +
       sic_11 + sic_12 + sic_13 + sic_14 + sic_15 + sic_16 + sic_17 + sic_18 + sic_19 + sic_20 +
       sic_21 + sic_22 + sic_23 + sic_24 + sic_25 + sic_26 + sic_27 + sic_28 + sic_29 + sic_30 +
       sic_31 + sic_32 + sic_33 + sic_34 + sic_35 + sic_36 + sic_37 + sic_38 + sic_39 + sic_40 +
       sic_41 + sic_42 + sic_43 + sic_44 + sic_45 + sic_46 + sic_47 + sic_48 + sic_49 + sic_50 +
       sic_51 + sic_52 + sic_53 + sic_54 + sic_55 + sic_56 + sic_57 + sic_58 + sic_59 + sic_60 +
       sic_61 + sic_62 + sic_63 + sic_64 + sic_65 + sic_66 + sic_67 + sic_70 + sic_71 + sic_72 +
       sic_73 + sic_74 + sic_75 + sic_76 + sic_78 + sic_79 + sic_80 + sic_81 + sic_82 + sic_83 +
       sic_84 + sic_85 + sic_86 + sic_87 + sic_88 + sic_89 + sic_90 + sic_91 + sic_92 + sic_93 +
       sic_94 + sic_95 + sic_96 + sic_97 + sic_99)
)) %>% select(permco:age, e) %>% as_tibble -> crspAdjAGE

detach("package:plyr", unload = TRUE)

# setDT(crspAdjAGE)
# crspAdjAGE[, AdjAGE := rank(e)/length(e), by=year]
crspAdjAGE <- crspAdjAGE %>%
  group_by(year) %>% 
  # mutate(percrank=rank(e)/length(e)) %>%
  mutate(AdjAGE=percent_rank(e)) %>%
  ungroup

saveRDS(crspAdjAGE, file = "2_pipeline/2_out/2b_crspAdjAGE.rds")
