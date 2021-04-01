# install.packages("data.table")
# install.packages("lubridate" )
# install.packages("tidyverse" )

## Imports ---------------------------------------------------------------------
## Install packages by using groundhog to address reproducible research
## Se more in http://datacolada.org/95 or https://groundhogr.com/

# ## All the library imports go here
# pkgs = c('lubridate','data.table', 'dplyr')
# 
# library(groundhog)
# groundhog_day = "2020-12-31" # Last date when script runs as expected
# groundhog.library(pkgs, groundhog_day)

library(lubridate)
library(data.table)
library(tidyverse)

## >> ORGANIZE CRSP/COMPUSTAT DATA ## ##########################################
## Load local data
## > Annual COMPUSTAT Block --- ================================================
comp_a <- readRDS(file = "0_data/wrds/raw_comp_a.rds")

## Calcular book-equiy --------------------------------------------------------
# annual book equity per Davis, Fama, and French (2000) as stockholders' book
# equity, plus balance sheet deferred taxes and investment tax credit (Compustat
# annual item TXDITC) if available, minus the book value of preferred stock.
# Stockholders' equity is the value reported by Compustat (item SEQ), if
# available. Otherwise, we use the book value of common equity (item CEQ) plus
# the par value of preferred stock (item PSTK), or the book value of assets
# (item AT) minus total liabilities (item LT). Depending on availability, we use
# redemption value (item PSTKRV), liquidating (item PSTKL), or par value
# (item PSTK) for the book value of preferred stock.

# # Calcula via python
# source_python('..\\Python\\eig.py')
# comp_a2 <- organize_compustat(comp_a)

# Calcular aqui no R
comp_a$year <- lubridate::year(comp_a$datadate)

# create preferrerd stock
comp_a <- comp_a %>%
  mutate(ps = ifelse(is.na(pstkrv), pstkl, pstkrv)) %>%
  mutate(ps = ifelse(is.na(ps), pstk, ps)) %>% 
  mutate(ps = ifelse(is.na(ps), 0, ps))

# create book equity
comp_a <- comp_a %>% 
  mutate(txditc = ifelse(is.na(txditc), 0, txditc)) %>%
  mutate(be = (seq + txditc - ps) ) %>% 
  mutate(be = ifelse(be>0, be, NA)) %>% as_tibble

# # Compara
# glimpse(comp_a2)
# glimpse(comp_a)
# comp_a2 %>% select(gvkey, year, be) %>% as_tibble
# comp_a %>% select(gvkey, year, be) %>% as_tibble


## Compute investment-to-assets ----------------------------------------------
# current investment-to-assets as total assets (Compustat annual item AT) from
# the most recent fiscal year ending at least four months ago minus the total
# assets from one year prior, scaled by the 1-yearprior total assets.
comp_a <- comp_a %>%
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  # Create Lagged AT
  mutate( lag_at = dplyr::lag(at)     ) %>% 
  # investment-to-assets
  mutate( ia = ((at - lag_at)/lag_at) ) %>%
  ungroup

# # Comparar com o calculado n codigo do Python
# comp_a2 %>% select(gvkey, year, at, lag_at, ia) %>% as_tibble
# comp_a %>% select(gvkey, year, at, lag_at, ia) %>% as_tibble


# Compute IG as investment-to-assets change -----------------------------------
#

# Compute dtI/A ---------------------------------------------------------------
# Oneyear-ahead investment-to-assets change
#
# The one-year ahead investment-to-assets change, d1I/A, is the investment-to-
# assets from the first year after the most recent fiscal year end minus the
# current investment-to-assets.
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(d0_ia = ia - dplyr::lag(ia)) %>% 
  mutate(d1_ia = dplyr::lead(ia) - ia) %>%
  mutate(d2_ia = dplyr::lead(ia, n = 2L) - ia) %>%
  mutate(d3_ia = dplyr::lead(ia, n = 3L) - ia) %>% 
  ungroup

# comp_a %>%
#   select(gvkey, year, at, lag_at, ia, d0_ia, d1_ia, d2_ia, d3_ia) %>% as_tibble %>%
#   filter(gvkey == "006066") %>% as.data.frame # ibm

## Create operating cash flows (Cop) -------------------------------------------
# operating cash flows, Cop, as total revenue (Compustat annual item REVT) minus
# cost of goods sold (item COGS), minus selling, general, and administrative
# expenses (item XSGA), plus research and development expenditures (item XRD,
# zero if missing), minus change in accounts receivable (item RECT), minus
# change in inventory (item INVT), minus change in prepaid expenses (item XPP),
# plus change in deferred revenue (item DRC plus item DRLT), plus change in
# trade accounts payable (item AP), and plus change in accrued expenses (item
# XACC), scaled by book assets,
#
# all from the fiscal year ending at least four months ago. <-- de ano p/ mes
#
# Missing annual changes are set to zero.
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(defrev = drc + drlt) %>% 
  mutate_at(vars(revt, cogs, xsga, xrd, rect, invt, xpp,
                 defrev, ap, xacc),  replace_na, 0 ) %>% 
  mutate(cop = (revt - cogs - xsga + xrd
                - (rect - dplyr::lag(rect))
                - (invt - dplyr::lag(invt))
                - (xpp  - dplyr::lag(xpp))
                + (defrev - dplyr::lag(defrev))
                + (ap - dplyr::lag(ap))
                + (xacc - dplyr::lag(xacc))      ) / at) %>% 
  ungroup

# comp_a %>% select(gvkey, datadate, d1_ia, ia, cop) %>% as_tibble
# comp_a %>% select(gvkey, datadate, d1_ia, ia, cop) %>%
#   filter(gvkey == "006066") %>% as.data.frame # ibm

## Compute Sales Growth --------------------------------------------------------
# SG is the log growth rate of sales (Compustat data item Sale)
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(SG = sale / dplyr::lag(sale) ) %>%
  mutate(SG = log(SG) ) %>% ungroup

# The warnings is about the Inf values
# So I set the Inf values in Sales Growth (SG) as zero 
# (zero because that is a dependent variable)
setDT(comp_a)
comp_a[!is.finite(SG) & !is.na(SG), SG := 0] %>% as_tibble -> comp_a
# comp_a %>% select(gvkey:at, sale, SG) %>%
#    filter(gvkey=="001117" & year(datadate)=="1990")
#    # filter(is.infinite(SG))

## Compute Cash Flow (CF) ------------------------------------------------------
# cash flow (Compustat data items NI + DP) divided by capital (Compustat data item PPEGT)
comp_a <- comp_a %>% 
  mutate_at(vars(ni, dp, ppegt),  replace_na, 0 ) %>%
  mutate(CF = (ni + dp)/ppegt)

## Compute Cash Flow Growth (CFG) ----------------------------------------------
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(CFG = CF - dplyr::lag(CF)) %>% ungroup

## Compute Earnings Growth (EG) ------------------------------------------------
# EG is defined as the change in earnings (Compustat data item IB) divided by
# capital (Compustat data item PPEGT).
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(EG = (ib - dplyr::lag(ib))/ ppegt ) %>% ungroup

## Compute Profitability Growth ------------------------------------------------
# PG is defined as the change in profitability (Compustat data items
# EBITDA-(XINT-IDIT)-(TXT-TXDC)) divided by capital (Compustat data item PPEGT).
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(profitability = (ebitda - (xint-idit) - (txt - txdc) / ppegt ) ) %>% 
  mutate(profitability_lag = dplyr::lag(profitability) ) %>% 
  mutate(PG = profitability - profitability_lag) %>% ungroup

## Compute IG as CAPXt/CAPXt-1 -------------------------------------------------
#
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate(IGc0 = log( capx / dplyr::lag(capx)  ) ) %>%
  mutate(IGc1 = log( dplyr::lead(capx) / capx ) ) %>% 
  mutate(IGc2 = log( dplyr::lead(capx, n = 2L) / capx ) ) %>% 
  mutate(IGc3 = log( dplyr::lead(capx, n = 3L) / capx ) ) %>% ungroup

# The warnings is about the Inf values
# So I set Inf values as NA (NA because that is a dependent variable)
setDT(comp_a)
comp_a[!is.finite(IGc0), IGc0 := NA]
comp_a[!is.finite(IGc1), IGc1 := NA]
comp_a[!is.finite(IGc2), IGc2 := NA]
comp_a[!is.finite(IGc3), IGc3 := NA]
comp_a <- comp_a %>% as_tibble

## Compute ID ------------------------------------------------------------------
# I D is equal to 1 if a firm increases its total debt by more than 10% and 0
# otherwise, where new debt issues is defined by change in total debt (Compustat
# data items DLTT + DLC) divided by lag debt.
comp_a <- comp_a %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>%
  mutate_at(vars(dltt, dlc),  replace_na, 0 ) %>%
  mutate(debt_increase = ( (dltt+dlc) - (dplyr::lag(dltt)+dplyr::lag(dltt)) ) /
           (dplyr::lag(dltt)+dplyr::lag(dltt)) ) %>% 
  ungroup

setDT(comp_a)
comp_a[debt_increase > 0.1, Id := 1]
comp_a[debt_increase <= 0.1, Id := 0]
comp_a <- as_tibble(comp_a)

## Compute Ie - Part 1/2 -------------------------------------------------------
# I E is equal to 1 if a firm increases its equity by more than 5% and 0
# otherwise, where new share issues is defined as the sale of common and
# preferred stock (Compustat data item SSTK) divided by lag market equity after
# 1971, and the growth rate of the split-adjusted shares (Compustat data items
# CSHO ? AJEX) before 1971 due to the data availability of SSTK
setDT(comp_a)
comp_a[year(datadate) <= 1971, new_shares := sstk]
comp_a[year(datadate) >  1971, new_shares := csho * ajex]
comp_a <- as_tibble(comp_a)
# Continue on next script


## Add end of the month as the date to link to crsp ----------------------------
# I/A: from the most recent fiscal year ending at least four months ago.
# Cop: from the fiscal year ending at least four months ago.
# comp_a <- comp_a %>% mutate(monthlink = ( datadate %m+% months(4) ) )


# Now align the annual Compustat variables in calendar month with the assumption
# that annual information is available with a lag of 4 months (if we had
# point-in-time we would use that)
setDT(comp_a)
comp_a[, monthlink :=  ymd(as.Date(datadate)) %m+% months(4)]
# comp_a[, .(datadate, monthlink)]
comp_a[, monthlink :=  ( ceiling_date(ymd(monthlink), 'month') %m-% days(1) )]

comp_a <- as_tibble(comp_a)
# comp_a %>% select(gvkey, datadate, monthlink, ia, cop) %>% drop_na %>% data.frame

## > Quarterly COMPUSTAT Block --- ==============================================
comp_q <- readRDS(file = "0_data/wrds/raw_comp_q.rds")
comp_q <- as_tibble(comp_q)
comp_q

comp_q <- comp_q %>% mutate(year = year(datadate)) %>% arrange(gvkey, datadate)

## Create book value of preferred stock
#
# We use redemption value (item PSTKRQ) if available, or carrying value for
# the book value of preferred stock (item PSTKQ).
# http://global-q.org/uploads/1/2/2/6/122679606/factorstd_2020july.pdf
#
# ps = (redemption value (item PSTKRQ))
#    = or carrying value for the book value of preferred stock (item PSTKQ)
#

# create preferrerd stock
comp_q <- comp_q %>%
  mutate(ps = if_else(is.na(pstkq), pstkrq, pstkq)) %>% 
  mutate(ps = if_else(is.na(ps), 0, ps))

## Create stockholders equity
#
# Depending on availability, we use stockholders equity (item SEQQ), or
# common equity (item CEQQ) plus the carrying value of preferred stock (item
# PSTKQ), or total assets (item ATQ) minus total liabilities (item LTQ) in
# that order as shareholders equity.
#

comp_q <- comp_q %>%
  mutate(pstkq = if_else(is.na(pstkq), 0, pstkq)) %>% 
  #    shareholders equity = stockholders equity (item SEQQ)
  # or shareholders equity = or common equity (item CEQQ) + the carrying value of preferred stock (item PSTKQ)
  mutate(se = if_else(is.na(seqq), ceqq+pstkq, seqq)) %>% 
  # or shareholders equity = or total assets (item ATQ) - total liabilities (item LTQ) in that order as 
  mutate(se = if_else(is.na(se), atq-ltq, se))
# compq['se']=np.where(compq['se'].isnull(), compq['atq']-compq['ltq'], compq['se'])


## Create Quartely book equity -------------------------------------------------
#
# In particular, book equity is shareholders equity, plus balance sheet
# deferred taxes and investment tax credit (item TXDITCQ) if available, minus
# the book value of preferred stock.
#
# be (book equity) = se (shareholders equity) 
#                       + tax (balance sheet deferred taxes and investment tax credit (item TXDITCQ))
#                       - ps (book value of preferred stock)
comp_q <- comp_q %>% 
  mutate(txditcq = if_else(is.na(txditcq), 0, txditcq)) %>% 
  # compq['txditcq']=compq['txditcq'].fillna(0)
  mutate(be = se + txditcq + ps) %>% 
  # compq['be']=compq['se']+compq['txditcq']-compq['ps']
  mutate(be = ifelse( be > 0, be, NA)) %>% filter(be>0)
# compq['be']=np.where(compq['be']>0, compq['be'], np.nan)

## Create 1 quarter Lagged Book Equity
comp_q <- comp_q %>% 
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate( lag_be = dplyr::lag(be) ) %>% 
  ungroup

## Create ROE -----------------------------------------------------------------
comp_q <- comp_q %>% 
  # Roe is income before extraordinary items (Compustat quarterly item IBQ)
  # scaled by the 1-quarter-lagged book equity
  mutate( roe = ibq / lag_be )

## Create dROE ----------------------------------------------------------------
# Roe is income before extraordinary items (Compustat quarterly item IBQ)
# scaled by the 1-quarter-lagged book equity. We compute dRoe with earnings
# from the most recent announcement dates (item RDQ), and if not available,
# from the fiscal quarter ending at least four months ago.
comp_q <- comp_q %>% 
  # dRoe is Roe minus the four-quarter-lagged Roe
  # mutate( quarter= fqtr ) %>% 
  arrange(gvkey, datadate) %>% group_by(gvkey, fqtr) %>% 
  mutate( dROE = roe - dplyr::lag(roe,1)) %>% 
  ungroup

# # conferindo
# comp_q %>% select(-pstkq, -pstkrq, -seqq, -ceqq, -ps, -se) %>%
#   filter(gvkey == "006066") # ibm
# 0.0448 - 0.0458 # 1973-Q1 # -0.001  ~ -0.000965
# 0.0467 - 0.0515 # 1972-Q4 # -0.0048 ~ -0.00475



## Add end of the month as the date to link to crsp ----------------------------
## Organizar datas 4 months lags e substituir rdq por ultimo balanco disponivel

## For ROE:
# (1) Earnings data in Compustat quarterly files are used in the months
# immediately after the most recent public quarterly earnings announcement dates
# (Compustat quarterly item RDQ).
# (2) With a restriction: for a firm to enter the factor construction, we
# require the end of the fiscal quarter that corresponds to its most recently
# announced quarterly earnings to be within six months prior to the portfolio
# formation. (rdq - datadate < 6)
# (3) To overcome the lack of quarterly earnings announcement dates, we use the
# most recent quarterly earnings from the fiscal quarter ending at least four
# months prior to the portfolio formation month.
# (4) To maximize the coverage for quarterly book equity, whenever available we
# first use quarterly book equity from Compustat quarterly files

# we use the most recent quarterly earnings from the fiscal quarter
# ending at least four months prior to the portfolio formation month.
## For dRoe: We compute dRoe with earnings from the most recent announcement
# dates (item RDQ), and if not available, from the fiscal quarter ending at
# least four months ago.
DT <- as.data.table(comp_q)
DT[is.na(rdq), rdq2 :=  ymd(as.Date(datadate)) %m+% months(4)]
DT[!is.na(rdq), rdq2 := rdq]
## all from the most recent fiscal year ending at least four months ago.
DT[, monthlink := ( ceiling_date(ymd(rdq2), 'month') %m-% days(1) ) ]
comp_q <- as_tibble(DT) ; rm(DT)

# comp_q %>%
#   # filter(year<1970) %>%
#   # filter(gvkey == "006066") %>%
#   select(gvkey, datadate, rdq, rdq2, monthlink, roe, dROE)
# 
# comp_q %>%
#   select(gvkey, datadate, rdq2, roe, dROE) %>% 
#   filter(!is.na(roe)) %>% arrange(datadate)

# source_python('..\\Python\\eig.py')
# roe_m <- get_roe() # Codigo muito lento
# save(roe_m, file="eig\\roe_m.RData")
# load("eig\\roe_m.RData")
# tibble(roe_m)

## > CRSP Block ## #############################################################

## Load the raw crsp data downloaded with R
crsp_m <- as_tibble(readRDS(file = "0_data/wrds/raw_crsp_m.rds"))

## Change variable format to int -----------------------------------------------
# # Python code to replicate
# crsp_m[['permco','permno',
#         'shrcd','exchcd', 'siccd']] = crsp_m[['permco','permno',
#                                               'shrcd','exchcd',
#                                               'siccd']].astype(int)
crsp_m[c("permco", "permno",
         "shrcd", "exchcd",
         "siccd")] <- sapply(crsp_m[c("permco", "permno",
                                      "shrcd", "exchcd",
                                      "siccd")], as.integer)

## Line up date to be end of month ---------------------------------------------
# # Python code to replicate
# crsp_m['date']=pd.to_datetime(crsp_m['date'])
# crsp_m['jdate']=crsp_m['date']+MonthEnd(0)
crsp_m$date  <- as.Date(crsp_m$date)
crsp_m$jdate <- crsp_m$date 
lubridate::day(crsp_m$jdate) <- lubridate::days_in_month(crsp_m$jdate)

# add delisting return ---------------------------------------------------------
dlret <- as_tibble(readRDS(file = "0_data/wrds/raw_dlret.rds"))

# # # Python code to replicate
# dlret.permno=dlret.permno.astype(int)
# dlret['dlstdt']=pd.to_datetime(dlret['dlstdt'])
# dlret['jdate']=dlret['dlstdt']+MonthEnd(0)
dlret$permno <- as.integer(dlret$permno)
dlret$dlstdt   <- as.Date(dlret$dlstdt)
dlret$jdate <- dlret$dlstdt
lubridate::day(dlret$jdate) <- lubridate::days_in_month(dlret$jdate)

# # Python code to replicate
# crsp = pd.merge(crsp_m, dlret, how='left',on=['permno','jdate'])
# crsp['dlret']=crsp['dlret'].fillna(0)
# crsp['ret']=crsp['ret'].fillna(0)
# crsp['retadj']=(1+crsp['ret'])*(1+crsp['dlret'])-1
# crsp['me']=crsp['prc'].abs()*crsp['shrout'] # calculate market equity
# crsp=crsp.drop(['dlret','dlstdt','prc','shrout'], axis=1)
# crsp=crsp.sort_values(by=['jdate','permco','me'])
crsp <- crsp_m %>% left_join(dlret, by=c("permno", "jdate"))
crsp <- crsp %>%
  mutate(dlret = replace_na(dlret, 0)) %>% 
  mutate(ret = replace_na(ret, 0)) %>% 
  mutate(retadj = (1+ret)*(1+dlret)-1) %>% 
  mutate(me = abs(prc)*shrout) %>% 
  select(-dlret, -dlstdt, -prc, -shrout) %>% 
  arrange(jdate, permco, me)

# ## Load the python data to confront
# crsp_py <- readRDS("0_data/wrds/py_crsp.rds")
# crsp
# crsp_py %>% arrange(jdate, permco, me)
# ## > crsp
# ## # A tibble: 3,841,914 x 11
# ## permno permco date       shrcd exchcd siccd      ret     retx jdate        retadj     me
# ## <int>  <int> <date>     <int>  <int> <int>    <dbl>    <dbl> <date>        <dbl>  <dbl>
# ##   1  28820      6 1966-01-31    10      2  3443  0.0126   0.0126  1966-01-31  0.0126  24374.
# ##   2  29161     64 1966-01-31    10      2  3662 -0.175   -0.175   1966-01-31 -0.175    9075 
# ##
# ## > crsp_py %>% arrange(jdate, permco, me)
# ## # A tibble: 2,530,079 x 11
# ## permno permco date       shrcd exchcd siccd      ret     retx jdate        retadj     me
# ## <dbl>  <dbl> <date>     <dbl>  <dbl> <dbl>    <dbl>    <dbl> <date>        <dbl>  <dbl>
# ##   1  28820      6 1966-01-31    10      2  3443  0.0126   0.0126  1966-01-31  0.0126  24374.
# ##   2  29161     64 1966-01-31    10      2  3662 -0.175   -0.175   1966-01-31 -0.175    9075 

## Aggregate Market Cap --------------------------------------------------------
# # Python code to replicate
# # sum of me across different permno belonging to same permco a given date
# crsp_summe = crsp.groupby(['jdate','permco'])['me'].sum().reset_index()
# # largest mktcap within a permco/date
# crsp_maxme = crsp.groupby(['jdate','permco'])['me'].max().reset_index()
# # join by jdate/maxme to find the permno
# crsp1=pd.merge(crsp, crsp_maxme, how='inner', on=['jdate','permco','me'])
# # drop me column and replace with the sum me
# crsp1=crsp1.drop(['me'], axis=1)
# # join with sum of me to get the correct market cap info
# crsp2=pd.merge(crsp1, crsp_summe, how='inner', on=['jdate','permco'])
# # sort by permno and date and also drop duplicates
# crsp2=crsp2.sort_values(by=['permno','jdate']).drop_duplicates()
crsp_summe <- crsp %>% group_by(jdate, permco) %>% summarise(me = sum(me))
crsp_maxme <- crsp %>% group_by(jdate, permco) %>% summarise(me = max(me))
crsp1 <- crsp %>% inner_join(crsp_maxme, by=c("jdate", "permco", "me"))
crsp1 <- crsp1 %>% select(-me)
crsp2 <- crsp1 %>% inner_join(crsp_summe, by=c("jdate", "permco"))
crsp2 <- crsp2 %>% arrange(permno, jdate)
crsp2 <- crsp2 %>% distinct

# # keep December market cap
# crsp2['year']=crsp2['jdate'].dt.year
# crsp2['month']=crsp2['jdate'].dt.month
crsp2 <- crsp2 %>%
  mutate(year = year(jdate)) %>%
  mutate(month = month(jdate))
# decme=crsp2[crsp2['month']==12]
# decme=decme[['permno','date','jdate','me','year']].rename(columns={'me':'dec_me'})
decme <- crsp2 %>% filter(month==12)
decme <- decme %>% select(permno, date, jdate, dec_me=me, year)

# ### July to June dates
# crsp2['ffdate']=crsp2['jdate']+MonthEnd(-6)
# crsp2['ffyear']=crsp2['ffdate'].dt.year
# crsp2['ffmonth']=crsp2['ffdate'].dt.month
# crsp2['1+retx']=1+crsp2['retx']
# crsp2=crsp2.sort_values(by=['permno','date'])
library(lubridate)
crsp2 %>%
  mutate(ffdate = jdate %m-% months(6)) %>% 
  mutate(ffyear = year(ffdate)) %>% 
  mutate(ffmonth = month(ffdate)) %>% 
  mutate(OnePlusRetx=1+retx) %>% 
  arrange(permno, date) -> crsp2

# # cumret by stock
# crsp2['cumretx']=crsp2.groupby(['permno','ffyear'])['1+retx'].cumprod()
crsp2 <- crsp2 %>%
  mutate(retx1 = replace_na(OnePlusRetx, 1)) %>% 
  group_by(permno, ffyear) %>%
  mutate(cumretx = cumprod(retx1)) %>% ungroup

setDT(crsp2)
crsp2[cumretx==1 & is.na(OnePlusRetx), cumretx := NA]
crsp2 %>% as_tibble -> crsp2

# # lag cumret
# crsp2['lcumretx']=crsp2.groupby(['permno'])['cumretx'].shift(1)
crsp2 %>% group_by(permno) %>% 
  mutate(lcumretx = dplyr::lag(cumretx)) %>% ungroup -> crsp2

# # lag market cap
# crsp2['lme']=crsp2.groupby(['permno'])['me'].shift(1)
crsp2 %>% group_by(permno) %>% 
  mutate(lme = dplyr::lag(me)) %>% ungroup -> crsp2

# # if first permno then use me/(1+retx) to replace the missing value
# crsp2['count']=crsp2.groupby(['permno']).cumcount()
# crsp2['lme']=np.where(crsp2['count']==0, crsp2['me']/crsp2['1+retx'], crsp2['lme'])
crsp2 %>% mutate(temp1 = 1) %>% group_by(permno) %>% 
  mutate(count = cumsum(temp1)) %>% select(-temp1) -> crsp2
setDT(crsp2)
crsp2[count==1] ; crsp2[count==1, lme := me/retx1]
crsp2 <- as_tibble(crsp2)

# # baseline me
# mebase=crsp2[crsp2['ffmonth']==1][['permno','ffyear', 'lme']].rename(columns={'lme':'mebase'})
mebase <- crsp2 %>% filter(ffmonth==1) %>% select(permno, ffyear, mebase=lme)

# # merge result back together
# crsp3=pd.merge(crsp2, mebase, how='left', on=['permno','ffyear'])
# crsp3['wt']=np.where(crsp3['ffmonth']==1, crsp3['lme'], crsp3['mebase']*crsp3['lcumretx'])
crsp3 <- crsp2 %>% left_join(mebase, by=c("permno", "ffyear"))
setDT(crsp3)
crsp3[ffmonth==1, wt := lme]
crsp3[ffmonth!=1, wt := mebase*lcumretx]

# decme['year']=decme['year']+1
# decme=decme[['permno','year','dec_me']]
decme <- decme %>% mutate(year = year+1) %>% select(permno, year, dec_me)

# # Info as of June
# crsp3_jun = crsp3[crsp3['month']==6]
# 
# crsp_jun = pd.merge(crsp3_jun, decme, how='inner', on=['permno','year'])
# crsp_jun=crsp_jun[['permno','date', 'jdate', 'shrcd','exchcd','retadj','me','wt','cumretx','mebase','lme','dec_me', 'siccd']]
# crsp_jun=crsp_jun.sort_values(by=['permno','jdate']).drop_duplicates()
crsp3_jun <- crsp3 %>% filter(month==6)
crsp_jun <- crsp3_jun %>% inner_join(decme, by=c("permno", "year")) %>% 
  select(permno,date, jdate, shrcd,exchcd,retadj,me,wt,cumretx,mebase,lme,dec_me, siccd) %>% 
  arrange(permno, jdate) %>% 
  distinct

## Organizar base crsp para merge por (permno, year, month)
crsp_m$permno <- as.integer(crsp_m$permno)
crsp_m$date   <- as.Date(crsp_m$date)
setDT(crsp_m)
crsp_m[, monthlink := ( ceiling_date(ymd(date), 'month') %m-% days(1) ) ]
crsp_m %>% as_tibble %>% select(permno, permno, date, monthlink, everything())

crsp_m[is.na(ret),] <- 0
J = 12 # Formation Period Length: J can be between 3 to 12 months
crsp_m[, logret := log(1+ret)]
crsp_m[, cumret := Reduce(`+`, shift(ret, 0:J)), by=permno]
crsp_m[, cumret := cumret-1]

crsp_m <- crsp_m %>% as_tibble %>% select(permno, permno, date, monthlink, everything())

crsp_m <- crsp_m %>%
  left_join(dlret, by=c("permno", "jdate")) %>%
  mutate(dlret = replace_na(dlret, 0)) %>% 
  mutate(ret = replace_na(ret, 0)) %>% 
  mutate(retadj = (1+ret)*(1+dlret)-1) %>% 
  mutate(me = abs(prc)*shrout) %>% 
  select(-dlret, -dlstdt, -prc, -shrout) %>% 
  arrange(jdate, permco, me)


# >> Exclude financial and utility firms --------------------------------------
# Following DeAngelo et al. (2006); Huang and Ritter (2009) and
# Chang et al. (2014), financial (SIC 6000- 6999) and utility (SIC 4900-4949)
# industries are excluded from the sample because the former have relatively
# low physical capital investment, while the latter are under government
# regulation.

# Financial Firms (SIC between 6000 and 6999)
comp_a2 <- comp_a %>% filter( !(sich  %between% c(6000, 6999)) )
crsp_m2 <- crsp_m %>% filter( !(siccd %between% c(6000, 6999)) )

# Utility Firms (SIC between 6000 and 6999)
comp_a2 <- comp_a2 %>% filter( !(sich  %between% c(4900, 4949)) )
crsp_m2 <- crsp_m2 %>% filter( !(siccd %between% c(4900, 4949)) )

# ## To exclude using comp_names or crsp_names database
# comp_names <- readRDS("0_data/wrds/raw_comp_names.rds")
# comp_names %>%
#   filter( !(sic  %between% c(6000, 6999)) ) %>% # Exclude financial firms
#   filter( !(sic  %between% c(4900, 4949)) ) %>% # Exclude utility Firms
#   select(gvkey)
# 
# crsp_names <- readRDS("0_data/wrds/raw_crsp_names.rds")
# crsp_names %>%
#   filter( !(siccd  %between% c(6000, 6999)) ) %>% # Exclude financial firms
#   filter( !(siccd  %between% c(4900, 4949)) ) %>% # Exclude utility Firms
#   select(permco)

## >> Save data ----------------------------------------------------------------
saveRDS(comp_a2, "2_pipeline/2_out/1a_cleaned_comp_a.rds")
saveRDS(crsp_m2, "2_pipeline/2_out/1a_cleaned_crsp_m.rds")
saveRDS(comp_q,  "2_pipeline/2_out/1a_cleaned_comp_q.rds")