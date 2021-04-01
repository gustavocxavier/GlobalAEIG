library(data.table)
library(lubridate)
library(tidyverse)

## > CCM Block (CCM = CRSP Compustat Merged) ## ################################
comp_a <- readRDS(file = "2_pipeline/2_out/1a_cleaned_comp_a.rds")
comp_q <- readRDS(file = "2_pipeline/2_out/1a_cleaned_comp_q.rds")
crsp_m <- readRDS(file = "2_pipeline/2_out/1a_cleaned_crsp_m.rds")

ccmlink <- readRDS(file = "0_data/wrds/raw_ccmlink.rds")
ccmlink <- as_tibble(ccmlink)
ccmlink

## if linkenddt is missing then set to today date
ccmlink <- ccmlink %>% mutate(linkenddt = replace_na(linkenddt, today()) )
# python: ccm['linkenddt']=ccm['linkenddt'].fillna(pd.to_datetime('today'))

ccm_a <- comp_a %>% left_join(ccmlink, by = "gvkey")
# in python: ccm1['yearend']=ccm1['datadate']+YearEnd(0)
# in python: ccm1['jdate']=ccm1['datadate']+MonthEnd(4)
# in python: ccm1['datadate']=ccm1['datadate']+MonthEnd(0)

## set link date bounds
ccm_a <- ccm_a %>% filter( (datadate >= linkdt) & (datadate <= linkenddt) )
# in python: ccm2=ccm1[(ccm1['jdate']>=ccm1['linkdt'])&(ccm1['jdate']<=ccm1['linkenddt'])] # <-- Em Python

ccm_a %>% select(gvkey, permno, datadate, at)

## change variable format to int
# # ccm2['permno'] = ccm2['permno'].astype(int) # <-- Em Python
ccm_a$permno <- as.integer(ccm_a$permno)
# ccm_a %>% select( -(3:25))

## Merge CRSP and Annual Compustat to compute Tobin's Q -----------------------

crsp2 <- crsp_m %>%
  select(permno, monthlink, date, everything()) %>% 
  select(-permco, -shrcd, -exchcd,
         -siccd, -ret, -retx, -jdate, -retadj)

ccm_a2 <- ccm_a %>%
  select(permno, gvkey, monthlink, fiscaldate = datadate,
         ia, d1_ia, IGc1,
         d0_ia, IGc0, SG, CFG, CF, PG, EG, Id,
         cop, at, dltt, dlc) %>%
  # Importar ME referente fim do ano fiscal para poder calulcar Q de Tobin
  left_join(select(crsp2, permno, fiscaldate=monthlink, meTobin = me),
            by = c("permno", "fiscaldate")) %>% 
  # Colocar na mesma escala dos valores contabeis
  mutate(meTobin = meTobin/1000)
saveRDS(ccm_a2, "2_pipeline/2_out/1b_ccm_a_me.rds")

## Adicionando valor de mercado ? ccm_a
me <- select(ccm_a2, gvkey, permno, monthlink, me = meTobin)
ccm_a <- ccm_a %>% left_join(me, by = c("gvkey", "permno", "monthlink"))

ccm_a %>% select(gvkey) %>% unique

## Calcular Tobin's q ---------------------------------------------------------
# Tobin's q is the market equity (from CRSP) plus long-term debt (Compustat
# annual item DLTT) and short-term debt (item DLC) scaled by book assets (item
# AT), all from the most recent fiscal year ending at least four months ago                                                                                                                                        AT), all from the most recent fiscal year ending at least four months ago
# For firms with multiple share classes, we merge the market equity for all
# classes.
ccm_a2 <- ccm_a2 %>% mutate( q = log((meTobin+dltt+dlc)/at) )

ccm_a %>% left_join(select(ccm_a2, gvkey, monthlink, meTobin, q),
                    by=c("gvkey", "monthlink")) -> ccm_a

ccm_a2 <- ccm_a2 %>% select(-meTobin, -dltt, -dlc, -at)

## Compute Ie - Part 2/2 -------------------------------------------------------
# Ie is equal to 1 if a firm increases its equity by more than 5% and 0
# otherwise, where new share issues is defined as the sale of common and
# preferred stock (Compustat data item SSTK) divided by lag market equity after
# 1971, and the growth rate of the split-adjusted shares (Compustat data items
# CSHO ? AJEX) before 1971 due to the data availability of SSTK
ccm_a <- ccm_a %>%
  arrange(gvkey, datadate) %>% 
  group_by(gvkey) %>% 
  mutate( equity_increase = new_shares/dplyr::lag(me) )
setDT(ccm_a)
ccm_a[equity_increase > 0.05, Ie := 1]
ccm_a[equity_increase <= 0.05, Ie := 0]
ccm_a <- as_tibble(ccm_a)

ccm_a %>% select(gvkey:datadate, new_shares, me, equity_increase, Ie)

ccm_a2 <- ccm_a2 %>%
  left_join(ccm_a %>% select(gvkey, fiscaldate=datadate, Ie),
            by = c("gvkey", "fiscaldate"))

## Merge CRSP and Quarter Compustat -------------------------------------------

ccm_q <- comp_q %>% left_join(ccmlink, by = "gvkey")
ccm_q <- ccm_q %>% filter( (datadate >= linkdt) & (datadate <= linkenddt) )

ccm_q$permno <- as.integer(ccm_q$permno)

ccm_q <- ccm_q %>%
  select(gvkey, permno, monthlink, datadate, everything()) %>% 
  select(-linktype, -linkprim, -linkdt, -linkenddt, -rdq)

ccm_a <- ccm_a %>%
  select(gvkey, permno, monthlink, datadate, everything()) %>% 
  select(-linktype, -linkprim, -linkdt, -linkenddt)

## Merge monthly CRSP with both comp_a and com_q -------------------------------

crsp3 <- crsp2 %>% 
  left_join(ccm_a2, by = c("permno", "monthlink")) %>% 
  arrange(permno, monthlink) %>% 
  group_by(permno) %>% 
  fill(gvkey,      .direction = c("down")) %>% 
  fill(fiscaldate, .direction = c("down")) %>% 
  fill(ia,         .direction = c("down")) %>% # <-- nao coloquei no select
  fill(d1_ia,      .direction = c("down")) %>%
  fill(IGc1,     .direction = c("down")) %>%
  fill(d0_ia,      .direction = c("down")) %>%
  fill(IGc0,     .direction = c("down")) %>%
  fill(cop,        .direction = c("down")) %>% 
  fill(q,          .direction = c("down")) %>% 
  
  mutate(Ret = cumret) %>% 
  fill(SG , .direction = c("down")) %>% 
  fill(CFG, .direction = c("down")) %>% 
  fill(CF , .direction = c("down")) %>% 
  fill(PG , .direction = c("down")) %>% 
  fill(EG , .direction = c("down")) %>% 
  fill(Ie , .direction = c("down")) %>% 
  fill(Id , .direction = c("down")) %>%
  
  drop_na %>% 
  select(permno, gvkey, date, monthlink, fiscaldate,
         d1_ia, IGc1,
         d0_ia, IGc0, q, Ret, cop, SG, CFG, CF, PG, EG, Ie, Id,
         me)
# crsp3 %>% filter(is.na(fiscaldate)) %>% as.data.table
# TODO Why crsp3 start from 1972-10-31??? Probably the drop_na

# crsp2 %>% filter(permno == 10001) %>% data.frame %>% head(25)
# crsp3 %>% filter(permno == 10001) %>% data.frame %>% head(25)
# ccm_a2 %>% filter(permno == 10001) %>% data.frame %>% head

ccm_q2 <- ccm_q %>% 
  select(permno, monthlink, rdq2, dROE) %>% drop_na

crsp4 <- crsp3 %>% 
  left_join(ccm_q2, by = c("permno", "monthlink")) %>% 
  group_by(permno) %>% 
  fill(rdq2, .direction = c("down")) %>% 
  fill(dROE, .direction = c("down")) %>%
  drop_na(gvkey) %>% 
  # Missing dRoe values are set to zero in the cross-sectional forecasting
  # regressions.
  mutate( dROE = if_else(is.na(dROE), 0, dROE)) %>% 
  select(permno, gvkey, date, monthlink, fiscaldate, rdq=rdq2, everything())

crsp4 %>% filter(permno == 10005)

ccm_a <- ccm_a %>%
  left_join(select(crsp4, permno, monthlink, dROE), by = c("permno", "monthlink"))
 
crsp4
ccm_a
ccm_q


rm(ccmlink, comp_a, comp_q, crsp_m, crsp2, crsp3, ccm_a2, ccm_q2, me)

saveRDS(ccm_a, "2_pipeline/2_out/1b_ccm_a.rds")
saveRDS(ccm_q, "2_pipeline/2_out/1b_ccm_q.rds")
saveRDS(crsp4, "2_pipeline/2_out/1b_crsp4.rds")

rm(ccm_a, ccm_q, crsp4) ; gc()

# ccm_a <- readRDS("2_pipeline/2_out/1b_ccm_a.rds")
# ccm_q <- readRDS("2_pipeline/2_out/1b_ccm_q.rds")
# crsp4 <- readRDS("2_pipeline/2_out/1b_crsp4.rds")

# ccm_a %>% select(ia=ia, capx=IGc0) %>% na.omit %>%
#   filter(is.finite(capx) & is.finite(ia)) %>%
#   data.frame %>% cor
