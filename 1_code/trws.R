## Load libary -----------------------------------------------------------------
library(RPostgres)
library(data.table)
library(dplyr)

## Set sample date range -------------------------------------------------------
begdate = '01/01/1994'
enddate = '12/31/2019'

## set wrds user ---------------------------------------------------------------
my_wrds_user = 'gxavier'


## >> RETRIEVE WRDS DATA ## ####################################################
## Load data from WRDS Server

## Connect with WRDS Server ----------------------------------------------------
wrdsConnection <- dbConnect(Postgres(),
                            host='wrds-pgdata.wharton.upenn.edu',
                            port=9737,
                            user=my_wrds_user,
                            password=getPass::getPass(),
                            dbname='wrds',
                            sslmode='require')

## Retrieve Monthly Prices -----------------------------------------------------
myQuery <- paste0("SELECT
                          a.year_ as year,
                          a.item18100 as ev,
                          a.item5015 as m01,
                          a.item5020 as m02,
                          a.item5025 as m03,
                          a.item5030 as m04,
                          a.item5035 as m05,
                          a.item5040 as m06,
                          a.item5045 as m07,
                          a.item5050 as m08,
                          a.item5055 as m09,
                          a.item5060 as m10,
                          a.item5065 as m11,
                          a.item5070 as m12,
                          a.item5601 as ticker,
                          a.item6105 as ws_id,
                          a.item9404 as DY
                          a.item8010 as tradingVol,
                          a.item8011 as turnover
                   FROM TRWS.WRDS_WS_STOCK as a
                   LEFT JOIN TRWS.WRDS_WS_COMPANY as b
                        ON a.item6105 = b.item6105
                   WHERE b.item6100='C'
                   AND b.item7021 NOT BETWEEN 6000 AND 6999
                   AND b.item7021 NOT BETWEEN 4900 AND 4949
                   AND a.year_ BETWEEN \'", 1994, "\' AND \'",2019,"\'")

# item9026 as trend52w

t0 <- Sys.time()         ## Registrando inicio da execucao
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

t0 <- Sys.time()         ## Registrando inicio da execucao
ws_stock <- dbFetch(wrdsResult, n = -1)
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

dbClearResult(wrdsResult)

setDT(ws_stock)
glimpse(ws_stock)

## Retrieve Annual data --------------------------------------------------------

myQuery <- paste0("SELECT
                          a.item5350 as date,
                          a.item5601 as ticker,
                          a.item6004 as CUSIP,
                          a.item6008 as ISIN,
                          a.item6105,
                          a.item6038 as IBES_ticker,
                          a.item7220 as equity,
                          a.item7230 as at,
                          a.item7210 as mv,
                          a.item4601 as capex,
                          a.item8301 as roe,
                          a.item4151 as cf,
                          a.item4890 as cff,
                          a.item4870 as cfi,
                          a.item4860 as cfo,
                          a.item8631 as salesgrowth,
                          a.item8621 as assetgrowth,
                          a.item6099 as currency,
                          b.item6026 as nation,
                          b.item6027 as nation_cod,
                          b.item7021 as sic,
                          b.item6100 as type
                   FROM TRWS.WRDS_WS_FUNDA as a
                   LEFT JOIN TRWS.WRDS_WS_COMPANY as b
                        ON a.item6105 = b.item6105
                   WHERE a.freq='A'
                   AND b.item6100='C'
                   AND b.item7021 NOT BETWEEN 6000 AND 6999
                   AND b.item7021 NOT BETWEEN 4900 AND 4949
                   AND a.item5350 between \'", begdate, "\' and \'",enddate,"\'
                   ")

t0 <- Sys.time()         ## Registrando inicio da execucao
wrdsResult <- dbSendQuery(wrdsConnection, myQuery)
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

t0 <- Sys.time()         ## Registrando inicio da execucao
ws_funda <- dbFetch(wrdsResult, n = -1)
Sys.time() - t0 ; rm(t0) ## Tempo total de Execucao

dbClearResult(wrdsResult)

setDT(ws_funda)
glimpse(ws_funda)

dbClearResult(wrdsResult)

ws_funda %>% filter(nation=="BRAZIL" & year(date)==2019) %>% arrange(ticker) %>%
  filter(ticker=="PETR4")

saveRDS(ws_stock, file = "0_data/trws/raw_ws_stock.rds")
saveRDS(ws_funda, file = "0_data/trws/raw_ws_funda.rds")


## Tests -----------------------------------------------------------------------
# wrdsResult <- dbSendQuery(wrdsConnection,
#                           "SELECT *
#                            FROM TRWS.WRDS_WS_FUNDA
#                            LEFT JOIN TRWS.WRDS_WS_COMPANY
#                               ON a.item6105 = b.item6105
#                            where item5601='PETR4'
#                            and item5350='2019-12-31'")
# result <- dbFetch(wrdsResult, n = -1)
# wrdsResult <- dbSendQuery(wrdsConnection,
#                           "SELECT *
#                            FROM TRWS.WRDS_WS_STOCK
#                            where item5601='PETR4'
#                            and year_ between 2019 and 2019")
# result2 <- dbFetch(wrdsResult, n = -1)
# result2$item6100
# "item6100" %in% names(result2)
# t(result) %>%  as.data.table %>% na.omit
# dbClearResult(wrdsResult)
# result2$item6100
# View(result2)
#
