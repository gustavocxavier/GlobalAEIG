# Chapter 3 - Mature Firms, Aggregate Investment Plans and Market Returns ---- #
#
#
#

## Working directory -----------------------------------------------------------

## Check if the current working directory is the project folder
getwd()

## If not, set the right working directory, which your project folder is located
# setwd("C:/") ## <--- Change here, make sure to avoid using single backslashes (i.e. \)

## Language --------------------------------------------------------------------
# Set language in English to resolve issues based on error messages
Sys.setenv(LANG = "en")

## Set  up pipeline folder if missing ------------------------------------------
## The code below will automatically create a pipeline folder for this code file
## if it does not exist.

if (!(dir.exists(file.path(getwd(), "2_pipeline")))) {
  dir.create(file.path(getwd(), "2_pipeline"))
}

pipeline <- file.path(getwd(), "2_pipeline")

for (folder in c('0_tmp', '1_store', '2_out')){
  if (!(dir.exists(file.path(pipeline, folder)))) {
    dir.create(file.path(pipeline, folder))
  }
}
rm(pipeline, folder)

## Imports ---------------------------------------------------------------------
## Install packages by using groundhog to address reproducible research
## Se more in http://datacolada.org/95 or https://groundhogr.com/

## All the library imports go here
pkgs = c('lubridate','data.table', 'dplyr')

library(groundhog)
groundhog_day = "2020-12-31" # Last date when script runs as expected
groundhog.library(pkgs, groundhog_day)

# library(tidyverse)
# library(reticulate)

# Set sample date range --------------------------------------------------------
begdate = '01/01/1966'
enddate = '12/31/2019'

## Functions -------------------------------------------------------------------
source('1_code/functions.R')

## Source next steps -----------------------------------------------------------
## If you prefer, you can source each script by this one

# Organize raw data and compute some variables
source("1_code/1a_compustatCRSP.R", echo = T)

# Merge CRSP and Compustat. Compute the variables that need both databases.
source("1_code/1b_CCM_block.R", echo = T)

# Compute Life Cycle Proxies
source("1_code/2a_lifeCycleProxiesFaff.R", echo = T)

# Compute Others Life Cycle Proxies
source("1_code/2b_lifeCycleProxiesAdjAgeCRSP.R", echo = T)

# H1b: LC proxies are relevant
source("1_code/3a_EIG_ElasticNet.R", echo = T)

# H1a
source("1_code/3b_panelDataAnalysis.R", echo = T)

# H1c
source("1_code/3c_portfolioSortAnalysis.R", echo = T)
