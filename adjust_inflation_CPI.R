## Script to adjust costs for inflation
## written by Anna J. Turbelin, November 14, 2022 
## aturbelin@gmail.com


library(dplyr)
library(ggplot2)
library(devtools)
library(mgcv)
library(ggpubr)
library(readxl)
library(tidyverse)
library(ggpmisc)
install.packages("wbstats")
library(wbstats)
# library(countrycode)
# library(ISOcodes)


options(stringsAsFactors=FALSE)


## function 2.0 to adjust costs for inflation with option to have a 'neutral' fixed year' original CPI column
##' @param costdb Name of database to pull costs from that need to be converted. 
##' @param costcolumn Name of the cost column to use in \code{costdb}
##' @param yearcolumn Name of the year column to use. Specify \code{NULL} when costs have already been adjusted for inflation. 
##' @param year Year of the required cost value / CPI value, YYYY format. For example if you want to standardize costs to 2017 USD values type 2017.
##' @param country_iso3 ISO3 code of the country from which the currency used to standardize costs is. 
##' ISO3 codes are three-letter country codes defined in ISO 3166-1 (International Organization for Standardization (ISO)). 
##' E.g United States of America (USA); France (FRA); Benin (BEN) 
##' @param originalyear Year of the current cost value - use when @param yearcolumn is null because costs have already been standardized to a given year.  
##' @param method Name of the method to use to adjust costs for inflation - currently only CPI available.

adjustInflation <- function(costdb, costcolumn, yearcolumn = NULL, year = "2020", country_iso3 = "USA", originalyear = NULL, method = "CPI"){
  
  if(is.null(costcolumn))
  {
    stop("Add column name of raw costs to adjust for inflation")
  }
  
  if(!(costcolumn %in% colnames(costdb)))
  {
    stop("The 'costcolumn' does not exist in the database, please check spelling.")
  }

  if(is.null(yearcolumn)) {
    
    if(is.null(originalyear))
    {
      stop("Add value to field 'originalyear' (lacks original CPI year) or add value to 'yearcolumn'")
    }
    
    ## load CPI world bank data from wbstats package
    CPI <- wb_data("FP.CPI.TOTL",country = "all") ## get CPI data from world bank data
    CPI <- dplyr::filter(CPI, iso3c == country_iso3) ## filter to only keep only relevant CPI values for given country - here would be USA
    CPI_new <- CPI[which(CPI$date == year & CPI$iso3c == country_iso3, arr.ind=TRUE), which(colnames(CPI)=="FP.CPI.TOTL")] # rename variable this is the CPI value we want so in this case 2020
    CPI_new <- CPI_new[[1]] # get single value
    raw.cost = costdb[which(colnames(costdb)==costcolumn)] # get cost column
    
    CPI.original <- CPI[which(CPI$date == originalyear & CPI$iso3c == country_iso3, arr.ind=TRUE), which(colnames(CPI)=="FP.CPI.TOTL")] # rename variable this is the CPI value we want so in this case 2020
    CPI.original <- CPI.original[[1]] # get single value
    
    adjvalue <- (1+((CPI_new - CPI.original)/CPI.original))*raw.cost
    colnames(adjvalue) <- paste0("cost_", year)
    
    return(df <- data.frame(dplyr::bind_cols(costdb, adjvalue))) #this returns a new data frame - probably best to add to current dataset but not 100% how to do it
    
  }
  else {
    if(!(yearcolumn %in% colnames(costdb)))
    {
      stop("The 'yearcolumn' does not exist in the database, please check spelling.")
    }
    
    ## load CPI world bank data from wbstats package
    CPI <- wb_data("FP.CPI.TOTL",country = "all") ## get CPI data from world bank data
    CPI <- dplyr::filter(CPI, iso3c == country_iso3) ## filter to only keep only relevant CPI values for given country - here would be USA
    CPI_new <- CPI[which(CPI$date == year & CPI$iso3c == country_iso3, arr.ind=TRUE), which(colnames(CPI)=="FP.CPI.TOTL")] # rename variable this is the CPI value we want so in this case 2020
    CPI_new <- CPI_new[[1]] # get single value
    raw.cost = costdb[which(colnames(costdb)==costcolumn)] # get cost column
    CPI.year = costdb[which(colnames(costdb)==yearcolumn)] # get year column
    CPI.year <- left_join(CPI.year, CPI, by=c('Year'='date')) # join year to match CPI value
    CPI.original <- CPI.year[which(colnames(CPI.year)=="FP.CPI.TOTL")] # exxtract CPI value for equation
    adjvalue <- (1+((CPI_new - CPI.original)/CPI.original))*raw.cost
    colnames(adjvalue) <- paste0("cost_", year)
    
    return(df <- data.frame(dplyr::bind_cols(costdb, adjvalue))) #this returns a new data frame - probably best to add to current dataset but not 100% how to do it
  }

  
}

