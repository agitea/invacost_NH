## Script for visualization and analysis of invacost data vs. 
## natural hazards cost data
## written by Anna J. Turbelin, September 13, 2021 
## First updated 06 April 2022
## Last updated 29 November 2022
## aturbelin@gmail.com


library(tidyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(scales)
library(grid)
library(gridExtra)
library(earth)
library(devtools)
library(mgcv)
library(quantreg)
library(ggpubr)
library(Cairo)
library(readxl)
library(tidyverse)
library(hrbrthemes)
library(ggpmisc)
library(RColorBrewer)
library(cshapes)
library(invacost)
library(magrittr)
library(plyr)
detach(package:plyr)    
library(dplyr)
library(sp)
library(rgdal)
library(raster)

theme_set(theme_bw())

options(stringsAsFactors=FALSE)

rm(list=ls())


################################ load functions ################################
      
source("adjust_inflation_CPI.R")

inject.score <- function(df) {names(df) <- sub(" ", "_", names(df));df} #function to add underscore in column names


################################ upload data ################################

## DIAGNE, Christophe; Leroy, Boris; E. Gozlan, Rodolphe; Vaissière, Anne-Charlotte; Assailly, Claire; Nuninger, Lise; et al. (2020): 
## InvaCost: Economic cost estimates associated with biological invasions worldwide.. figshare. 
## Dataset. https://doi.org/10.6084/m9.figshare.12668570.v5 
data(invacost) #load data from the invacost R package version 1.1-4

################################

## load modified georegions using https://raw.githubusercontent.com/tdwg/wgsrpd/master/109-488-1-ED/2nd%20Edition/TDWG_geo2.pdf
region <- as.data.frame(read_xlsx("data/S2_InvaCost_countries_TDWG_regions.xlsx",
                                  na = c("NA", "#N/A", "#DIV/0!", "#VALEUR!", "Unspecified", "Unknown"),
                                  guess_max = 10000))


invacost <- left_join(invacost, region)

################################

## EM-DAT, CRED / UCLouvain, Brussels, Belgium international disasters database (www.emdat.be)
## data downloaded on 06 April 2022 using the following filters:
## Disaster Classification -> Natural
## Location -> Asia, Africa, Americas, Europe, Oceania
## from 1980 to 2019
## query id x0lcUA
## # of records 13008

## File was modified to include a new classification of the 'Continent' field. 
## The new field is called "TDWG_level1" and is a classification based on the World Geographical Scheme for Recording Plant Distributions (WGSRPD) 
## (https://raw.githubusercontent.com/tdwg/wgsrpd/master/109-488-1-ED/2nd%20Edition/TDWG_geo2.pdf)

disaster <- as.data.frame(read_xlsx("data/S3_emdat_public_2022_04_06_query_uid-x0lcUA.xlsx",
                                    na = c("NA", "#N/A", "#DIV/0!", "#VALEUR!", "Unspecified", "Unknown"), 
                                    skip = 6,
                                    guess_max = 10000))


################################

## NOAA National Centers for Environmental Information (NCEI) U.S. Billion-Dollar Weather and Climate Disasters (2022). 
## https://www.ncei.noaa.gov/access/monitoring/billions/, DOI: 10.25921/stkw-7w73
## downloaded on 06 April 2022
USA <- read.csv("data/S4_time-series-US-1980-2021_unadjusted.csv", header = TRUE, sep = ",", quote = "\"", dec = ".",skip = 2, fill = TRUE)


################################ data filtering ################################

#### InvaCost data filtering and expansion
## remove na cost values
if(any(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)))
{
  invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]
}

## create new column in InvaCost with costs in 2017 USD converted to 2020 USD value (https://data.worldbank.org/indicator/FP.CPI.TOTL)
invacost <- adjustInflation(costdb = invacost, 
                             costcolumn = "Cost_estimate_per_year_2017_USD_exchange_rate", 
                             # yearcolumn = "Applicable_year" , 
                             year = 2020, 
                             country_iso3 = "USA",
                             originalyear = 2017)


## original formula used to convert 2017 USD estimates to 2020 USD values
# invacost$cost_2020 <- (1+((118.6905016-112.411557302308)/112.411557302308))*invacost$Cost_estimate_per_year_2017_USD_exchange_rate 

invacost$Publication_year <- as.numeric(invacost$Publication_year)

## select most up to date high reliability method
invacost$Method_reliability2 <- invacost$Method_reliability
invacost <- within(invacost, Method_reliability2[Method_reliability2 == 'High' & Method_reliability_refined == 'Low'] <- "Low")


## Keep only highly reliable observation costs
invacost <- invacost[which(invacost$Method_reliability2 == "High"), ]
invacost <- invacost[which(invacost$Implementation == "Observed"), ]


## remove missing starting dates and ending dates
if(any(is.na(invacost$Probable_starting_year_adjusted)))
{
  invacost <- invacost[-which(is.na(invacost$Probable_starting_year_adjusted)), ]
}

if(any(is.na(invacost$Probable_ending_year_adjusted)))
{
  invacost <- invacost[-which(is.na(invacost$Probable_ending_year_adjusted)), ]
}

## Expanding the database using invacost R package function
expand <- expandYearlyCosts(invacost,
                                  startcolumn = "Probable_starting_year_adjusted",
                                  endcolumn = "Probable_ending_year_adjusted")
expand <- dplyr::filter(expand, Impact_year <= "2019")
expand <- dplyr::filter(expand, Impact_year >= "1980")



## add group to cost data
expand[grepl("198",expand$Impact_year), "Period"] <- "1980-1999"
expand[grepl("199",expand$Impact_year), "Period"] <- "1980-1999"
expand[grepl("200",expand$Impact_year), "Period"] <- "2000-2019"
expand[grepl("201",expand$Impact_year), "Period"] <- "2000-2019"

## filter InvaCost data to retain damage costs only
expand_sub <- filter(expand, Type_of_cost_merged == "Damage")

## save filtered dataset
# write.csv(expand_sub, "output/S1_InvaCost_NH_filtered_dataset.csv")

################################

#### Invacost USA subset
country_df <- expand_sub[which(expand_sub$Official_country == "United States of America"), ]


################################

#### EM-DAT data filtering
## add underscore to column names
disaster <- inject.score(disaster)

## remove space impact events
disaster <- dplyr::filter(disaster, Disaster_Type != "Impact" ) 
## add group to disasters data
disaster[grepl("198",disaster$Year), "Period"] <- "1980-1999"
disaster[grepl("199",disaster$Year), "Period"] <- "1980-1999"
disaster[grepl("200",disaster$Year), "Period"] <- "2000-2019"
disaster[grepl("201",disaster$Year), "Period"] <- "2000-2019"

disaster <- dplyr::filter(disaster, Period %in% c("1980-1999","2000-2019") ) ## make sure there are no other periods included

# disaster$Total_Damages <- disaster$`Total_Damages, Adjusted ('000 US$)` * 1000 ## convert value back to USD

disaster$Total_Damages <- disaster$`Total_Damages ('000 US$)` * 1000 ## convert value back to USD


## remove na costs 
if(any(is.na(disaster$Total_Damages)))
{
  disaster <- disaster[-which(is.na(disaster$Total_Damages)), ]
}

## remove na years 
if(any(is.na(disaster$Year)))
{
  disaster <- disaster[-which(is.na(disaster$Year)), ]
}

disaster$Year <- as.numeric(disaster$Year)

## adjust for inflation
disaster <- adjustInflation(costdb = disaster, 
                            costcolumn = "Total_Damages", 
                            yearcolumn = "Year" , 
                            year = 2020, 
                            country_iso3 = "USA")



################################

#### NOAA NCEI USA data filtering
USA <- dplyr::select(USA, Year, Drought.Cost, Flooding.Cost, Freeze.Cost,Severe.Storm.Cost, Tropical.Cyclone.Cost, Winter.Storm.Cost,Wildfire.Cost)
USA <- dplyr::filter(USA, Year <= "2019")
USA$Storm <- USA$Severe.Storm.Cost + USA$Tropical.Cyclone.Cost + USA$Winter.Storm.Cost
USA <- gather(USA,"Disaster_Type", "cost", 2:9)
USA <- dplyr::filter(USA, Disaster_Type %in% c("Drought.Cost", "Flooding.Cost", "Freeze.Cost","Wildfire.Cost", "Storm") ) ## make sure there are no other periods included

## adjust costs for inflation
USA <- adjustInflation(costdb = USA, 
                       costcolumn = "cost", 
                       yearcolumn = "Year" , 
                       year = 2020, 
                       country_iso3 = "USA")


## add group to disasters data
USA[grepl("198",USA$Year), "Period"] <- "1980-1999"
USA[grepl("199",USA$Year), "Period"] <- "1980-1999"
USA[grepl("200",USA$Year), "Period"] <- "2000-2019"
USA[grepl("201",USA$Year), "Period"] <- "2000-2019"

################################

rm(list=setdiff(ls(), c("expand_sub", "disaster", "USA", "country_df", "adjustInflation", "inject.score"))) ## remove files from R memory

################################ data analysis ################################

#### FIGURE 2

## Global costs
## Calculate total damage cost of invasions for each 20 year period (1980-1999; 2000-2019)

IAS_global_period <- expand_sub %>% dplyr::group_by(Period) %>% dplyr::summarise(cost = sum(cost_2020))

IAS_global_period$Disaster_Type <- "BiologicalInvasions" ## create new column to merge with natural hazards data

IAS_global_period <- IAS_global_period[,c(3,1,2)] ## reorder columns to merge with natural hazards data

## summarise cost by hazard type and 20 year period (1980-1999; 2000-2019)
NH_global_period <- disaster %>%
  dplyr::group_by(Disaster_Type, Period) %>%
  dplyr::summarise(cost = sum(cost_2020))

## merge cost of biological invasions with cost of disasters
NHIAS_cost <- bind_rows(IAS_global_period, NH_global_period)

NHIAS_cost <- dplyr::filter(NHIAS_cost, Period %in% c("1980-1999","2000-2019"))

## calculate total costs for 1980-2019
IAS_global <- expand_sub %>%
  dplyr::summarise(cost = sum(cost_2020))

IAS_global$Disaster_Type <- "BiologicalInvasions"

NH_global <- disaster %>%
  dplyr::group_by(Disaster_Type) %>%
  dplyr::summarise(cost = sum(cost_2020))

NHIAS_cost_total <- bind_rows(IAS_global,NH_global)

NHIAS_cost_total$Period <- "1980-2019"
NHIAS_cost_total <- NHIAS_cost_total[,c(2,3,1)]

# NHIAS_costs <- bind_rows(NHIAS_cost_total,NHIAS_cost)

## calculate rate of change (ROC) between (1980-1999; 2000-2019)

NHIAS_cost2 <- spread(NHIAS_cost, Period, cost)
NHIAS_cost2$`1980-1999` <- as.numeric(NHIAS_cost2$`1980-1999`)
NHIAS_cost2$`2000-2019` <- as.numeric(NHIAS_cost2$`2000-2019`)
NHIAS_cost2$ROC <- ((NHIAS_cost2$`2000-2019` / NHIAS_cost2$`1980-1999`) - 1) * 100 ## percentage change
NHIAS_cost2$ROC2 <- NHIAS_cost2$`2000-2019` / NHIAS_cost2$`1980-1999` ## multiplier (by how much has the value increased/decreased)
NHIAS_cost2$total <- NHIAS_cost2$`1980-1999` + NHIAS_cost2$`2000-2019`

## save file to drive
write.csv(NHIAS_cost2, "output/NHIAS_ROC_IC4_1_revision.csv")

################################

## Figure 2A - Damage costs of natural hazards and biological invasions globally
## colours for biological invasions were added in adobe illustrator
my.palette <- c("#969696","#525252") # grey

cairo_pdf("output/Fig2a_NHIAS_COST_IC4_1_2022_revision.pdf", width = 9, height = 6)

ggplot(NHIAS_cost, aes(fill=reorder(Period, desc(Period)), y=cost, x= reorder(Disaster_Type, cost))) + 
  geom_bar(position="stack", stat="identity", width=0.9) +
  scale_fill_manual(values = my.palette)+
  coord_flip() +
  theme(legend.position = "none")+
  theme_bw() 

dev.off()

################################

## Figure 2B - Rate of change (ROC) between (1980-1999; 2000-2019) - damage costs of natural hazards and biological invasions globally
my.palette <- c("#C42503","#2f4845","#b1bbba",  "#0d3a40",  "#848a8d",  "#214953",  "#9cc0bd",  "#3a5a69",  "#6b7d7a",  "#597c8b",  "#416a6b",  "#778c97")

cairo_pdf("output/Fig2B_NHIAS_ROC_world_revision.pdf", width = 9, height = 6)

ggplot(NHIAS_cost2, aes(fill=Disaster_Type, y=ROC, x= reorder(Disaster_Type, total))) + 
  geom_bar(position="stack", stat="identity", width=0.9) +
  scale_fill_manual(values = my.palette)+
  coord_flip() +
  theme_bw()  +
  theme(legend.position = "none")+
  NULL

dev.off()

################################

rm(list=setdiff(ls(), c("expand_sub", "disaster", "USA", "country_df", "adjustInflation", "inject.score"))) ## remove files from R memory

################################

## USA costs
## Calculate total damage cost of invasions for each 20 year period (1980-1999; 2000-2019) in the USA

IAS_USA_period <- country_df %>%
  dplyr::group_by(Period) %>%
  dplyr::summarise(cost_2020 = sum(cost_2020))

IAS_USA_period$Disaster_Type <- "BiologicalInvasions"

IAS_USA_period  <- IAS_USA_period[,c(3,1,2)]
IAS_USA_period$cost_2020 <- IAS_USA_period$cost_2020/1000000000 ##divide to get costs in billions as USA extreme events dataset is in billions

totalUSA <- country_df %>% summarise(cost_tt = sum(country_df$cost_2020))

NH_USA_period <- USA %>% dplyr::group_by(Disaster_Type, Period) %>% dplyr::summarise(cost_2020 = sum(cost_2020))

USA_NHIAS <- bind_rows(IAS_USA_period, NH_USA_period)


## calculate rate of change (ROC) of costs between (1980-1999; 2000-2019) in the USA

USA_spread <- spread(USA_NHIAS, Period, cost_2020)
USA_spread$ROC <- ((USA_spread$`2000-2019` / USA_spread$`1980-1999`) - 1) * 100
USA_spread$total <- USA_spread$`1980-1999` + USA_spread$`2000-2019`
USA_spread$ROC2 <- USA_spread$`2000-2019` / USA_spread$`1980-1999`

write.csv(USA_spread, "output/NHIAS_ROC_USA_IC4_1_revision.csv")

################################

## Figure 2C - Damage costs of natural hazards and biological invasions in the USA
## colour for biological invasions was added in adobe illustrator
my.palette <- c("#969696","#525252") # grey

cairo_pdf("output/Fig2c_NHIAS_USACOST_IC4_1_revision.pdf", width = 9, height =6)

ggplot(USA_NHIAS, aes(fill=reorder(Period, desc(Period)), y=cost_2020, x= reorder(Disaster_Type, cost_2020))) + 
  geom_bar(position="stack", stat="identity", width=0.9) +
  scale_fill_manual(values = my.palette)+
  coord_flip() +
  theme(legend.position = "none")+
  theme_bw() 

dev.off()

################################

## Figure 2D - Rate of change between (1980-1999; 2000-2019) - USA

my.palette2 <- c("#C42503","#2f4845","#b1bbba",  "#0d3a40",  "#848a8d",  "#214953")

## colors were edited in adobe illustrator
cairo_pdf("output/Fig2D_NHIAS_ROC_USA_revision.pdf", width = 8, height = 6)

ggplot(USA_spread, aes(fill=Disaster_Type, y=ROC, x= reorder(Disaster_Type, total))) + 
  geom_bar(position="stack", stat="identity", width=0.9) +
  scale_fill_manual(values = my.palette2)+
  coord_flip() +
  theme_bw()  +
  theme(legend.position = "none")+
  NULL

dev.off()

################################

rm(list=setdiff(ls(), c("expand_sub", "disaster", "USA", "country_df", "adjustInflation", "inject.score"))) ## remove files from R memory

################################################################

#### FIGURE 3 - yearly costs through time

## Global costs
## calculate the yearly damage cost of biological invasions globally 
IAS_global_yr <- expand_sub %>%
  dplyr::group_by(Impact_year) %>%
  dplyr::summarise(cost = sum(cost_2020))

IAS_global_yr$Disaster_Type <- "BiologicalInvasions"

IAS_global_yr <- IAS_global_yr[,c(3,1,2)]

names(IAS_global_yr)[names(IAS_global_yr) == 'Impact_year'] <- 'Year'
IAS_global_yr$Year <- as.numeric(IAS_global_yr$Year)

## calculate the yearly damage cost of natural hazards using the EM-DAT data
NH_global_yr <- disaster %>%
  dplyr::group_by(Disaster_Type,Year) %>%
  dplyr::summarise(cost = sum(cost_2020))

NH_global_yr$Year <- as.numeric(NH_global_yr$Year)

NHIAS_cost_yr <- bind_rows(IAS_global_yr,NH_global_yr)

NHIAS_cost_yr <- NHIAS_cost_yr %>% dplyr::group_by(Disaster_Type) %>% dplyr::mutate(Cum_cost = cumsum(cost))

NHIAS_cost_yr <- dplyr::filter(NHIAS_cost_yr, Disaster_Type != "Impact" )
NHIAS_cost_yr <- dplyr::filter(NHIAS_cost_yr, Year < 2020 )

################################

my.palette <- c("#C42503","#2f4845","#b1bbba",  "#0d3a40",  "#848a8d",  "#214953",  "#9cc0bd",  "#3a5a69",  "#6b7d7a",  "#597c8b",  "#416a6b",  "#778c97")

#### Figure 3A Cumulated damage costs globally
cairo_pdf("output/Fig3A_NHIAS_cumulatedcost_revision.pdf", width = 7, height = 2.5)

NHIAS_cost_yr %>%
  ggplot(aes(x=Year, y=Cum_cost, fill = Disaster_Type, col = Disaster_Type)) +
  geom_line() +
  geom_point(size = 1) +
  scale_colour_manual(values = my.palette)+
  scale_fill_manual(values = my.palette)+
  NULL

dev.off()

################################
## USA costs
## calculate the yearly damage cost of biological invasions in the USA

IAS_USA <- country_df %>%
  dplyr::group_by(Impact_year) %>%
  dplyr::summarise(cost = sum(cost_2020))

IAS_USA$Disaster_Type <- "BiologicalInvasions"

IAS_USA <- IAS_USA[,c(3,1,2)]

names(IAS_USA)[names(IAS_USA) == 'Impact_year'] <- 'Year'
IAS_USA$Year <- as.numeric(IAS_USA$Year)

## calculate the yearly damage cost of natural hazards using:
## NOAA National Centers for Environmental Information (NCEI) U.S. Billion-Dollar Weather and Climate Disasters (2022). 
NH_USA <- USA %>%
  dplyr::group_by(Disaster_Type,Year) %>%
  dplyr::summarise(cost = sum(cost_2020))

NH_USA$cost <- NH_USA$cost * 1000000000

NH_USA$Year <- as.numeric(NH_USA$Year)

NHIAS_cost <- bind_rows(IAS_USA,NH_USA)

NHIAS_cost <- NHIAS_cost %>% dplyr::group_by(Disaster_Type) %>% dplyr::mutate(Cum_cost = cumsum(cost))

NHIAS_cost <- dplyr::filter(NHIAS_cost, Year < 2020 )

NHIAS_cost$Disaster_Type2 <- gsub("[.]", "_", NHIAS_cost$Disaster_Type)

################################
## Figure 3B Cumulated damage costs USA

my.palette2 <- c("#C42503","#2f4845","#848a8d",  "#CBDEDC",  "#597c8b",  "#778c97")

cairo_pdf("output/Fig3B_NHIAS_cumulatedcost_USA_revision.pdf", width = 7, height = 2.5)

NHIAS_cost %>%
  ggplot(aes(x=Year, y=Cum_cost, fill = Disaster_Type2, col = Disaster_Type2)) +
  geom_line() +
  geom_point(size = 1) +
  scale_colour_manual(values = my.palette2)+
  scale_fill_manual(values = my.palette2)+
  NULL

dev.off()



################################################################

#### Mixed cost calculation for biological invasions
expand_mixed <- filter(expand,Type_of_cost_merged == "Mixed")
TotalCost_Mixed <- sum(expand_mixed$cost_2020)
expand_management <- filter(expand, Management_type == "Post-invasion_management")
TotalCost_postInv <- sum(expand_management$cost_2020)
additioncalCost <- TotalCost_Mixed + TotalCost_postInv

################################

rm(list=setdiff(ls(), c("expand_sub", "disaster", "USA", "country_df", "adjustInflation", "inject.score"))) ## remove files from R memory

################################################################

#### FIGURE 4
## Regional analysis
## Calculate total damage cost of invasions for each 20 year period (1980-1999; 2000-2019) for each geographical region
## continental scale (adapted TWDG region)

IAS_region_period <- expand_sub %>% dplyr::group_by(TDWG_level1,Period) %>% dplyr::summarise(cost = sum(cost_2020))

IAS_region_period$Disaster_Type <- "BiologicalInvasions" ## create new column to merge with natural hazards data

IAS_region_period <- IAS_region_period[,c(4,1,2,3)] ## reorder columns to merge with natural hazards data

## summarise cost by hazard type and 20 year period by region
names(disaster)[names(disaster) == 'TGWD_region'] <- 'TDWG_level1'

NH_region_period <- disaster %>%
  dplyr::group_by(Disaster_Type,TDWG_level1, Period) %>%
  dplyr::summarise(cost = sum(cost_2020))


## merge cost of biological invasions with cost of disasters
NHIAS_cost <- bind_rows(IAS_region_period,NH_region_period)
NHIAS_cost <- dplyr::filter(NHIAS_cost, TDWG_level1 != "Diverse/Unspecified" ) ## remove diverse/unspecified regions
NHIAS_cost <- dplyr::filter(NHIAS_cost, TDWG_level1 != "Antarctic-Subantarctic" ) ## remove Antarctic-Subantarctic regions as low cost records

## calculate total costs for 1980-2019 
IAS_global <- expand_sub %>%
  dplyr::summarise(total_cost_global = sum(cost_2020))

IAS_global$Disaster_Type <- "BiologicalInvasions"

NH_global <- disaster %>%
  dplyr::group_by(Disaster_Type) %>%
  dplyr::summarise(total_cost_global = sum(cost_2020))

NHIAS_cost_total <- bind_rows(IAS_global,NH_global)

NHIAS_cost <- left_join(NHIAS_cost,NHIAS_cost_total)

#### calculate rank
IAS_regional <- expand_sub %>% dplyr::group_by(TDWG_level1) %>% dplyr::summarise(cost = sum(cost_2020))

IAS_regional$Disaster_Type <- "BiologicalInvasions" ## create new column to merge with natural hazards data

IAS_regional <- IAS_regional[,c(3,1,2)] ## reorder columns to merge with natural hazards data

## summarise cost by hazard type 
NH_regional <- disaster %>%
  dplyr::group_by(Disaster_Type,TDWG_level1) %>%
  dplyr::summarise(cost = sum(cost_2020))

## merge cost of biological invasions with cost of disasters
NHIAS_cost_reg <- bind_rows(IAS_regional,NH_regional)

NHIAS_cost_reg <- dplyr::filter(NHIAS_cost, TDWG_level1 != "Diverse/Unspecified" ) 
NHIAS_cost_reg <- dplyr::filter(NHIAS_cost, TDWG_level1 != "Antarctic-Subantarctic" ) 


NHIAS_cost_reg <- NHIAS_cost_reg %>%
  dplyr::group_by(TDWG_level1) %>%
  dplyr::mutate(rank = row_number(-cost))

write.csv(NHIAS_cost_reg, "output/NHIAS_cost_regions_1980_2019.csv")


################################

## Figure 4 - colour for biological invasions was added in adobe illustrator

my.palette <- c("#969696","#525252") # grey

cairo_pdf("output/Fig4_NHIAS_COST_region_IC4_1_2022_revision.pdf", width = 13, height = 8)

ggplot(NHIAS_cost, aes(fill=reorder(Period, desc(Period)), y=cost, x= reorder(Disaster_Type, total_cost_global))) + 
  geom_bar(position="stack", stat="identity", width=0.9) +
  scale_fill_manual(values = my.palette)+
  coord_flip() +
  facet_wrap(vars(TDWG_level1), scales = "free", nrow = 3) + 
  theme(legend.position = "none")+
  theme_bw() 

dev.off()

################################################################

#### SUPPLEMENTARY FIGURES S1 AND S2

################################

## Figure S1 Calculate ROC of damage cost of invasions for each 20 year period (1980-1999; 2000-2019) for each geographical region
## use NHIAS_cost dataset generated for Figure 4

NHIAS_cost2 <- spread(NHIAS_cost, Period, cost)
NHIAS_cost2$`1980-1999` <- as.numeric(NHIAS_cost2$`1980-1999`)
NHIAS_cost2$`2000-2019` <- as.numeric(NHIAS_cost2$`2000-2019`)
NHIAS_cost2$ROC <- ((NHIAS_cost2$`2000-2019` / NHIAS_cost2$`1980-1999`) - 1) * 100
NHIAS_cost2$ROC2 <- NHIAS_cost2$`2000-2019` / NHIAS_cost2$`1980-1999`
NHIAS_cost2$total <- NHIAS_cost2$`1980-1999` + NHIAS_cost2$`2000-2019`

write.csv(NHIAS_cost2, "output/NHIAS_ROC_IC4_1_regions_revision.csv")


## Figure S1 Rate of change - regional
my.palette <- c("#C42503","#2f4845","#b1bbba",  "#0d3a40",  "#848a8d",  "#214953",  "#9cc0bd",  "#3a5a69",  "#6b7d7a",  "#597c8b",  "#416a6b",  "#778c97")

cairo_pdf("output/FigSupp_NHIAS_ROC_world_region_revision.pdf", width = 9, height = 6)

ggplot(NHIAS_cost2, aes(fill=Disaster_Type, y=ROC, x= reorder(Disaster_Type, total_cost_global))) + 
  geom_bar(position="stack", stat="identity", width=0.9) +
  scale_fill_manual(values = my.palette)+
  coord_flip() +
  theme_bw()  +
  facet_wrap(vars(TDWG_level1), scales = "free", nrow = 3) + 
  theme(legend.position = "none")+
  NULL

dev.off()

################################

rm(list=setdiff(ls(), c("expand_sub", "disaster", "USA", "country_df", "adjustInflation", "inject.score"))) ## remove files from R memory

################################

## Figure S2 Cumulated damage costs by region 
## calculate the yearly damage cost of biological invasions 

IAS_regional_yr <- expand_sub %>%
  dplyr::group_by(TDWG_level1, Impact_year) %>%
  dplyr::summarise(cost = sum(cost_2020))

IAS_regional_yr$Disaster_Type <- "BiologicalInvasions"

IAS_regional_yr<- IAS_regional_yr[,c(4,1,2,3)]

names(IAS_regional_yr)[names(IAS_regional_yr) == 'Impact_year'] <- 'Year'
IAS_regional_yr$Year <- as.numeric(IAS_regional_yr$Year)

## calculate the yearly damage cost of natural hazards using the EM-DAT data
NH_regional_yr <- disaster %>%
  dplyr::group_by(TDWG_level1, Disaster_Type,Year) %>%
  dplyr::summarise(cost = sum(cost_2020))

NH_regional_yr$Year <- as.numeric(NH_regional_yr$Year)

NHIAS_cost <- bind_rows(IAS_regional_yr,NH_regional_yr)

NHIAS_cost <- NHIAS_cost %>% dplyr::group_by(TDWG_level1, Disaster_Type) %>% dplyr::mutate(Cum_cost = cumsum(cost))

NHIAS_cost <- dplyr::filter(NHIAS_cost, Disaster_Type != "Impact" )
NHIAS_cost <- dplyr::filter(NHIAS_cost, Year < 2020 )

NHIAS_cost <- dplyr::filter(NHIAS_cost, TDWG_level1 != "Diverse/Unspecified" ) 
NHIAS_cost <- dplyr::filter(NHIAS_cost, TDWG_level1 != "Antarctic-Subantarctic" ) 


## Figure S2 

my.palette <- c("#C42503","#2f4845","#b1bbba",  "#0d3a40",  "#848a8d",  "#214953",  "#9cc0bd",  "#3a5a69",  "#6b7d7a",  "#597c8b",  "#416a6b",  "#778c97")


cairo_pdf("output/FigSupplement_NHIAS_cumulatedcost_region_revision.pdf", width = 8, height = 10)

NHIAS_cost %>%
  ggplot(aes(x=Year, y=Cum_cost, fill = Disaster_Type, col = Disaster_Type)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = my.palette)+
  scale_fill_manual(values = my.palette)+
  scale_y_continuous(name= "Cumulated damage cost of natural hazards and biological invasions by region - 1980-2019 (USD 2020)",labels = comma, position = "right") +
  facet_grid(vars(TDWG_level1), scales = "free", switch="y") + 
  # theme(strip.text.y.left = element_text(angle = 0))
  NULL

dev.off()



