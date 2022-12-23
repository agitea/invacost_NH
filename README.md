Repository to accompany

# **Biological invasions globally are as costly as natural hazards**

### Anna J. Turbelin, Ross N. Cuthbert, Franz Essl, Phillip J. Haubrock, Anthony Ricciardi, Franck Courchamp

*code written by Anna J Turbelin*

### Repo includes:

Code used to filter and analyse data for the study:
RScript_S1_InvaCost_NH_2022.R

Code to adjust cost for inflation using CPI - *to be used in conjunction with RScript_S1_InvaCost_NH_2022.R*
adjust_inflation_CPI.R

Data used in the analysis (in data folder):
**InvaCost_database_v4.1 - Biological Invasions costs**

Source: DIAGNE, Christophe; Leroy, Boris; E. Gozlan, Rodolphe; Vaissière, Anne-Charlotte; Assailly, Claire; Nuninger, Lise; et al. (2020): InvaCost: Economic cost estimates associated with biological invasions worldwide.. figshare. Dataset. https://doi.org/10.6084/m9.figshare.12668570.v5

**S2 Adjusted continental geographical regions with matching countries from InvaCost**

Regions adjusted using https://raw.githubusercontent.com/tdwg/wgsrpd/master/109-488-1-ED/2nd%20Edition/TDWG_geo2.pdf

**S3 Natural hazards costs (global)**

Source: EM-DAT, CRED / UCLouvain, Brussels, Belgium international disasters database (www.emdat.be) data downloaded on 06 April 2022 using the following filters: Disaster Classification -> Natural Location -> Asia, Africa, Americas, Europe, Oceania from 1980 to 2019 query id x0lcUA number of records 13008

File was modified to include a new classification of the 'Continent' field. The new field is called "TDWG_level1" and is a classification based on the World Geographical Scheme for Recording Plant Distributions (WGSRPD) (https://raw.githubusercontent.com/tdwg/wgsrpd/master/109-488-1-ED/2nd%20Edition/TDWG_geo2.pdf)

**S4 U.S. Billion-Dollar Weather and Climate Disasters (2022) raw costs**

Source: NOAA National Centers for Environmental Information (NCEI) U.S. Billion-Dollar Weather and Climate Disasters (2022). https://www.ncei.noaa.gov/access/monitoring/billions/, DOI: 10.25921/stkw-7w73 downloaded on 06 April 2022


