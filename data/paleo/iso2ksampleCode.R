## --------------------------------------------------------------------------
##
## Script name: Iso2k guide code
##
## Purpose of script: Filter records from full Iso2k database
##
## Script author: G. Falster
##
## Date updated: 2020-06-10
##
## Email: gfalster@wustl.edu
##
## Citation: Konecky et al. (2020) The Iso2k Database: A global compilation of paleo-d18O and d2H records to aid understanding of Common Era climate
## Database is available for download from https://doi.org/10.25921/57j8-vs18 or http://lipdverse.org/iso2k/current_version/
## --------------------------------------------------------------------------
##
## Notes: 
##
## *current for database version 1.0.0
##   
## *written using R version 4.0.0 
##
## *for easier manipulation of LiPD files, we suggest installing the 'lipdR' package:
## introduction & installation instructions available at https://nickmckay.github.io/LiPD-utilities/r/index.html 
## examples in this script use only base R and a couple of commonly used packages.
##
## Code offered as-is. For questions or feedback on LiPD-utilities code, please contact Nick <nick@nau.edu>, or post an issue on github at https://github.com/nickmckay/lipd-utilities
## --------------------------------------------------------------------------

# =============================================================================
# set display options
# =============================================================================

options(scipen = 10, digits = 4) # run this if you don't like scientific notation in your outputs

# =============================================================================
# load required packages
# =============================================================================

library("magrittr")
library("tidyverse")

# =============================================================================
# load Iso2k database
# =============================================================================

load("iso2k1_0_0.RData") # change this to match the current R serialization.

rm(D, TS) # Remove extraneous objects

# =============================================================================
# look at individual records
# =============================================================================

## first, a couple of ways to search by Iso2k unique identifier, or by site name.

## extract all Iso2k UIDs
TSids <- as.character(sapply(sTS, "[[", "paleoData_iso2kUI"))

## filter chosen record from full record list
whichRecordName <- which(TSids == "MS12CCCH01b")
recordTS <- sTS[whichRecordName]

## extract dataset names
siteNames <- as.character(sapply(sTS, "[[", "geo_siteName"))

## filter datasets containing the desired site name from the full record list
whichSite <- which(grepl("bahamas", siteNames, ignore.case=TRUE))
selectedSiteTS <- sTS[whichSite]

View(selectedSiteTS)

## view site names for the Bahamas records
as.character(sapply(selectedSiteTS, "[[", "geo_siteName")) %>%
  unique() %>%
  sort()

# =============================================================================
# initial filtering of the database, using Level 1 fields
# =============================================================================

## starting with the entire database, filter for records that
  # have water isotope proxy data i.e.  d18O or d2H,
  # have units in per mille, and
  # are flagged as the primary timeseries
  
variableName <- as.character(sapply(sTS, "[[", "paleoData_variableName"))
units <- as.character(sapply(sTS, "[[", "paleoData_units"))
primaryTS <- as.character(sapply(sTS, "[[", "paleoData_iso2kPrimaryTimeseries"))

## create filters for primary records with isotope data in per mille
isd18O <- which(variableName == "d18O" & primaryTS == "TRUE"  & units == "permil")
isd2H <- which(variableName == "d2H" & primaryTS == "TRUE"  & units == "permil")
isIso <- c(isd18O, isd2H)

allIsoTS <- sTS[isIso] # apply filter to the full timeseries

length(allIsoTS) # See how many records are in this filtered subset of the database

# =============================================================================
# additional filtering of the database, using Level 1 or Level 2 fields
# =============================================================================

## from the filtered isotope records, restrict to records where
# the rank 1 interpretation is the isotopic composition of precipitation
# the archive is terrestrial, or
# the inferred material is groundwater or soil water or leaf water

interpretation <- as.character(sapply(allIsoTS, "[[", "isotopeInterpretation1_variableGroup"))
description <- as.character(sapply(allIsoTS, "[[", "paleoData_description"))
inferredMaterial <- as.character(sapply(allIsoTS, "[[", "paleoData_inferredMaterial"))

## check names
unique(description)
unique(inferredMaterial)

## create filters
isPrecipIsotope <- which(interpretation == "P_isotope")
isTerrestrial <- which(inferredMaterial == "lake water" | inferredMaterial == "lagoon water" | 
                         inferredMaterial == "groundwater" | inferredMaterial == "soil water")

isoPIso <- allIsoTS[isPrecipIsotope]
isoTerrestrial <- allIsoTS[isTerrestrial]

## view descriptions for the terrestrial records. How many are there of each?
descriptionTerrestrial <- as.character(sapply(isoTerrestrial, "[[", "paleoData_description"))

table(descriptionTerrestrial) %>%
  as.data.frame() %>%
  mutate(perCent = (Freq/length(isoTerrestrial))*100)

# =============================================================================
# additional filtering of the database, using Level 2 fields
# =============================================================================

## from the filtered isotope records, restrict to records where
# the authors' primary climatic interpretation was based on the amount effect

climInterpretation <- as.character(sapply(allIsoTS, "[[", "climateInterpretation1_variable"))

## check names
unique(climInterpretation)

## create filter
isAmount <- which(grepl("P_", climInterpretation, ignore.case = TRUE) | 
                    grepl("Precipitation", climInterpretation, ignore.case = TRUE) | climInterpretation == "P")

isoAmount <- allIsoTS[isAmount]
 
## how many records were retained? 
length(isoAmount)

## what are the names of the climatic interpretations that were retained?
as.character(sapply(isoAmount, "[[", "climateInterpretation1_variable")) %>%
  unique() %>%
  sort()

# maybe some additional filtering to be done here
