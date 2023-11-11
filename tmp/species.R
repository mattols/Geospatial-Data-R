#
#
# from Carl's script
# datasets for species distribution

library(spocc)

bison <-occ( query='bison', limit=200)
head(bison$gbif$data$bison)

# only bounds on species

# also
# https://science.ebird.org/en/status-and-trends
# https://cran.r-project.org/web/packages/ebirdst/index.html
library(ebirdst)


# USGS dpecies and predicted habitat
# https://gapanalysis.usgs.gov/apps/species-data-download/
library(sf)
epath = "C:/Users/10504912/OneDrive - Utah Valley University/Projects/src/data/elk/Elk_Cervuselaph_Habitat\\mELK1x_CONUS_HabModel_2001v1.json"
elk <- st_read(epath)

# json jsut metadata?

library(terra)
epath = "C:/Users/10504912/OneDrive - Utah Valley University/Projects/src/data/elk/Elk_Cervuselaph_Habitat/mELK1x_CONUS_HabMap_2001v1/mELK1x_CONUS_HabMap_2001v1.tif"
elk <- rast(epath)
elk

# too large to display 1-summer, 2-winter, -3year round

pdpath = "C:/Users/10504912/OneDrive - Utah Valley University/Projects/src/data/elk/UtahPrairieDog_/mUPDOx_CONUS_HabMap_2001v1/mUPDOx_CONUS_HabMap_2001v1.tif"
pdog <- rast(epath)
pdog
plot(pdog)

# nothing plots

# look more into it
# https://www.usgs.gov/programs/gap-analysis-project/science/species-data-download
