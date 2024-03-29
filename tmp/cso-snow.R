#
# terrain model
# avalanche forecast

cso <- vect("data/snow/cso-data-March2024.geojson")
head(cso)
dim(cso)

# plot
plot(cso,"depth")
plot(cso,"elevation")
plot(depth~elevation,cso)

# date
# as.POSIXct(cso$timestamp[1], "%Y/%m/%d %H:%M:%OS")
cso$timestamp <- as.Date(cso$timestamp, "%Y/%m/%d")
dt = as.Date(cso$timestamp, "%Y/%m/%d")
plot(depth~dt, cso)


#### BIOclim
library(geodata)
# define output folder for data download
if (!file.exists("data")){dir.create("data")}
outpath = "data"
# download WorldClim global bioclimatic variables
bioclim19 <- worldclim_global('bio', res=10, path=outpath)
cat("Object has",nlyr(bioclim19),"geographic layers") 



# # # # # # # # #
# download country
# https://www.naturalearthdata.com/downloads/

w <- vect("/Users/jessicaforsdick/Downloads/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
plot(w,add=T)
head(w)
writeVector(w, "data/snow/ne_110m_admin_0countries.geojson", filetype = "GeoJSON")


wx <- vect("data/snow/ne_110m_admin_0countries.geojson")
plot(wx, "POP_EST", breaks=20) # interval not great
# Figure out how to best display data?
plot(wx, "POP_RANK")
plot(wx, "GDP_MD")

# TO DO
# Lesson plan for rasters & spat data cont...
# CS 02 including raster and shapefiles
#   forecast - rose recreation









# community snow