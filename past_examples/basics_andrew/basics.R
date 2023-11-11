#
# Basic geospatial methods in R
#   Olson 2/2/2023
#
# # # # # # # 

# Andrew, you should have the following packages installed in R:
#  terra, raster, sf, rgdal

# You are going to learn a few simple methods for working with these geospatial datasets
# Complete the "challenge" section by next time we meet

library(terra);library(sf);library(rgdal)

# read in raster DEM data
r <- rast("data/rast/N40W112.hgt")
plot(r, main="SRTM DEM (30-meter)")
plot(r, legend=F)
plot(r, legend.only=TRUE, col=topo.colors(100),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2], 25),
                    labels=seq(r.range[1], r.range[2], 25), 
                    cex.axis=0.6),
     legend.args=list(text='Elevation (m)', side=4, font=2, line=2.5, cex=0.8))


wwt <- read_sf("data/shp/WWTPs/WWTPs.shp")
wwt


plot(dem, main="Elevation of Utah County", 
     ylim=c(40,40.4),xlim=c(-112,-111.5),legend=F)
plot(dem, legend.only=TRUE, 
     legend.args=list(text='Elevation (m)', side=4, font=2, line=2.5, cex=0.8))


plot(dem_utm, main="Elevation in UT (meters)",ylim=c(4430000,4480000))
plot(st_geometry(wwt_utm),add=T,color='skyblue4',pch=16)
text(wwt_utm["L"],cex=0.7)

# issues with text
