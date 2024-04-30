




# 

d = rast("/Users/jessicaforsdick/Mattsuff/src/Geospatial-Data-R/data/UT_elev/ASTGTM2_N40W112/ASTGTM2_N40W112_dem.tif")

cc = vect("~/Downloads/cottonwood_canyons_roi.geojson")
lcc = vect("~/Downloads/lcc.geojson")

dem = crop(d, lcc)
dalb = project(dem, units)

# geology units
units = vect("~/Downloads/SaltLakeCity3060/geounits.shp")

u = crop(units, dalb)
plot(u, "UNITNAME")
# geo = project(u, d)

# writeRaster(dalb, "albion_aster_dem.tif")
# writeVector(u, "Albion_geounits.geojson", filetype="GeoJSON")



dim(units)
plot(units, "UNITNAME")




dfg = read.csv("~/Downloads/AS_Chem_Data_ES.csv")
head(dfg)
v = vect(dfg, geom = c("easting", "northing"), crs = "EPSG:26712")
v = v[-1,]
plot(v, "CI")

rv = rast(v)
res(rv) = 30
plot(rv)


library(gstat)
# create an empty gridded raster for interpolation
rv = rast(vect_obj)
res(rv) = 30
# change vector object back into dataframe
d = data.frame(geom(vect_obj)[,c("x", "y")], as.data.frame(vect_obj))
# create idw model and use interpolate to create new interpolated raster
gs <- gstat(formula=plag~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(rv, gs, debug.level=0)
plot(idw, 1)

v2 = st_as_sf(v)
i <- idw(plag~1, v2, rv)



library(gstat)
# create an empty gridded raster for interpolation
rv = rast(v)
res(rv) = 30
gs <- gstat(formula=plag~1, locations=~easting+northing, data=dfg[-1,], nmax=Inf, set=list(idp=2))
idw <- interpolate(rv, gs, debug.level=0)
plot(idw, 1)