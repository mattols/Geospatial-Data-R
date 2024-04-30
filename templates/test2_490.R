


d <- dem
d = crop(d, ext(76,77, 35,36))
plot(d)

dagg = lapply(2:10, function(x) aggregate(d, x))
dagg
global(dagg, "mean", na.rm=T)
ev = unlist(lapply(1:length(dagg), function(x) global(dagg[[x]], "mean", na.rm=T)))
sd(ev)

plot(ev)
abline(h=mean(ev), lty=3,col='red')





### KARAKORAM - SNOW


######
# workflow

# datasets
kara_outline <- vect("https://raw.githubusercontent.com/mattols/geospat_data/main/karakoram_outline.geojson")
glaciers_subset <- vect("https://raw.githubusercontent.com/mattols/geospat_data/main/kara_glaciers_subset100.geojson")
dem = rast("data/KarakoramSRTM/SRTM_Karakoram_reproj.tif")

fnn = list.files("./data/modscag_tiles_2014/", full.names = T)
fbn = basename(fnn)

# fun
stk.mosaic = function(ls){
  project(mosaic(ls[[1]], ls[[2]]), "EPSG:4326")
}

rls = lapply(fnn, rast)
mv = lapply(seq(1,(length(rls)-1), 2), function(x) stk.mosaic(rls[x:(x+1)]))
mv
plot(mv[[1]])

mvs = crop(rast(mv), kshp, mask=T)
mvs[mvs>100]=NA

md = mvs[[7]] - mvs[[1]]
plot(md)
rdem = resample(dem, md)
plot(values(md)~values(rdem))
plot(mvs[[c(1,7)]])

bb = as.polygons(ext(rast(fnn[2])))
crs(bb) <- crs(rast(fnn[2]))
plot(project(bb , crs(mvs)), add=T, border='red')
# ave values by date
plot(global(mvs, "mean", na.rm=T)$mean)

m <- c(1000, 3000, 1, # min, max, new value
       3000, 5000, 2,
       5000, 9000, 3)
rcl_mat <- matrix(m, ncol=3, byrow=TRUE) # create matrix
dcl <-  classify(rdem, rcl_mat)
plot(dcl)

z = zonal(md, dcl, fun="mean",na.rm=T)
z


# RESULTS
tile_name = gsub(".*(h.*).006.*$","\\1", fbn) # extract location from name
date_extract = gsub(".*GA.A(.*).h.*$","\\1", fbn) # extract date
date_convert = as.Date(date_extract, "%Y%j")

# table
sca = global(mvs, "mean", na.rm=T)
head(sca)
sca$date = as.Date(gsub(".*GA.A(.*).h.*$","\\1", row.names(sca)), "%Y%j") 
row.names(sca) <- NULL
plot(sca$mean~sca$date, ylim=c(0,100), type="b", main="2014 MODSCAG Snow cover percent")
boxplot(mvs, names = sca$date)


# mvs - snow days
mvs[mvs>100]=NA
msno = mvs >= 20
plot(msno[[1]])

plot(msno[[7]] - msno[[1]], col=c("firebrick","lightblue","green"))

snct = global(msno, "sum", na.rm=T)
head(snct)
snct$date = as.Date(gsub(".*GA.A(.*).h.*$","\\1", row.names(snct)), "%Y%j") 
row.names(snct) <- NULL
snct$
plot(snct$sum~snct$date, type="b", main="2014 MODSCAG Snow cover area")

modsum = sum(msno, na.rm=T)
plot(modsum, col=blues9)
plot(kara_outline,add=T)

plot(rdem)
rcl = c(0,4000, 1,
        4000, 5000, 2,
        5000, 8100, 3)
rclmat = matrix(rcl, byrow = T)
mcl = classify(rdem, rclmat)
plot(mcl)

mez = zonal(msno, mcl, "sum", na.rm=T)
mez
library(tidyr)
df1 = mez %>% rename (ezone = SRTM_Karakoram_reproj) %>% 
  pivot_longer(., cols = !ezone,
                   names_to = "date", values_to = "value") 
head(df1)
library(dplyr)
library(ggplot2)
df1 %>% 
  mutate(snow_km = value*0.5*0.5) %>% 
  mutate(date = as.Date(gsub(".*GA.A(.*).h.*$","\\1", date), "%Y%j")) %>% 
  ggplot(aes(x=date,y = snow_km, col = ezone)) + geom_line() +
  geom_smooth(method="lm")

ezpoly = as.polygons(mcl)
plot(modsum, col=blues9)
plot(ezpoly, border = c("red","green","yellow"), add=T)
lapply(1:3, function(x) plot(mask(modsum, ezpoly[x,]), col=blues9))
