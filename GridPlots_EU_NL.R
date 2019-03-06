library(sf)
library(raster)
library(tmap)
make_grid <- function(map, res=1000){
  ext = extent(map)
  ext[1] <- floor(ext[1]/res) * res
  ext[2] <- ceiling(ext[2]/res) * res
  ext[3] <- floor(ext[3]/res) * res
  ext[4] <- ceiling(ext[4]/res) * res
  raster(map, ext=ext, res = res)
}

CRS_NL <- st_crs(28992) 
CRS_EU <- st_crs(3035)  

gemeente_2017 <- 
  st_read("http://cartomap.github.io/nl/wgs84/gemeente_2017.geojson") %>% # for different year change this
  st_transform(CRS_NL)  

gemcodes <- c(# Zuid Holland
#       "GM0484","GM0491","GM0499","GM0513","GM0534","GM0537","GM0546","GM0547","GM0553","GM0569",
# 		  "GM0575","GM0576","GM0579","GM0608","GM0623","GM0626","GM0627","GM0638","GM0643","GM0644",
# 		  "GM1525","GM1672","GM1884","GM1892","GM1901","GM0503","GM0518","GM0603","GM0629","GM0637",
# 		  "GM1783","GM1842","GM1916","GM1926","GM0482","GM0489","GM0501","GM0502","GM0504","GM0505",
# 		  "GM0511","GM0512","GM0523","GM0530","GM0531","GM0542","GM0545","GM0556","GM0559","GM0568",
# 		  "GM0571","GM0580","GM0584","GM0585","GM0588","GM0590","GM0597","GM0599","GM0606","GM0610",
# 		  "GM0611","GM0612","GM0613","GM0614","GM0617","GM0622","GM0642","GM0689","GM0693","GM0694",
# 		  "GM0707","GM1621",
# 		  # Noord Holland
# 		  "GM0376","GM0381","GM0402","GM0406","GM0417","GM0424","GM0425","GM0457","GM1696","GM0361",
# 		  "GM0365","GM0373","GM0383","GM0388","GM0395","GM0398","GM0399","GM0400","GM0405","GM0416",
# 		  "GM0420","GM0432","GM0441","GM0448","GM0458","GM0476","GM0498","GM0532","GM1598","GM1911",
# 		  "GM0358","GM0362","GM0363","GM0370","GM0375","GM0377","GM0384","GM0385","GM0392","GM0393",
# 		  "GM0394","GM0396","GM0397","GM0415","GM0431","GM0437","GM0439","GM0450","GM0451","GM0453",
# 		  "GM0473","GM0478","GM0479","GM0852","GM0880",
		  # Utrecht
		  "GM0307","GM0308","GM0310","GM0312","GM0313","GM0317","GM0321","GM0353","GM0327","GM0331",
		  "GM0335","GM0356","GM0589","GM0339","GM0340","GM0736","GM0342","GM1904","GM0344","GM1581",
		  "GM0345","GM0620","GM0352","GM0632","GM0351","GM0355")

# gemcodes <- c("GM0505") # Dordrecht

dord <- gemeente_2017 %>%
  dplyr::filter(statcode %in% gemcodes)

gemeente_eu <- gemeente_2017 %>% st_transform(CRS_EU) 

dord_eu <- gemeente_eu %>%
  dplyr::filter(statcode %in% gemcodes)

tm_shape(dord) + tm_borders("black") + tm_fill("MAP_COLORS",palette=terrain.colors(5)) +
tm_shape(dord_eu) + tm_borders("red")

myres <- 5000

# empty grid
grid_rd <- make_grid(dord, myres)

# include environment of dord 
dord_eo <- st_crop(gemeente_2017, extent(grid_rd))

# chop map into 1km pieces
dord_rd <- rasterize(dord_eo, grid_rd)

# and transform the pieces back into a polygon file
dord_gridded_rd <- 
  rasterToPolygons(dord_rd) %>% 
  st_as_sf()

dord_rdgrid_eu <-st_transform(dord_gridded_rd, CRS_EU)

gemeente_eu <- gemeente_2017 %>% st_transform(CRS_EU) 

dord_eu <- gemeente_eu %>%
  dplyr::filter(statcode %in% gemcodes)

grid_eu <- make_grid(dord_eu, myres)
dord_eu <- st_crop(gemeente_eu, extent(grid_eu))

g_dord_eu <- rasterize(dord_eu, grid_eu)

dord_eugrid_eu <- 
  rasterToPolygons(g_dord_eu) %>% 
  st_as_sf()

# EU-grid vs NL-grid

# The NL-grid en EU-grid differ in the following aspects: 
# 
# - they use a different projection (Rijksdriehoekstelsel (28892) vs Lambert Equal Area (3035))
# - the 1km grid origins are aligned in the original projection, so that the boundaries of the 
#   grid cells also have nice rounded coordinates.
# 
# Lets look at a small piece of the Netherlands in the EU projection:

background <- tm_shape(dord_eu) + tm_fill("MAP_COLORS", palette=terrain.colors(n=6)) + 
  tm_borders("white", alpha=0.8)


#fig.cap="EU 1km grid"
background + tm_shape(dord_eugrid_eu) + tm_borders("gray")

#When the NL grid is reprojected in the EU projection, one readily sees that the grids are not nicely aligned.

#fig.cap="NL 1km grid in EU projection"
background + tm_shape(dord_rdgrid_eu) + tm_borders("gray")

#fig.cap="EU 1km grid (blue) vs NL 1km grid (red) in EU projection"
background + 
  tm_shape(dord_eo) + tm_borders("blue") +
  tm_shape(dord_eu) + tm_borders("red")

