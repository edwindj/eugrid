library(sf)
library(raster)
library(tmap)

CRS_NL <- st_crs(28992) 
CRS_EU <- st_crs(3035)  

gemeente_2017 <- 
  st_read("http://cartomap.github.io/nl/wgs84/gemeente_2017.geojson") %>% st_transform(CRS_NL)  

gemcodes <- c(# Utrecht
  "GM0307","GM0308","GM0310","GM0312","GM0313","GM0317","GM0321","GM0353","GM0327","GM0331",
  "GM0335","GM0356","GM0589","GM0339","GM0340","GM0736","GM0342","GM1904","GM0344","GM1581",
  "GM0345","GM0620","GM0352","GM0632","GM0351","GM0355")

Utrecht <- gemeente_2017 %>%
  dplyr::filter(statcode %in% gemcodes)

gemeente_eu <- 
  st_read("http://cartomap.github.io/nl/wgs84/gemeente_2017.geojson") %>% st_transform(CRS_EU)

Utrecht_eu <- gemeente_eu %>%
  dplyr::filter(statcode %in% gemcodes)

tm_shape(Utrecht) + tm_borders("black") + tm_fill("MAP_COLORS",palette=terrain.colors(10)) +
tm_shape(Utrecht_eu) + tm_borders("red")

