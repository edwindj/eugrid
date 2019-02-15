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
  st_read("http://cartomap.github.io/nl/wgs84/gemeente_2017.geojson") %>% 
  st_transform(CRS_NL)


# rijksdriehoek coordinates
tm_shape(gemeente_2017) + 
  tm_borders()

# EU coordinates
gemeente_2017 %>% 
  st_transform(CRS_EU) %>% 
  tm_shape() + 
  tm_borders()

st_bbox(gemeente_2017)

grid_rd <- make_grid(gemeente_2017, 1000)
# fill grid cells with gemeente id
gem_rd <- rasterize(gemeente_2017, grid_rd)
plot(gem_rd)
gem_gridded_rd <-
  rasterToPolygons(gem_rd) %>% 
  st_as_sf()


dord <-
  gemeente_2017 %>% 
  dplyr::filter(statnaam %in% c("Dordrecht"))

grid_rd <- make_grid(dord, 1000)
dord_eo <- st_crop(gemeente_2017, extent(grid_rd))
dord_rd <- rasterize(dord_eo, grid_rd)

dord_gridded_rd <- 
  rasterToPolygons(dord_rd) %>% 
  st_as_sf()

plot(dord_gridded_rd)

dord_rdgrid_eu <-st_transform(dord_gridded_rd, CRS_EU)

gemeente_eu <- gemeente_2017 %>% st_transform(CRS_EU) 

dord_eu <-
  gemeente_eu %>% 
  dplyr::filter(statnaam %in% c("Dordrecht"))

grid_eu <- make_grid(dord_eu, 1000)
dord_eu <- st_crop(gemeente_eu, extent(grid_eu)) %>% dplyr::mutate(statcode = droplevels(statcode))
g_dord_eu <- rasterize(dord_eu, grid_eu)

dord_eugrid_eu <- 
  rasterToPolygons(g_dord_eu) %>% 
  st_as_sf()

plot(dord_eugrid_eu)

tm_shape(dord_eu) + tm_fill("statcode") + 
tm_shape(dord_eugrid_eu) + tm_borders("blue") + # eu grid is blauw
  tm_shape(dord_rdgrid_eu) + tm_borders("red") # nl grid is rood
