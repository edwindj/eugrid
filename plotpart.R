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

# NL projection (RijksDriehoek)
gemeente_2017 <- 
  st_read("http://cartomap.github.io/nl/wgs84/gemeente_2017.geojson") %>% st_transform(CRS_NL)  

gemcodes <- c(# Zeeland
  "GM0654","GM0664","GM0677","GM0678","GM0687","GM1695","GM0703","GM1676","GM1714","GM0715",
  "GM0716","GM0717","GM0718")

NLpart <- gemeente_2017 %>%
  dplyr::filter(statcode %in% gemcodes) %>% dplyr::mutate(statcode = droplevels(statcode))

myres <- 4000 # meters

grid_rd <- make_grid(NLpart, myres) %>%
           rasterToPolygons() %>% 
           st_as_sf()

# EU projection (Laea)
gemeente_eu <- 
  st_read("http://cartomap.github.io/nl/wgs84/gemeente_2017.geojson") %>% st_transform(CRS_EU)

NLpart_eu <- gemeente_eu %>%
  dplyr::filter(statcode %in% gemcodes) %>% dplyr::mutate(statcode = droplevels(statcode))

grid_eu <- make_grid(NLpart_eu, myres) %>%
           rasterToPolygons() %>% 
           st_as_sf()

# Combine plots
grid_rd_eu <- st_transform(grid_rd, CRS_EU) # RD grid in EU projection

bbox_eu <- extent(grid_eu)
bbox_rd_eu <- extent(grid_rd_eu)

box <- c(xmin=max(bbox_eu@xmin, bbox_rd_eu@xmin)+5000,ymin=max(bbox_eu@ymin, bbox_rd_eu@ymin)+5000,
         xmax=min(bbox_eu@xmax, bbox_rd_eu@xmax)-5000,ymax=min(bbox_eu@ymax, bbox_rd_eu@ymax)-5000)


NLpart_plot <- st_crop(NLpart_eu, box)
grid_rd_plot <- st_crop(grid_rd_eu, box)
grid_eu_plot <- st_crop(grid_eu, box)


alpha <- 0.5

baselayer <- tm_shape(NLpart_plot) + tm_borders("black",lwd=1,alpha=alpha) + 
  tm_fill(col="statcode", palette=terrain.colors(n=4),legend.show = FALSE,alpha=alpha) 

baselayer + tm_shape(grid_eu_plot) + tm_borders("black") +
  tm_shape(grid_rd_plot) + tm_borders("blue",lty="dotted")
