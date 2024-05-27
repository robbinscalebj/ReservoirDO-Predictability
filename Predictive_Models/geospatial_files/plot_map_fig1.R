library(tidyverse)
library(tigris)
library(terra)
library(tmap)
library(tmaptools)
library(sf)
library(here)


here::i_am("Predictive_Models/geospatial_files/plot_map_fig1.R")

# Base state polygons
states <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")|>
  filter(STUSPS %in% c("AR", "TX"))

ar <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")|>
  filter(STUSPS %in% c("AR"))

tx <- tigris::states(cb = TRUE, resolution = "20m", class = "sf")|>
  filter(STUSPS %in% c("TX"))

us <- ggmap::map_data()

#read data
#sampling locations
prof_coords <- read_csv(here("Predictive_Models/geospatial_files/profile_coordinates.csv"))

#convert points to sf
all_coords <- st_as_sf(prof_coords, coords = c("longitude", "latitude"), crs = 4326)
em_coords <- st_as_sf(prof_coords|>filter(reservoir == "em"),coords = c("longitude", "latitude"), crs = 4326)
fay_coords <- st_as_sf(prof_coords|>filter(reservoir == "fay"),coords = c("longitude", "latitude"), crs = 4326)
rc_coords <- st_as_sf(prof_coords|>filter(reservoir == "rc"),coords = c("longitude", "latitude"), crs = 4326)
maum_coords <- st_as_sf(prof_coords|>filter(reservoir == "maum"),coords = c("longitude", "latitude"), crs = 4326)


#polygons

fay_poly <- sf::st_read("Predictive_Models/geospatial_files/fay_NHD_H_11110103_HU8_GDB.gdb", layer = "NHDWaterbody")|>
  filter(gnis_name == "Lake Fayetteville")|>st_transform(src = 4269, crs = 4326)

maum_poly <- sf::st_read("Predictive_Models/geospatial_files/maum_NHD_H_11110207_HU8_GDB.gdb", layer = "NHDWaterbody")|>
  filter(gnis_name == "Lake Maumelle")|>st_transform(src = 4269, crs = 4326)

rc_poly <- sf::st_read("Predictive_Models/geospatial_files/rc_shapefiles/RC18_Lake.shp")|>st_transform(src = 2276, crs = 4326)

em_poly <- sf::st_read("Predictive_Models/geospatial_files/em_shapefiles/bndy_Lake.shp")|>st_transform(src = 2276, crs = 4326)

all_res <- bind_rows(fay_poly, maum_poly, rc_poly, em_poly)

#
em_map<-tm_shape(em_poly)+
  tm_polygons(col = "blue")+
  tm_scale_bar(text.size = 1, breaks = c(0,2,4), position = c("right", "top"))+
  tm_layout()+
  tm_shape(em_coords)+
  tm_symbols(size = 3, col = "orange")
tmap_save(em_map, here("Predictive_Models/figures/em_map.png"))

fay_map <- tm_shape(fay_poly)+
  tm_polygons(col = "blue")+
  tm_scale_bar(text.size = 1, breaks = c(0,0.5,1),position = c("left", "top"))+
  tm_shape(fay_coords)+
  tm_symbols(size = 3, col = "orange")+
  tm_layout(bg.color = "transparent")
tmap_save(fay_map, here("Predictive_Models/figures/fay_map.png"))

rc_map<-tm_shape(rc_poly, is.main = TRUE)+
  tm_polygons(col = "blue")+
  tm_scale_bar(text.size = 1.5, breaks = c(0,5,10), position = c("right", "top"))+
  tm_shape(rc_coords)+
  tm_symbols(size = 3, col = "orange")+
  tm_style("white")
tmap_save(rc_map, here("Predictive_Models/figures/rc_map.png"))

maum_map <-tm_shape(maum_poly, is.main = TRUE)+
  tm_polygons(col = "blue")+
  tm_scale_bar(text.size = 1.5, breaks = c(0,2,4), position = c("left", "bottom"))+
  tm_shape(maum_coords)+
  tm_symbols(size = 3, col = "orange")+
  tm_style("white")
tmap_save(maum_map, here("Predictive_Models/figures/maum_map.png"))

states_map<-tm_shape(states, is.main = TRUE)+
  tm_polygons()+
  tm_shape(em_poly)+
  tm_polygons()+
  tm_shape(maum_poly)+
  tm_polygons()+
  tm_shape(rc_poly)+
  tm_polygons()+
  tm_shape(fay_poly)+
  tm_polygons()+
  tm_layout(frame = FALSE)

tmap_save(states_map, here("Predictive_Models/figures/states_map.png"))

norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(states)
fay_ins_dim = norm_dim(fay_poly)
em_ins_dim = norm_dim(em_poly)
maum_ins_dim = norm_dim(maum_poly)
rc_ins_dim = norm_dim(rc_poly)

#set viewports
main_vp = grid::viewport(width = main_dim[1]*1.5, height = main_dim[2]*1.5)
fay_vp = grid::viewport(width = fay_ins_dim[1] * 0.7, height = fay_ins_dim[2] * 0.7,
                  x = 0.775, y = 0.98,
                  just = c("right", "top"))
em_vp = grid::viewport(width = em_ins_dim[1] * 0.65, height = em_ins_dim[2] * 0.65,
                  x = 0.3, y = 0.35,
                  just = c("right", "bottom"))
maum_vp = grid::viewport(width = maum_ins_dim[1] * 0.5, height = maum_ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom"))
rc_vp = grid::viewport(width = rc_ins_dim[1] * 0.5, height = rc_ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom"))

grid::grid.newpage()
print(states_map, vp = main_vp)
grid::pushViewport(main_vp)
print(em_map, vp = em_vp)

states_map
#print(maum_map, vp = grid::viewport(0.7,0.2, width = 0.45, height =0.3))
#print(rc_map, vp = grid::viewport(0.22,0.2, width = 0.55, height =0.3))
#print(em_map, vp = em_vp)
#print(em_map, vp = grid::viewport(0.2, 0.65, width = 10, height =0.7))
print(fay_map, vp = fay_vp)



#create US map with states
us <- spData::world|>
  filter(iso_a2=="US")


txar_in_us <-tm_shape(us, bbox = st_bbox(c(xmin = -124, xmax = -68, ymin = 25.4, ymax = 49.5), crs = 4326)) +
  tm_fill()+
  tm_shape(ar)+
  tm_polygons(col = "grey")+
  tm_shape(tx)+
  tm_polygons(col = "grey")+ 
  tm_shape(all_coords)+
  tm_dots(size = 0.8, col = "orange", border.col = "black", border.lwd = 2)+
  tm_grid(lines = FALSE, labels.size = 1.25)
txar_in_us

tmap_save(txar_in_us, here("Predictive_Models/figures/txar_in_us_map.png"))

em_sampler<-st_as_sf(namc_gen_df|>filter(collection_date > as_date("2015-01-01")), coords = c("longitude", "latitude"), crs = 4326)


tm_shape(alaska, is.main = TRUE, bbox = tmaptools::bb(alaska, xlim = c(-180,-130), ylim = c(50,72)))+ 
  #original bounding box has 180th meridian problem (western Aleutians plot on farrrr east of map)
  tm_polygons()+
  tm_shape(df_sf)+
  tm_symbols(col = "set",  alpha = 0.2)+
  tm_facets(by = c("set"), nrow = 2)


tm_shape(ar, is.main = TRUE)+
  tm_polygons()
  tm_shape(maum_db)+
  tm_polygons()

