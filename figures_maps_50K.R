
# The purpose of this file is to build on figures_maps.R and to do everything in 1:50K scale. The idea here is that we get a lot more detail but also, because of what was learned in making figure_maps.R, i.e., making maps but more importantly, extracting small chunks from the CANVEC data and then eliminating unneeded features.

# start by making maps in figures-maps.R - just the Canada and NL maps ~ L162
# Ultimately, we want to bring everything together in a plot like what Guillaume has proposed.


# PB ----
bbox_pb <- st_bbox(
  c(xmin = -56.2,
    ymin = 48.83,
    xmax = -55.95,
    ymax = 49.0),
  crs = st_crs(newfoundland_island)
)

# from https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
pb_watercourse_50K <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "watercourse_1",
  wkt_filter = st_as_text(st_as_sfc(bbox_pb))
)

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(pb_watercourse_50K) == st_crs(large_polygons_island)
st_crs(pb_watercourse_50K) == st_crs(newfoundland_island)

# transform crs for nl_watercourse
pb_watercourse_50K <- st_transform(pb_watercourse_50K, st_crs(newfoundland_island))
st_crs(pb_watercourse_50K) == st_crs(newfoundland_island) # now they are T

# intersection
pb_watercourse_50K <- st_intersection(pb_watercourse_50K, newfoundland_island)


mapview(pb_watercourse_50K)
keep_ids <- c(272, 74, 418, 289, 417, 386, 316, 100, 216, 254, 228, 264, 413, 430, 423, 161, 320, 413, 264, 450, 423, 344, 359, 204, 441, 271, 228, 175)
target_rivers <- pb_watercourse_50K %>% slice(keep_ids)

#keep_ids <- c(75)
#stream_diversion <- pb_watercourse_50K %>% slice(keep_ids)

# waterbodies ----
# this is large squares in ocean - not sure what it means
# Did 'waterbody_2_2' which seems right 
pb_waterbody_50K <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "waterbody_2_2",
  wkt_filter = st_as_text(st_as_sfc(bbox_pb)))

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(pb_waterbody_50K) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
pb_waterbody_50K <- st_transform(pb_waterbody_50K, st_crs(newfoundland_island))
st_crs(pb_waterbody_50K) == st_crs(newfoundland_island) #


# intersection
pb_waterbody_50K <- st_intersection(pb_waterbody_50K, newfoundland_island)


mapview(pb_waterbody_50K)
# filter on values identified in mapview
keep_ids <- c(32, 77, 300, 272, 143, 238, 363, 91, 159, 352, 217, 74, 274, 357, 149, 392, 237, 184)
target_bodies <- pb_waterbody_50K %>% slice(keep_ids)


## waypoints ----
df_loc <- read.csv("Data/PB_data/waypoints.csv")
station_way <- c("4", "7b", "6b", "7", "12", "3b", "3", "5A", "5b", "5", "6", "8A", "8", "1", "2")
df_loc <- cbind(df_loc, station_way) 
str(df_loc)

sites <- c(1:8)

df_loc1 <- df_loc |>
  filter(station_way %in% sites)

## plot ----
pam_map <- ggplot() +
  geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
  geom_sf(data = target_rivers, color = "blue", size = 0.3) +
  #geom_sf(data = stream_diversion, color = "red", size = 0.3) +
  geom_sf(data = target_bodies, color = "black", size = 0.3) +
  coord_sf(xlim = c(-56.20, -55.95), ylim = c(48.83, 49.0), expand = FALSE) +
  geom_text(data = study.areas[study.areas$name == "Pamehac Brook", ],
            aes(x = long + 0.1, y = lat, label = name),
            nudge_y = 0.02, size = 5) + 
  geom_text(data = study.areas[study.areas$name == "Pamehac Brook", ],
            aes(x = long - 0.025, y = lat + 0.025, label = "Exploits \n River"),
            nudge_y = 0.02, size = 5) + 
  geom_text(data = study.areas[study.areas$name == "Pamehac Brook", ],
            aes(x = long - 0.029, y = lat - 0.023, label = "Stream \n diversion"),
            nudge_y = 0.02, size = 3) + 
  geom_text(data = study.areas[study.areas$name == "Pamehac Brook", ],
            aes(x = long + 0.03, y = lat - 0.035, label = "Diversion \n structure"),
            nudge_y = 0.02, size = 3) + 
  geom_segment(data = study.areas[study.areas$name == "Pamehac Brook", ],
               aes(x = xstart, y = ystart, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.01, "npc"))
  ) +
  geom_point(data = df_loc1, aes(x = west, y = north), , position = "jitter", size = 2, shape = "triangle", colour = "red") +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude") +
  annotation_north_arrow(location = "tl",which_north = "true", 
                      pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                      style = north_arrow_orienteering,width = unit(1, "cm"), 
                      height = unit(1, "cm")) +
  annotation_scale() +
  labs(title = "Pamehac Brook")
pam_map

ggsave("Figures/PB_figs/Pam_map_50K.png", pam_map, width = 8, height = 6, dpi = 300, units = "in")
ggsave("Figures/PB_figs/Pam_map_50K_no_north_arrow.png", pam_map, width = 8, height = 6, dpi = 300, units = "in")




# GC -----
# the figure in figures_maps.R is fine for the first layer

# Granite ----
# limit the amount of data 
bbox_gc_250 <- st_bbox(c(xmin = -56.35, ymin = 48.1, xmax = -57.3, ymax = 48.4),
                   crs = st_crs(newfoundland_island))  # match whatever CRS your CHS data is in

gc_watercourse_250 <- st_read(
  "C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro", 
  layer = "watercourse_1",
  wkt_filter = st_as_text(st_as_sfc(bbox_gc_250))
)

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro")

# check if crs is the same
st_crs(gc_watercourse_250) == st_crs(large_polygons_island)
st_crs(gc_watercourse_250) == st_crs(newfoundland_island)

# transform crs for nl_watercourse
gc_watercourse_250 <- st_transform(gc_watercourse_250, st_crs(newfoundland_island))
st_crs(gc_watercourse_250) == st_crs(newfoundland_island) # now they are T

# intersection
gc_watercourse_250 <- st_intersection(gc_watercourse_250, newfoundland_island)


# waterbodies ----
# this is large squares in ocean - not sure what it means
gc_waterbody_250 <- st_read(
  "C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro", 
  layer = "waterbody_2",
  wkt_filter = st_as_text(st_as_sfc(bbox_gc_250)))

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro")

# check if crs is the same
st_crs(gc_waterbody_250) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
gc_waterbody_250 <- st_transform(gc_waterbody_250, st_crs(newfoundland_island))
st_crs(gc_waterbody_250) == st_crs(newfoundland_island) # 


# intersection
gc_waterbody_250 <- st_intersection(gc_waterbody_250, newfoundland_island)



## mapview ----
# this is to identify objects and clean up the map a bit
library(mapview)

mapview(gc_watercourse_250)
#keep_ids <- c(760, 157, 726, 581, 417, 705)
keep_ids <- c(786, 725, 156, 580, 759, 704, 416)
target_rivers <- gc_watercourse_250 %>% slice(keep_ids)


mapview(gc_waterbody_250)
# filter on values identified in mapview
keep_ids <- c(425, 622)
target_bodies <- gc_waterbody_250 %>% slice(keep_ids)



## plot ----
gc_map <- ggplot() +
  geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
  geom_sf(data = target_rivers, color = "black", size = 0.3) +
  geom_sf(data = target_bodies, color = "black", size = 0.3) +
  coord_sf(xlim = c(-56.4, -57.3), ylim = c(48.0, 48.4), expand = FALSE) +
  geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
            aes(x = long + 0.2, y = lat + 0.03, label = "Meelpaeg \n Resovoir"),
            nudge_y = 0.02, size = 6) + 
  geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
            aes(x = long - 0.27, y = lat - 0.04, label = "Granite \n Lake"),
            nudge_y = 0.02, size = 5) + 
  geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
            aes(x = long - 0.06, y = lat + 0.001, label = "Granite \n Canal"),
            nudge_y = 0.02, size = 5) + 
  geom_rect(aes(xmin = -56.77, ymin = 48.185, xmax = -56.82, ymax = 48.21, colour = "red"), fill = NA, linewidth = 1.25) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude") +
  theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
  theme(axis.text = element_text(size = 14)) +
  annotation_north_arrow(location = "tl",which_north = "true",
                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                       style = north_arrow_orienteering,width = unit(1, "cm"),
                      height = unit(1, "cm")) +
  annotation_scale() +
  #labs(title = "Granite Canal") +
  theme(legend.position = "none")

gc_map
ggsave("Figures/GC_figs/GC_map_inset.png", gc_map, width = 8, height = 6, dpi = 300, units = "in")
ggsave("Figures/GC_figs/GC_map_inset_no_arrow.png", gc_map, width = 8, height = 6, dpi = 300, units = "in")


# Wrap the inset in a plot with a border
inset_map_gc <- gc_map +
  theme(
    plot.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = 0.8
    ),
    plot.margin = margin(0, 0, 0, 0)
  )


ggsave(
  "inset.png",
  inset_map_gc,
  width = 4,
  height = 4,
  dpi = 600,
  bg = "white"
)

# Combine using cowplot
final_map_gc <- ggdraw() +
  draw_plot(cc_map) +
  draw_image(
    "inset.png",
    x = 0.75,
    y = 0.25,
    width = 0.3,
    height = 0.3
  )



final_map_gc <- cowplot::ggdraw() +
  draw_plot(cc_map) +
  draw_plot(inset_map_gc, x = 0.75, y = 0.25, width = 0.35, height = 0.2)
print(final_map_gc)
ggsave("Figures/3study_areas_inset.png", final_map, width = 8, height = 6, dpi = 300, units = "in")


# GC_50K ----
# 50K experiment
#### STOP###
#The 50K is probably too high a resolution but its nice to have.  Try Neal's layers.  
### Do the cropping before brining the data layer in!!!

# crop while reading
bbox_gc <- st_bbox(
  c(xmin = -57.3,
    ymin = 48.1,
    xmax = -56.35,
    ymax = 48.4),
  crs = st_crs(newfoundland_island)
)

# from https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
gc_watercourse_50K <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "watercourse_1",
  wkt_filter = st_as_text(st_as_sfc(bbox_gc))
)

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(gc_watercourse_50K) == st_crs(large_polygons_island)
st_crs(gc_watercourse_50K) == st_crs(newfoundland_island)

# transform crs for nl_watercourse
gc_watercourse_50K <- st_transform(gc_watercourse_50K, st_crs(newfoundland_island))
st_crs(gc_watercourse_50K) == st_crs(newfoundland_island) # now they are T

# intersection
gc_watercourse_50K <- st_intersection(gc_watercourse_50K, newfoundland_island)




## waterbodies ----
# this is large squares in ocean - not sure what it means
# Did 'waterbody_2_2' which seems right 
gc_waterbody_50K <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "waterbody_2_2",
  wkt_filter = st_as_text(st_as_sfc(bbox_gc)))

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(gc_waterbody_50K) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
gc_waterbody_50K <- st_transform(gc_waterbody_50K, st_crs(newfoundland_island))
st_crs(gc_waterbody_50K) == st_crs(newfoundland_island) #


# intersection
gc_waterbody_50K <- st_intersection(gc_waterbody_50K, newfoundland_island)



## water_linear_flow ----
gc_water_linearflow_1_3 <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "water_linear_flow_1_3",
  wkt_filter = st_as_text(st_as_sfc(bbox_gc))
)

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(gc_water_linearflow_1_3) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
gc_water_linearflow_1_3 <- st_transform(gc_water_linearflow_1_3, st_crs(newfoundland_island))
st_crs(gc_water_linearflow_1_3) == st_crs(newfoundland_island) #


# intersection
# this step doesn't work - no data????
gc_water_linearflow_1_3 <- st_intersection(gc_water_linearflow_1_3, newfoundland_island)



library(mapview)

mapview(gc_waterbody_50K)
mapview(nl_water_linearflow_1_1_crop)


## plot ----
gc_map_50K <- ggplot() +
  geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
  #geom_sf(data = gc_water_linearflow_1_3, color = "blue", size = 0.3) +
  #geom_sf(data = gc_watercourse_50K, color = "blue", size = 0.3) +
  geom_sf(data = gc_waterbody_50K, color = "gray", fill = "lightblue", size = 0.05) +
  coord_sf(xlim = c(-56.4, -57.3), ylim = c(48.0, 48.4), expand = FALSE) +
  geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
            aes(x = long + 0.2, y = lat + 0.03, label = "Meelpaeg \n Resovoir"),
            nudge_y = 0.02, size = 6) + 
  geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
            aes(x = long - 0.27, y = lat - 0.04, label = "Granite \n Lake"),
            nudge_y = 0.02, size = 5) + 
  geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
            aes(x = long - 0.06, y = lat + 0.001, label = "Granite \n Canal"),
            nudge_y = 0.02, size = 5) + 
  geom_rect(aes(xmin = -56.77, ymin = 48.185, xmax = -56.82, ymax = 48.21, colour = "red"), fill = NA, linewidth = 1.25) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude") +
  theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
  theme(axis.text = element_text(size = 14)) +
  annotation_north_arrow(location = "tl",which_north = "true",
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_orienteering,width = unit(1, "cm"),
                         height = unit(1, "cm")) +
  annotation_scale() +
  #labs(title = "Granite Canal") +
  theme(legend.position = "none")

gc_map_50K

# Compensation Creek ----
## openstreetmap - this works quite well but need to go to OpenStreetMap to find the features of various objects

# 1. Force the package to use the main global Overpass server
set_overpass_url("https://overpass-api.de/api/interpreter")

library(osmdata)

bb <- c(
  xmin = -56.81,
  ymin = 48.185,
  xmax = -56.78,
  ymax = 48.2
)
#The most reliable approach is often to inspect the feature directly on OpenStreetMap:
  
 # Open https://www.openstreetmap.org
#Zoom to approximately 48.194, -56.799.
#Right-click the feature.
#Choose Query Features.

#This will show every OSM object at that location and its tags, for example:
# I got the key and value from this
water <- opq(bb) |>
  add_osm_feature(
    key = "natural",
    value = "water"
  ) |>
  osmdata_sf()


# save the object ASAP - it is simply not worth trying to deal with open street map all the time - lots of delays and issues accessing.  
saveRDS(water, "water_osm.rds")
water <- readRDS("water_osm.rds")
water$osm_polygons
water$osm_multipolygons

mapview(water$osm_polygons) # this shows CC and 
#mapview(water$osm_multipolygons) # shows whole of resovoir and Granite Canal

keep_ids <- c(96:99)
target_cc <- water$osm_polygons %>% slice(keep_ids)

keep_ids <- c(1:2)
target_lake <- water$osm_multipolygons %>% slice(keep_ids)

## good map - needs to be labelled
## GC_250K needs to be inset in the corner
## fix axes
cc_map <- ggplot() +
  geom_sf(
    data = target_cc,
    fill = "lightblue",
    colour = "blue"
  ) + 
  geom_sf(
    data = target_lake,
    fill = "lightblue",
    colour = "lightblue"
  ) +
  coord_sf(xlim = c(-56.814, -56.777), ylim = c(48.187, 48.202), expand = FALSE) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(-56.810, -56.780, by = 0.005),  # every second label
    labels = abs                             # remove W and minus sign
  ) +
  scale_y_continuous(
    breaks = seq(48.188, 48.202, by = 0.002),   # every second label
    labels = scales::number_format(accuracy = 0.01)
  ) +
  ylab("Latitude") +
  xlab("Longitude") +
  theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
  theme(axis.text = element_text(size = 12)) +
  # annotation_north_arrow(location = "tl",which_north = "true",
  #                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
  #                        style = north_arrow_orienteering,width = unit(1, "cm"),
  #                        height = unit(1, "cm")) +
   annotation_scale() +
  #labs(title = "Granite Canal") +
  theme(legend.position = "none")
cc_map



# Compensation Creek ----
## from Neal - I don't like these - poor flexibility for me and dependent on Neal.  
# cc <- st_read(
#   dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/comensation_creek", 
#   layer = "Compensation Creek_region"
# )
# 
# st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/comensation_creek")
# 
# # check if crs is the same
# st_crs(cc) == st_crs(newfoundland_island)
# 
# 
# # transform crs for nl_watercourse
# cc <- st_transform(cc, st_crs(newfoundland_island))
# st_crs(cc) == st_crs(newfoundland_island) #
# 
# 
# 
# # tailrace ----
# tr <- st_read(
#   dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/comensation_creek", 
#   layer = "Tailrace Outline_polyline"
# )
# 
# st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/comensation_creek")
# 
# # check if crs is the same
# st_crs(tr) == st_crs(newfoundland_island)
# 
# 
# # transform crs for nl_watercourse
# tr <- st_transform(tr, st_crs(newfoundland_island))
# st_crs(tr) == st_crs(newfoundland_island) #
# 
# 
# ## crp[]
# bbox1_sf <- st_bbox(
#   c(xmin = -56.82,
#     ymin = 48.185,
#     xmax = -56.77,
#     ymax = 48.21),
#   crs = st_crs(newfoundland_island)
# )
# nf_waterbody_crop <- st_crop(gc_waterbody_50K, bbox1_sf) 
# 
# # terrible map
# cc_map <- ggplot() +
#   geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
#   #geom_sf(data = nl_water_linearflow_1_3_int, color = "blue", size = 0.3) +
#   #geom_sf(data = nf_watercourses, color = "blue", size = 0.3) +
#   geom_sf(data = cc, color = "blue", size = 0.3) +
#   geom_sf(data = nf_waterbody_crop, color = "gray", fill = "lightblue", size = 0.05) +
#   coord_sf(xlim = c(-56.77, -56.82), ylim = c(48.185, 48.21), expand = FALSE) +
#   # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
#   #           aes(x = long + 0.2, y = lat + 0.03, label = "Meelpaeg \n Resovoir"),
#   #           nudge_y = 0.02, size = 6) + 
#   # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
#   #           aes(x = long - 0.27, y = lat - 0.04, label = "Granite \n Lake"),
#   #           nudge_y = 0.02, size = 5) + 
#   # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
#   #           aes(x = long - 0.06, y = lat + 0.001, label = "Granite \n Canal"),
#   #           nudge_y = 0.02, size = 5) + 
#   # geom_rect(aes(xmin = -56.77, ymin = 48.185, xmax = -56.82, ymax = 48.21, colour = "red"), fill = NA, linewidth = 1.25) +
#   theme_minimal() +
#   ylab("Latitude") +
#   xlab("Longitude") +
#   theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
#   theme(axis.text = element_text(size = 14)) +
#   # annotation_north_arrow(location = "tl",which_north = "true",
#   #                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
#   #                        style = north_arrow_orienteering,width = unit(1, "cm"),
#   #                        height = unit(1, "cm")) +
#   # annotation_scale() +
#   #labs(title = "Granite Canal") +
#   theme(legend.position = "none")
# 
# cc_map
# 
# # What needs to be done here is that the CC map needs to be 1:50K
# # the larger map is fine but bring it in by cropping - reduce time
# 
# # crop while reading
# bbox1_sf <- st_bbox(
#   c(xmin = -56.82,
#     ymin = 48.185,
#     xmax = -56.77,
#     ymax = 48.21),
#   crs = st_crs(newfoundland_island)
# )
# cc_waterbody <- st_crop(gc_waterbody_50K, bbox1_sf) 
# 
# cc_map_50K <- ggplot() +
#   geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
#   #geom_sf(data = nl_water_linearflow_1_3_int, color = "blue", size = 0.3) +
#   #geom_sf(data = nf_watercourses, color = "blue", size = 0.3) +
#   geom_sf(data = cc, color = "blue", size = 0.3) +
#   geom_sf(data = tr, color = "blue", size = 0.3) +
#   geom_sf(data = cc_waterbody, color = "gray", fill = "lightblue", size = 0.05) +
#   coord_sf(xlim = c(-56.77, -56.82), ylim = c(48.185, 48.21), expand = FALSE) +
#   # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
#   #           aes(x = long + 0.2, y = lat + 0.03, label = "Meelpaeg \n Resovoir"),
#   #           nudge_y = 0.02, size = 6) + 
#   # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
#   #           aes(x = long - 0.27, y = lat - 0.04, label = "Granite \n Lake"),
#   #           nudge_y = 0.02, size = 5) + 
#   # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
#   #           aes(x = long - 0.06, y = lat + 0.001, label = "Granite \n Canal"),
#   #           nudge_y = 0.02, size = 5) + 
#   # geom_rect(aes(xmin = -56.77, ymin = 48.185, xmax = -56.82, ymax = 48.21, colour = "red"), fill = NA, linewidth = 1.25) +
#   theme_minimal() +
#   ylab("Latitude") +
#   xlab("Longitude") +
#   theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
#   theme(axis.text = element_text(size = 14)) +
#   # annotation_north_arrow(location = "tl",which_north = "true",
#   #                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
#   #                        style = north_arrow_orienteering,width = unit(1, "cm"),
#   #                        height = unit(1, "cm")) +
#   # annotation_scale() +
#   #labs(title = "Granite Canal") +
#   theme(legend.position = "none")
# 
# cc_map_50K


# combine all maps -----

# 2x2 layout where:
# - Left column is twice as wide as the right column
# - Top row is twice as tall as the bottom row
plot_grid(pam_map, final_map, pam_map, pam_map, 
          ncol = 2, nrow = 2, 
          rel_widths = c(1, 2))



# Row 1: Two plots of equal width
row1 <- plot_grid(p1, p2, ncol = 1, rel_widths = c(1, 1)) # PB and RB

# Row 2: One plot that spans the whole width (or with different sub-ratios)
row2 <- plot_grid(p1, nrow = 2, rel_heights = 2,1)

# Combine rows vertically and make Row 1 twice as tall as Row 2
final_plot <- plot_grid(row1, row2, ncol = 1, rel_heights = c(1, 2))



# RB_50K ----
# 50K experiment
#The 50K is probably too high a resolution but its nice to have.  Try Neal's layers.  
### Do the cropping before brining the data layer in!!!

# crop while reading
rb_bbox_sf <- st_bbox(
  c(xmin = -58.69,
    ymin = 47.617,
    xmax = -58.74,
    ymax = 47.72),
  crs = st_crs(newfoundland_island)
)

# from https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
rb_watercourse_50K <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "watercourse_1",
  wkt_filter = st_as_text(st_as_sfc(rb_bbox_sf))
)

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(rb_watercourse_50K) == st_crs(large_polygons_island)
st_crs(rb_watercourse_50K) == st_crs(newfoundland_island)

# transform crs for nl_watercourse
rb_watercourse_50K <- st_transform(rb_watercourse_50K, st_crs(newfoundland_island))
st_crs(rb_watercourse_50K) == st_crs(newfoundland_island) # now they are T

# intersection
rb_watercourse_50K <- st_intersection(rb_watercourse_50K, newfoundland_island)

# mapview(rb_watercourse_50K)
keep_ids <- c(80, 66, 93, 25, 1, 92, 127, 45, 11, 65, 140, 99)
rb_watercourse_50K <- rb_watercourse_50K %>% slice(keep_ids)

## waterbodies ----
# waterbody_2_2 is the only water_body object with data
rb_waterbody_50K <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "waterbody_2_2",
  wkt_filter = st_as_text(st_as_sfc(rb_bbox_sf)))

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(rb_waterbody_50K) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
rb_waterbody_50K <- st_transform(rb_waterbody_50K, st_crs(newfoundland_island))
st_crs(rb_waterbody_50K) == st_crs(newfoundland_island) #


# intersection
rb_waterbody_50K <- st_intersection(rb_waterbody_50K, newfoundland_island)

# mapview(rb_waterbody_50K)
keep_ids <- c(33, 361, 31, 210, 391, 404, 349, 242, 363, 225, 327, 342)
rb_waterbody_50K <- rb_waterbody_50K %>% slice(keep_ids)



# water linear ----
rb_water_linear_50K1 <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro", 
  layer = "water_linear_flow_1_3",
  wkt_filter = st_as_text(st_as_sfc(rb_bbox_sf)))

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/base_layers/canvec_50K_NL/canvec_50K_NL_Hydro")

# check if crs is the same
st_crs(rb_water_linear_50K1) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
rb_water_linear_50K1 <- st_transform(rb_water_linear_50K1, st_crs(newfoundland_island))
st_crs(rb_water_linear_50K1) == st_crs(newfoundland_island) #


# intersection
rb_water_linear_50K1 <- st_intersection(rb_water_linear_50K1, newfoundland_island)


# mapview(rb_water_linear_50K1)
keep_ids <- c(696, 559, 598, 109, 430, 531, 356, 151, 745, 613, 417, 580, 209, 388, 665, 396)
rb_water_linear_50K1 <- rb_water_linear_50K1 %>% slice(keep_ids)

## plot ----
rb_map_50K <- ggplot() +
  geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
  geom_sf(data = rb_watercourse_50K, color = "black", size = 0.3) +
  geom_sf(data = rb_waterbody_50K, color = "black", size = 0.3) +
  geom_sf(data = rb_water_linear_50K1, color = "black", size = 0.3) +
  coord_sf(xlim = c(-58.7, -58.76), ylim = c(47.617, 47.69), expand = FALSE) +
  # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
  #           aes(x = long + 0.2, y = lat + 0.03, label = "Meelpaeg \n Resovoir"),
  #           nudge_y = 0.02, size = 6) + 
  # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
  #           aes(x = long - 0.27, y = lat - 0.04, label = "Granite \n Lake"),
  #           nudge_y = 0.02, size = 5) + 
  # geom_text(data = study.areas[study.areas$name == "Granite Canal", ],
  #           aes(x = long - 0.06, y = lat + 0.001, label = "Granite \n Canal"),
  #           nudge_y = 0.02, size = 5) + 
  # geom_rect(aes(xmin = -56.77, ymin = 48.185, xmax = -56.82, ymax = 48.21, colour = "red"), fill = NA, linewidth = 1.25) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude") +
  theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
  theme(axis.text = element_text(size = 14)) +
#  annotation_north_arrow(location = "tl",which_north = "true",
#                         pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
 #                        style = north_arrow_orienteering,width = unit(1, "cm"),
  #                       height = unit(1, "cm")) +
  annotation_scale() +
  #labs(title = "Granite Canal") +
  theme(legend.position = "none")

rb_map_50K 

rb_map_50K <- ggplot() +
  geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
  geom_sf(data = rb_watercourse_50K, color = "black", size = 0.3) +
  geom_sf(data = rb_waterbody_50K, color = "black", size = 0.3) +
  geom_sf(data = rb_water_linear_50K1, color = "black", size = 0.3) +
  coord_sf(
    xlim = c(-58.75, -58.69),
    ylim = c(47.617, 47.69),
    expand = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(-58.76, -58.70, by = 0.02),  # every second label
    labels = abs                             # remove W and minus sign
  ) +
  scale_y_continuous(
    breaks = seq(47.62, 47.68, by = 0.02),   # every second label
    labels = scales::number_format(accuracy = 0.01)
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "none"
  ) +
  annotation_scale()
rb_map_50K 

ggsave("Figures/RB_figs/RB_map_50K.png", rb_map, width = 8, height = 6, dpi = 300, units = "in")


# RB - openstreet ----
## openstreetmap - this works quite well but need to go to OpenStreetMap to find the features of various objects
### Don't use this ###
library(osmdata)

rb_bbox_os <- c(
    xmin = -58.69,
    ymin = 47.617,
    xmax = -58.74,
    ymax = 47.72
    )

rb_bbox_os <- c(
  xmin = -58.74,
  ymin = 47.617,
  xmax = -58.69,
  ymax = 47.72
)
#The most reliable approach is often to inspect the feature directly on OpenStreetMap:

# Open https://www.openstreetmap.org
#Zoom to approximately 48.194, -56.799.
#Right-click the feature.
#Choose Query Features.

#This will show every OSM object at that location and its tags, for example:
# I got the key and value from this
water_rb <- opq(rb_bbox_os) |>
  add_osm_feature(
    key = "natural",
    value = "water"
  ) |>
  osmdata_sf()


# save the object ASAP - it is simply not worth trying to deal with open street map all the time - lots of delays and issues accessing.  
saveRDS(water_rb, "water_rb.rds")
water_rb <- readRDS("water_rb.rds")
water_rb$osm_polygons
water_rb$osm_multipolygons

mapview(water_rb$osm_polygons) # this shows a bunch of lakes/ponds 
#mapview(water_rb$osm_multipolygons) # shows RB river and some lakes
#mapview(water_rb$osm_lines) # just shows the river


keep_ids <- c(167, 178)
target_lake <- water_rb$osm_polygons %>% slice(keep_ids)


keep_ids <- c(1)
target_river <- water_rb$osm_multipolygons %>% slice(keep_ids)

#
waterway_rb <- opq(rb_bbox_os) |>
  add_osm_feature(
    key = "waterway",
    value = "stream"
  ) |>
  osmdata_sf()

# save the object ASAP - it is simply not worth trying to deal with open street map all the time - lots of delays and issues accessing.  
saveRDS(waterway_rb, "waterway_rb")
waterway_rb <- readRDS("waterway_rb.rds")
waterway_rb$osm_polygons
waterway_rb$osm_multipolygons

mapview(waterway_rb$osm_lines) # this shows CC and 

keep_ids <- c(27, 134)
target_streams <- waterway_rb$osm_lines %>% slice(keep_ids)


## good map - needs to be labelled
## GC_250K needs to be inset in the corner
## fix axes
rb_map <- ggplot() +
  geom_sf(
    data = target_streams,
    fill = "lightblue",
    colour = "blue"
  ) + 
  geom_sf(
    data = target_river,
    fill = "lightblue",
    colour = "lightblue"
  ) +
  coord_sf(xlim = c(-58.69, -58.74), ylim = c(47.617, 47.72), expand = FALSE) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude") +
  theme(axis.title = element_text(size = 18)) + # Increases both titles to 18pt
  theme(axis.text = element_text(size = 12)) +
  # annotation_north_arrow(location = "tl",which_north = "true",
  #                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
  #                        style = north_arrow_orienteering,width = unit(1, "cm"),
  #                        height = unit(1, "cm")) +
  annotation_scale() +
  #labs(title = "Granite Canal") +
  theme(legend.position = "none")
rb_map


# shape file ----

## from Neal 
rb_resovoir <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/RB",
  layer = "KL RB_Lakes_Streams_ExportFeatures"
)

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/RB")

# check if crs is the same
st_crs(rb_resovoir) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
rb_resovoir <- st_transform(rb_resovoir, st_crs(newfoundland_island))
st_crs(rb_resovoir) == st_crs(newfoundland_island) #

lakes_poly <- st_polygonize(rb_resovoir)


# sites
rb_sites <- st_read(
  dsn = "C:/Users/lewiske/Documents/CAFE/projects/restoration/synthesis_figs_tables/Data_geo/RB",
  layer = "KL EF_Sites_ExportFeatures"
)


# transform crs for nl_watercourse
rb_sites <- st_transform(rb_sites, st_crs(newfoundland_island))
st_crs(rb_sites) == st_crs(newfoundland_island) #

fishways <- as.data.frame(matrix(
            c(-58.7019, 47.6166,
              -58.7053, 47.6197,
              -58.7080, 47.6222),
            ncol = 2, 
            byrow = TRUE
            )
          )
names(fishways) <- c("lon", "lat")


fishways_sf <- st_as_sf(
  fishways,
  coords = c("lon", "lat"),
  crs = 4326
)

# combined -----
## This is the RB plot
rb_map_50K <- ggplot() +
  geom_sf(data = rb_resovoir, color = "black", size = 0.3) +
  geom_sf(data = lakes_poly, fill="lightblue", color = "black", size = 0.3) +
  geom_sf(data = rb_sites, color = "black", size = 3) +
  geom_sf(data = fishways_sf, color = "black", shape = 8, size = 3) +
  coord_sf(
    xlim = c(-58.75, -58.68),
    ylim = c(47.615, 47.67),
    expand = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(-58.76, -58.68, by = 0.02),  # every second label
    labels = abs                             # remove W and minus sign
  ) +
  scale_y_continuous(
    breaks = seq(47.62, 47.68, by = 0.02),   # every second label
    labels = scales::number_format(accuracy = 0.01)
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  annotate("text", 
           x = -58.716, 
           y = 47.644, 
           label = "Compensation \n Stream",
          size = 4) + 
  annotate(
    "segment",
    x = -58.713,
    y = 47.644,
    xend = -58.708,
    yend = 47.6445,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text", 
           x = -58.707, 
           y = 47.669, 
           label = "Reservoir",
           size = 4) + 
  annotate(
    "segment",
    x = -58.710,
    y = 47.669,
    xend = -58.7179,
    yend = 47.667,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text", 
           x = -58.703, 
           y = 47.662, 
           label = "Dewatered \n Habitat",
           size = 4) + 
  annotate(
    "segment",
    x = -58.708,
    y = 47.662,
    xend = -58.715,
    yend = 47.664,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate(
    "segment",
    x = -58.708,
    y = 47.662,
    xend = -58.711,
    yend = 47.658,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
 # annotate("text", 
  #         x = -58.705, 
   #        y = 47.654, 
    #       label = "Powerhouse",
     #      size = 4) + 
  annotate("text", 
           x = -58.705, 
           y = 47.652, 
           label = "Culvert",
           size = 4) + 
  annotate(
    "segment",
    x = -58.707,
    y = 47.6513,
    xend = -58.711,
    yend = 47.651,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text", 
           x = -58.693, 
           y = 47.645, 
           label = "Main Stem",
           size = 4) + 
  annotate(
    "segment",
    x = -58.698,
    y = 47.645,
    xend = -58.705,
    yend = 47.645,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate(
    "segment",
    x = -58.698,
    y = 47.645,
    xend = -58.706,
    yend = 47.6407,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text", 
           x = -58.74, 
           y = 47.6585, 
           label = "Penstock",
           size = 4) + 
  annotate(
    "segment",
    x = -58.735,
    y = 47.6585,
    xend = -58.719,
    yend = 47.6585,
    colour = "black",
    linewidth = 0.5,
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "none"
  ) +
  annotation_scale()
rb_map_50K

ggsave("Figures/RB_figs/RB_map_shape.png", rb_map_50K, width = 8, height = 6, dpi = 300, units = "in")



# END ----
