# This file is to make maps for the synthesis document.  It is based on scratch_pad_map_chat in the GC folder.

# library ----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(cowplot)
library(ggrepel)
library(ggspatial)
library(mapview)
library(leaflet)

# basic map ----
# layers 
# Get Canada map at province level
canada <- ne_states(country = "Canada", returnclass = "sf")

# Filter for Newfoundland and Labrador
newfoundland <- canada %>% 
  filter(name_en == "Newfoundland and Labrador")


ggplot(data = newfoundland) +
  geom_sf(fill = "lightblue", color = "black") +
  coord_sf() +
  theme_minimal() +
  ggtitle("Map of Newfoundland and Labrador")

# island ----
# Download GADM Canada at level 1 (province/territory)
# canada_prov <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_CAN_shp.zip", 
#                        layer = "gadm41_CAN_1")
# Download GADM Canada at level 1 (province/territory)
canada_prov <- st_read("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo", layer = "gadm41_CAN_1")

# Filter to Newfoundland and Labrador
nl <- canada_prov %>% filter(NAME_1 == "Newfoundland and Labrador")

# Cast into individual polygons
nl_polygons <- nl %>% st_cast("POLYGON")


# Compute area in square meters (default unit from st_area)
nl_polygons <- nl %>% st_cast("POLYGON")
areas <- st_area(nl_polygons)

# Set size threshold (e.g., 100 km² = 100,000,000 m²) to eliminate small islands and Labrador
threshold <- units::set_units(100, km^2)
threshold2 <- units::set_units(150000, km^2)

# Filter polygons larger than threshold
large_polygons2 <- nl_polygons[threshold2 > areas & areas > threshold, ]


# Filter polygons: east of -58 and south of 52; removes small islands around Labrador
centroids <- st_centroid(large_polygons2)
coords <- st_coordinates(centroids)
#coords <- st_coordinates(large_polygons2)
large_polygons_island <- large_polygons2[coords[, 1] > -58 & coords[, 2] < 52, ]


# Merge into one geometry (optional)
newfoundland_island <- st_union(large_polygons_island) %>% st_sf()


## inset CAN----
canada_outline <- ne_states(country = "Canada", returnclass = "sf")

inset_map <- ggplot(canada_outline) +
  geom_sf(fill = "white", color = "gray50") +
  geom_sf(data = nl, fill = "gray90", color = "black") +
  geom_rect(aes(xmin = -60, xmax = -51, ymin = 46.5, ymax = 52, colour = "red"), fill = NA, linewidth = 1.25) +
  coord_sf(
    xlim = c(-142, -50.5),
    ylim = c(41, 84),
    expand = FALSE
  ) +
  theme_void() +
  theme(legend.position = "none")

# Wrap the inset in a plot with a border
inset_map <- inset_map +
  theme(
    plot.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = 0.8
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

# framed_inset <- ggplot() +
#   theme_void() +
#   annotation_custom(ggplotGrob(inset_map), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   theme(
#     plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
#   )

## study areas ----
study.areas <- data.frame(name = c("Rose Blanche River",
                                   "Granite Canal",
                                   "Pamehac Brook" #, "Seal Cove \n River" 
                                   ),
                          lat = c(47.65, 48.2, 48.89), # , 47.37
                          long = c(-58.71, -56.8, -56.11), # , -53.03
                          y = c(47.5, 48.3, 49.1), # , 48.25 for labels
                          x = c(-58, -56.4, -56.55), # , -54.6
                          ystart = c(47.5, 48.3, 48.875), # , 48.25
                          xstart = c(-58, -56.4, -56.095), # , -54.6
                           yend = c(47.85, 48.35, 48.886), # , 48.1 # arrow
                           xend = c(-58, -57, -56.119) # , -54.4
  )

## draw map ----
main_map <- ggplot(newfoundland_island) +
  geom_sf(fill = "grey", color = "black") +
  geom_point(data = study.areas, aes(long, lat)) +
  geom_text(data = study.areas, aes(x, y, label = name )) +
  # geom_segment(data = study.areas,
  #              aes(x = xend, y = yend, xend = long, yend = lat),
  #              arrow = arrow(length = unit(0.01, "npc"))
  # ) +
  ylab("Latitude") +
  xlab("Longitude") +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_orienteering,width = unit(1, "cm"), 
                         height = unit(1, "cm")) +
  annotation_scale() +
  #  coord_fixed(1.3) +
  theme_minimal()
main_map
ggsave("Figures/3study_areas.png", main_map, width = 8, height = 6, dpi = 300, units = "in")



# Combine using cowplot
final_map <- cowplot::ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.55, y = 0.65, width = 0.3, height = 0.3)
print(final_map)
ggsave("Figures/3study_areas_inset.png", final_map, width = 8, height = 6, dpi = 300, units = "in")


# watercourses ----
# from https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
nl_watercourse <- st_read("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro", layer = "watercourse_1")

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro")

# check if crs is the same
st_crs(nl_watercourse) == st_crs(large_polygons_island)
st_crs(nl_watercourse) == st_crs(newfoundland_island)

# transform crs for nl_watercourse
nl_watercourse <- st_transform(nl_watercourse, st_crs(newfoundland_island))
st_crs(nl_watercourse) == st_crs(newfoundland_island) # now they are T

# intersection
nf_watercourses <- st_intersection(nl_watercourse, newfoundland_island)

# waterbodies ----
# this is large squares in ocean - not sure what it means
nl_waterbody <- st_read("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro", layer = "waterbody_2")

st_layers("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo/canvec_250K_NL_Hydro")

# check if crs is the same
st_crs(nl_waterbody) == st_crs(newfoundland_island)


# transform crs for nl_watercourse
nl_waterbody <- st_transform(nl_waterbody, st_crs(newfoundland_island))
st_crs(nl_waterbody) == st_crs(newfoundland_island) # 


# intersection
nf_waterbody <- st_intersection(nl_waterbody, newfoundland_island)


# Pamehac ----
# limit the amount of data 
bbox_sf <- st_bbox(c(xmin = -56.2, ymin = 48.83, xmax = -55.95, ymax = 49.0),
                   crs = st_crs(nf_watercourses))  # match whatever CRS your CHS data is in
nf_watercourses_crop <- st_crop(nf_watercourses, bbox_sf)
nf_waterbody_crop <- st_crop(nf_waterbody, bbox_sf)

## mapview ----
# this is to identify objects and clean up the map a bit
library(mapview)

mapview(nf_watercourses_crop)
keep_ids <- c(11, 102, 67, 54, 66, 16, 78, 74, 76)
target_rivers <- nf_watercourses_crop %>% slice(keep_ids)

keep_ids <- c(75)
stream_diversion <- nf_watercourses_crop %>% slice(keep_ids)


mapview(nf_waterbody_crop)
# filter on values identified in mapview
keep_ids <- c(9, 15, 17, 19, 42, 44)
target_bodies <- nf_waterbody_crop %>% slice(keep_ids)


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
  geom_sf(data = stream_diversion, color = "red", size = 0.3) +
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
  geom_point(data = df_loc1, aes(x = west, y = north), ) +
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
ggsave("Figures/PB_figs/Pam_map.png", pam_map, width = 8, height = 6, dpi = 300, units = "in")


## inset NF ----
inset_main_map <- main_map +
  geom_rect(aes(xmin = -55.9, xmax = -56.25, ymin = 48.75, ymax = 49.0, colour = "red"), fill = NA, linewidth = 1.25) +
  theme_void() +
  theme(legend.position = "none") + 
  theme(
    plot.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = 0.8
    ),
    plot.margin = margin(0, 0, 0, 0)
  )


# Combine using cowplot
final_map_pam <- cowplot::ggdraw() +
  draw_plot(pam_map) +
  draw_plot(inset_main_map, x = 0.55, y = 0.65, width = 0.3, height = 0.3)
print(final_map_pam)
ggsave("Figures/PB_figs/Pam_map_inset.png", final_map_pam, width = 8, height = 6, dpi = 300, units = "in")


# Granite ----
# limit the amount of data 
bbox_sf <- st_bbox(c(xmin = -56.35, ymin = 48.1, xmax = -57.3, ymax = 48.4),
                   crs = st_crs(nf_watercourses))  # match whatever CRS your CHS data is in
nf_watercourses_crop <- st_crop(nf_watercourses, bbox_sf)
nf_waterbody_crop <- st_crop(nf_waterbody, bbox_sf)


## mapview ----
# this is to identify objects and clean up the map a bit
library(mapview)

mapview(nf_watercourses_crop)
keep_ids <- c(760, 157, 726, 581, 417, 705)
target_rivers <- nf_watercourses_crop %>% slice(keep_ids)


mapview(nf_waterbody_crop)
# filter on values identified in mapview
keep_ids <- c(425, 621)
target_bodies <- nf_waterbody_crop %>% slice(keep_ids)



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


# I spent a lot of time trying to figure out how to a layer from Compensation Creek but its just too small and none of the below worked.  Just hacked it with the above map and Neils figures from Seminar_SPERA2015_2016.pdf

# Claude ----
# library(leaflet)
# library(sf)
# library(mapview)
# library(cowplot)
# library(magick)
# 
# mapviewOptions(legend = FALSE)   # kill mapview's auto legend globally
# 
# mapview(nf_watercourses_crop)
# keep_ids <- c(2, 4, 3)
# comp_creek <- nf_watercourses_crop %>% slice(-keep_ids)
# 
# # your target extent in WGS84
# bb_ll <- st_bbox(c(xmin = -56.81, ymin = 48.188, xmax = -56.78, ymax = 48.2), crs = 4326)
# 
# # reproject to Web Mercator to get the true aspect ratio leaflet will render
# bb_merc <- bb_ll |> st_as_sfc() |> st_transform(3857) |> st_bbox()
# 
# width_m  <- bb_merc["xmax"] - bb_merc["xmin"]
# height_m <- bb_merc["ymax"] - bb_merc["ymin"]
# aspect   <- as.numeric(width_m / height_m)
# 
# # pick a target resolution for the long edge, derive the short edge from aspect
# target_long_edge <- 1600
# if (aspect >= 1) {
#   vwidth  <- target_long_edge
#   vheight <- round(target_long_edge / aspect)
# } else {
#   vheight <- target_long_edge
#   vwidth  <- round(target_long_edge * aspect)
# }
# 
# 
# base_map <- leaflet(options = leafletOptions(zoomSnap = 0, zoomDelta = 0.1)) |>
#   addProviderTiles(providers$CartoDB.Positron)  # or whatever basemap mapview used
# 
# m <- mapview(comp_creek, layer.name = "Watercourses", map = base_map)@map |>
#   fitBounds(lng1 = unname(bb_ll["xmin"]), lat1 = unname(bb_ll["ymin"]),
#             lng2 = unname(bb_ll["xmax"]), lat2 = unname(bb_ll["ymax"]),
#             options = list(padding = c(0, 0)))
# 
# # strip the scale bar control mapview adds automatically
# m$x$calls <- Filter(function(call) !identical(call$method, "addScaleBar"), m$x$calls)
# 
# mapshot2(
#   m,
#   file = "inset_raw.png",
#   vwidth  = vwidth,
#   vheight = vheight,
#   remove_controls = c("zoomControl", "layersControl", "homeButton")
# )
# 
# 
# 
# inset_img <- ggdraw() + draw_image("Figures/GC_figs/GC_map_inset.png")   # or your ggplot object directly
# main_plot <- magick::image_read("inset_raw.png")
# 
# 
# ggdraw() +
#   draw_image("inset_raw.png", x = 0, y = 0, width = 1, height = 1) +   # mapview = main map, full canvas
#   draw_image("Figures/GC_figs/GC_map_inset.png",                       # ggplot2 map = inset
#              x = 0.62, y = 0.05,
#              width = 0.35, height = 0.35,
#              hjust = 0, vjust = 0)
# 
# 
# 
# 
# library(leaflet)
# library(sf)
# library(mapview)
# library(cowplot)
# library(magick)
# 
# mapviewOptions(legend = FALSE)   # kill mapview's auto legend globally
# 
# keep_ids <- c(2, 4, 3)
# comp_creek <- nf_watercourses_crop %>% slice(-keep_ids)
# 
# bb_ll <- st_bbox(c(xmin = -56.81, ymin = 48.188, xmax = -56.78, ymax = 48.2), crs = 4326)
# bb_merc <- bb_ll |> st_as_sfc() |> st_transform(3857) |> st_bbox()
# width_m  <- bb_merc["xmax"] - bb_merc["xmin"]
# height_m <- bb_merc["ymax"] - bb_merc["ymin"]
# aspect   <- as.numeric(width_m / height_m)
# 
# target_long_edge <- 1600
# if (aspect >= 1) {
#   vwidth  <- target_long_edge
#   vheight <- round(target_long_edge / aspect)
# } else {
#   vheight <- target_long_edge
#   vwidth  <- round(target_long_edge * aspect)
# }
# 
# base_map <- leaflet(options = leafletOptions(zoomSnap = 0, zoomDelta = 0.1)) |>
#   addProviderTiles(providers$CartoDB.Positron)
# 
# m <- mapview(comp_creek, layer.name = "Watercourses", map = base_map,
#              legend = FALSE)@map |>
#   fitBounds(lng1 = unname(bb_ll["xmin"]), lat1 = unname(bb_ll["ymin"]),
#             lng2 = unname(bb_ll["xmax"]), lat2 = unname(bb_ll["ymax"]),
#             options = list(padding = c(0, 0)))
# 
# # strip the scale bar control mapview adds automatically
# m$x$calls <- Filter(function(call) !identical(call$method, "addScaleBar"), m$x$calls)
# 
# mapshot2(
#   m,
#   file = "inset_raw.png",
#   vwidth  = vwidth,
#   vheight = vheight,
#   remove_controls = c("zoomControl", "layersControl", "homeButton")
# )
# 
# ggdraw() +
#   draw_image("inset_raw.png", x = 0, y = 0, width = 1, height = 1)
# 
# 
# # georeference approach
# 
# library(terra)
# 
# img <- rast("inset_raw.png")  # RGB(A) bands
# ext(img) <- ext(bb_merc["xmin"], bb_merc["xmax"], bb_merc["ymin"], bb_merc["ymax"])
# crs(img) <- "EPSG:3857"
# 
# # reproject to match your vector data's CRS if needed
# img_ll <- project(img, "EPSG:4326")
# 
# library(tidyterra)
# 
# ggplot() +
#   geom_spatraster_rgb(data = img_ll) +
#   geom_sf(data = comp_creek, color = "blue") +
#   coord_sf()
# 
# 
# 
# library(ggspatial)
# library(ggplot2)
# library(sf)
# library(prettymapr)
# 
# ggplot() +
#   annotation_map_tile(type = "cartolight", zoomin = 2, progress = "text") +
#   geom_sf(data = comp_creek, color = "blue") +
#   coord_sf(xlim = c(bb_ll[["xmin"]], bb_ll[["xmax"]]),
#            ylim = c(bb_ll[["ymin"]], bb_ll[["ymax"]]),
#            expand = FALSE)



# Rose Blanche ----
# limit the amount of data 
bbox_sf <- st_bbox(c(xmin = -58.69, ymin = 47.617, xmax = -58.74, ymax = 47.65),
                   crs = st_crs(nf_watercourses))  # match whatever CRS your CHS data is in
nf_watercourses_crop <- st_crop(nf_watercourses, bbox_sf)
nf_waterbody_crop <- st_crop(nf_waterbody, bbox_sf)


## mapview ----
# this is to identify objects and clean up the map a bit
mapview(nf_watercourses_crop)
keep_ids <- c(3, 6, 2, 7, 1, 5, 4)
target_rivers <- nf_watercourses_crop %>% slice(keep_ids)


mapview(nf_waterbody_crop)
# filter on values identified in mapview
target_bodies <- nf_waterbody_crop



## plot ----
rb_map <- ggplot() +
  geom_sf(data = newfoundland_island, fill = "gray95", color = "black") +
  geom_sf(data = target_rivers, color = "black", size = 0.3) +
  geom_sf(data = target_bodies, color = "black", size = 0.3) +
  coord_sf(xlim = c(-58.7, -58.72), ylim = c(47.617, 47.65), expand = FALSE) +
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
  annotation_north_arrow(location = "tl",which_north = "true",
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_orienteering,width = unit(1, "cm"),
                         height = unit(1, "cm")) +
  annotation_scale() +
  #labs(title = "Granite Canal") +
  theme(legend.position = "none")

rb_map
ggsave("Figures/RB_figs/RB_map_inset.png", rb_map, width = 8, height = 6, dpi = 300, units = "in")


# END ----