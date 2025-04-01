source("scripts/utils.R")
library(CRiSp)

city_river_names <- tibble::tribble(
  ~city_name,       ~river_name,          ~network_buffer, ~buildings_buffer, ~dem_buffer,
  "Bucharest",      "Dâmbovița",           2500,             100,              2500,
  # "Bucharest",      "Colentina",
  # "Iaşi",           "Bahlui",
  # "Köln",           "Rhein",
  # "Warsaw",         "Wisła",
  # "Arnhem",         "Nederrijn",
  # "Graz",           "Mur",
  # "Prague",         "Vltava",
  # "Roma",           "Tevere",
  # "Cairo, Egypt",   "Nile River",
  # "Madrid",         "Río Manzanares",
  # "Turin",          "Fiume Po",
  # "Sheffield, UK",  "River Don",
  # "Miercurea Ciuc", "Olt",
  # "Los Angeles",    "Los Angeles River",
  # "Ljubljana",      "Ljubljanica",
  # "Paris",          "La Seine",
  # "Berlin",         "Spree",
)

# buffer_distance <- 3500 # for some cities

get_delineate_layers <- function(
  city_name, river_name, network_buffer, buildings_buffer,
  dem_buffer, force_download = FALSE
) {
  # Download OSM datasets
  bb <- CRiSp::get_osm_bb(city_name)
  crs <- CRiSp::get_utm_zone(bb)
  osm_data <- CRiSp::get_osmdata(
    city_name, river_name, network_buffer = network_buffer,
    buildings_buffer = buildings_buffer, crs = crs,
    force_download = force_download
  )

  # Download DEM
  aoi <- CRiSp::reproject(osm_data$aoi_network, crs)
  aoi_dem <- buffer(osm_data$aoi_network, dem_buffer) # from utils.R
  dem <- CRiSp::get_dem(aoi_dem, crs = crs, force_download = force_download)

  # Compute valley, spatial network, corridor and segments
  valley <- CRiSp::delineate_valley(
    dem, c(osm_data$river_centerline, osm_data$river_surface)
  )

  network <- dplyr::bind_rows(osm_data$streets, osm_data$railways) |>
    CRiSp::as_network()
  corridor <- CRiSp::delineate_corridor(
    network,
    osm_data$river_centerline,
    osm_data$river_surface,
    aoi = sf::st_transform(aoi, crs),
    dem = dem,
    capping_method = "shortest-path"
  )

  segments <- get_segments(corridor, network, osm_data$river_centerline)

  list(
    aoi = osm_data$aoi_network,
    city_boundary = osm_data$boundary,
    river_centerline = osm_data$river_centerline,
    river_surface = osm_data$river_surface,
    streets = osm_data$streets,
    railways = osm_data$railways,
    dem = dem,
    valley = valley,
    network = network,
    corridor = corridor,
    segments = segments
  )
}

plot_delineations <- function(x, setup) {
  file_name <- paste0("delineation_", setup$city_name, "_", setup$river_name , ".png")
  file_path <- file.path("figures", file_name)

  png(
    filename = file_path, width = 8, height = 5, res = 600, units = "in",
    bg = NA
  )

  plot(x$corridor)
  plot(x$valley, col = "gold", border = FALSE, add = TRUE)
  plot(x$river_surface, col = "lightblue", border = NA, add = TRUE)
  plot(x$river_centerline, col = "lightblue", lwd = 2, add = TRUE)
  plot(x$streets$geometry, col = "grey", lwd = 0.5, add = TRUE)
  plot(x$railways$geometry, col = "darkgrey", lwd = 0.5, add = TRUE)
  plot(x$segments, border = "blue", col = NA, add = TRUE)
  plot(x$corridor, border = "red", col = NA, lwd = 2, add = TRUE)
  plot(x$city_boundary, add = TRUE)

  title_text <- paste0(
    "Delineation of ", setup$river_name, " in ", setup$city_name
  )
  title(main = title_text)

  legend(
    "topright",
    legend = c(
      "Corridor", "Valley", "River Surface", "River Centerline",
      "Streets", "Railways", "Segments", "City Boundary"
      ),
    col = c(
      "red", "gold", "lightblue",
    "lightblue", "grey", "darkgrey", "blue", "black"
    ),
    lwd = c(2, 1, 1, 2, 0.5, 0.5, 1, 1),
    bty = "n", cex = 0.8
  )

  # Close figure
  dev.off()
}

save_delineations <- function(x, setup) {
  # save all the layers to a geopackage file using sf::st_write
  file_name <- paste0(
    "delineation_", setup$city_name, "_", setup$river_name, ".gpkg"
  )
  file_path <- file.path("data", file_name)
  for (layer_name in names(x)) {
    sfc_layer <- x[[layer_name]]
    if (inherits(sfc_layer, "sfnetwork")) {
      print(paste("converting network to sfc:", layer_name))
      nodes_sf <- sf::st_as_sf(sfc_layer, what = "nodes")
      edges_sf <- sf::st_as_sf(sfc_layer, what = "edges")
      sfc_layer <- rbind(nodes_sf, edges_sf)
      sf::st_write(sfc_layer, file_path, layer = layer_name, append = TRUE)
    } else if (inherits(sfc_layer, "SpatRaster")) {
      print(paste("writing a SpatRaster:", layer_name))
      terra::writeRaster(sfc_layer, file_path, overwrite = TRUE)
    } else if (inherits(sfc_layer, "sfc")) {
      print(paste("object is already sfc:", layer_name))
      sf::st_write(sfc_layer, file_path, layer = layer_name, append = TRUE)
    } else {
      print(paste("object is not sfc and not stored:", layer_name))
    }
  }
  print(paste("Saving layers done!", file_path))

  # Connect to the GeoPackage again for custom attributes
  con <- RPostgreSQL::dbConnect(RSQLite::SQLite(), file_path)
  RPostgreSQL::dbWriteTable(
    con, "delineation_attributes", setup, overwrite = TRUE
  )
  RPostgreSQL::dbDisconnect(con)
  print(paste("Saving attributes done!", file_path))
}

# Delineate all corridors
# delineations <- vector("list", length = nrow(city_river_names))
# for (i in seq_along(delineations)) {
#   setup <- city_river_names[i, ]
#   print(paste("Processing", setup$city_name, setup$river_name))
#   delineations[[i]] <- get_delineate_layers(
#     setup$city_name,
#     setup$river_name,
#     setup$network_buffer,
#     setup$buildings_buffer,
#     setup$dem_buffer,
#     force_download = FALSE
#   )
#   print(paste("Finished processing", setup$city_name, setup$river_name))
# }

# # # Save each delineation into a single geopackage file
# for (i in seq_along(delineations)) {
#   save_delineations(delineations[[i]], city_river_names[i, ])
# }

# Plot each delineation
for (i in seq_along(delineations)) {
  plot_delineations(delineations[[i]], city_river_names[i, ])
}
