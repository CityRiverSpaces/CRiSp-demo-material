source("scripts/utils.R")
library(CRiSp)

# nolint start
city_river_names <- tibble::tribble(
  ~city_name,       ~river_name,          ~network_buffer,  ~dem_buffer,
  "Bucharest",      "Dâmbovița",           2500,             2500,
  "Bucharest",      "Colentina",           2500,             2500,
  "Iaşi",           "Bahlui",              2500,             2500,
  "Köln",           "Rhein",               2500,             2500,
  "Warsaw",         "Wisła",               2500,             2500,
  "Arnhem",         "Nederrijn",           2500,             2500,
  "Graz",           "Mur",                 2500,             2500,
  "Prague",         "Vltava",              2500,             2500,
  "Roma",           "Tevere",              3500,             3500,
  "Cairo, Egypt",   "Nile River",          2500,             2500,
  "Madrid",         "Río Manzanares",      2500,             2500,
  "Turin",          "Fiume Po",            2500,             2500,
  "Sheffield, UK",  "River Don",           2500,             2500,  # No city boundary found. name may be incorrect
  "Miercurea Ciuc", "Olt",                 2500,             2500,  # Cannot identify corridor edges
  "Los Angeles",    "Los Angeles River",   2500,             2500,
  "Ljubljana",      "Ljubljanica",         2500,             2500,
  "Paris",          "La Seine",            2500,             2500,
  "Berlin",         "Spree",               2500,             2500,
  "Dresden",        "Elbe",                2500,             2500,
  "London",         "River Lea",           2500,             2500,
  "Bratislava",     "Danube",              2500,             2500,
  "Calarasi",       "Dunare",              2500,             2500, # error in railway function
  "Bristol",        "River Avon",          2500,             2500,
  "Dresden",        "Geberbach",           2500,             2500, # error in railway function
  "Rotterdam",      "Rotte",               2500,             2500, # No city boundary found. name may be incorrect.
  "Leiden",         "Rijn",                2500,             2500,
  "Delft",          "Schie",               2500,             2500, # error in railway function
)
# nolint end


get_delineate_layers <- function(
  city_name, river_name, network_buffer,
  dem_buffer, force_download = FALSE
) {
  # Download OSM datasets
  bb <- CRiSp::get_osm_bb(city_name)
  crs <- CRiSp::get_utm_zone(bb)
  osm_data <- CRiSp::get_osmdata(
    city_name, river_name, network_buffer = network_buffer,
    buildings_buffer = NULL, crs = crs,
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
  file_name <- paste0(
    "delineation_", setup$city_name, "_", setup$river_name, ".png"
  )
  file_path <- file.path("plots", file_name)

  if (file.exists(file_path)) {
    print(paste("Plot already exists:", file_path))
    return()
  }

  png(
    filename = file_path, width = 8, height = 8, res = 600, units = "in"
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
  subtitle_text <- paste0(
    "Network Buffer: ", setup$network_buffer, "m, ",
    "DEM Buffer: ", setup$dem_buffer, "m"
  )
  title(main = title_text, sub = subtitle_text, cex.main = 1.5, cex.sub = 0.5)

  par(xpd = TRUE)
  legend(
    "topright",
    legend = c(
      "Corridor", "Valley", "River Surface", "River Centerline",
      "Streets", "Railways", "Segments", "City Boundary"
    ),
    col = c(
      "red", "gold", "lightblue",
      "lightblue", "grey", "darkgrey",
      "blue", "black"
    ),
    lwd = c(2, 1, 1, 2, 0.5, 0.5, 1, 1),
    bty = "n", cex = 0.8
  )

  # Close figure
  dev.off()
}

save_delineations_layers <- function(x, setup) {
  # save all the layers to a geopackage file using sf::st_write
  file_name <- paste0(
    "delineation_", setup$city_name, "_", setup$river_name, ".gpkg"
  )
  file_path <- file.path("data", file_name)

  # Check if the file already exists, do nothing
  if (file.exists(file_path)) {
    print(paste("File already exists:", file_path))
    return()
  }

  for (layer_name in names(x)) {
    sfc_layer <- x[[layer_name]]
    if (inherits(sfc_layer, "sfnetwork")) {
      nodes_sf <- sf::st_as_sf(sfc_layer, what = "nodes")
      sf::st_write(
        nodes_sf, file_path, layer = paste0(layer_name, "_nodes"), append = TRUE
      )

      edges_sf <- sf::st_as_sf(sfc_layer, what = "edges")
      sf::st_write(
        edges_sf, file_path, layer = paste0(layer_name, "_edges"), append = TRUE
      )

    } else if (inherits(sfc_layer, "sfc")) {
      sf::st_write(sfc_layer, file_path, layer = layer_name, append = TRUE)
    } else {
      print(paste("object is not sfc and not stored:", layer_name))
    }
  }
  print(paste("Saving layers done!", file_path))

  # Connect to the GeoPackage again for custom attributes
  con <- RPostgreSQL::dbConnect(RSQLite::SQLite(), file_path)
  RPostgreSQL::dbWriteTable(
    con, "delineation_metadata", setup, row.names = FALSE
  )
  RPostgreSQL::dbDisconnect(con)
  print(paste("Saving metadata done!", file_path))
}

save_dem <- function(dem, setup) {
  file_name <- paste0("dem_", setup$city_name, "_", setup$river_name, ".tif")
  file_path <- file.path("data", file_name)

  # Check if the file already exists, do nothing
  if (file.exists(file_path)) {
    print(paste("File already exists:", file_path))
    return()
  }
  terra::writeRaster(dem, file_path)
  print(paste("Saving DEM done!", file_path))
}

# Delineate all corridors
delineations <- vector("list", length = nrow(city_river_names))

for (i in seq_along(delineations)) {
  setup <- city_river_names[i, ]

  # if delineations[[i]] does not exist, get it
  if (is.null(delineations[[i]])) {
    print(paste("Processing", setup$city_name, setup$river_name))
    delineations[[i]] <- get_delineate_layers(
      setup$city_name,
      setup$river_name,
      setup$network_buffer,
      setup$dem_buffer,
      force_download = FALSE
    )
    print(paste("Finished processing", setup$city_name, setup$river_name))
  }

  # Save each delineation into a single geopackage file and dem to tiff
  save_delineations_layers(delineations[[i]], city_river_names[i, ])
  save_dem(delineations[[i]]$dem, city_river_names[i, ])

  # Plot each delineation and save it as a png
  plot_delineations(delineations[[i]], city_river_names[i, ])
}
