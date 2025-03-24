source("scripts/utils.R")


cr <- tibble::tribble(
  ~city_name,       ~river_name,
  "Bucharest",      "Dâmbovița",
  "Bucharest",      "Colentina",
  "Iaşi",           "Bahlui",
  "Köln",           "Rhein",
  "Warsaw",         "Wisła",
  "Arnhem",         "Nederrijn",
  "Graz",           "Mur",
  "Prague",         "Vltava",
  "Roma",           "Tevere",
  "Cairo, Egypt",   "Nile River",
  "Madrid",         "Río Manzanares",
  "Turin",          "Fiume Po",
  "Sheffield, UK",  "River Don",
  "Miercurea Ciuc", "Olt",
  "Los Angeles",    "Los Angeles River",
  "Ljubljana",      "Ljubljanica",
  "Paris",          "La Seine",
  "Berlin",         "Spree",
)

buffer_distance <- 3500


save_fig <- function(x, bbox, city_name, river_name, label, col, ..., border = col,
                     out_dir = file.path(".", "figures", "generate-corridor")) {
  # Create directory and build output file path
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  filename <- paste(city_name, river_name, label, sep = "-") |>
    paste("png", sep = ".")
  filepath <- file.path(out_dir, filename)

  # Create canvas
  png(filename = filepath, width = 5, height = 5, res = 600, units = "in",
      bg = NA)

  # Extract image boundaries from bounding box
  bbox_repr <- bbox |>
    sf::st_transform(sf::st_crs(x))
  xlim <- bbox_repr[c("xmin", "xmax")]
  ylim <- bbox_repr[c("ymin", "ymax")]

  # Generate plot
  if (inherits(x, c("sf", "sfc"))) {
    plot(sf::st_geometry(x), col = col, border = border,
         xlim = xlim, ylim = ylim, ...)
  } else if (inherits(x, "SpatRaster")) {
    # setup plot as for a sf object to have it exactly superimposable
    point <- sf::st_as_sfc(bbox_repr) |>
      sf::st_centroid()
    plot(point, xlim = xlim, ylim = ylim, ...)
    terra::plot(x, add = TRUE, legend = FALSE)
  }

  # Close figure
  dev.off()
}


delineate <- function(city_name, river_name) {
  # Download datasets and setup
  bbox <- retry(CRiSp::get_osm_bb, city_name)
  crs <- CRiSp::get_utm_zone(bbox)
  # city boundary retrieval fails for Sheffield
  # city_boundary <- retry(CRiSp::get_osm_city_boundary, bbox, city_name,
  #                        force_download = TRUE) |>
  #   sf::st_transform(crs)
  river <- retry(CRiSp::get_osm_river, bbox, river_name, crs = crs)
  river_centerline <- river$centerline
  river_surface <- river$surface
  aoi <- CRiSp::get_river_aoi(river, bbox, buffer_distance = buffer_distance)
  streets <- retry(CRiSp::get_osm_streets, aoi, crs)
  railways <- retry(CRiSp::get_osm_railways, aoi, crs)
  aoi_dem <- sf::st_buffer(aoi, buffer_distance)
  dem <- CRiSp::get_dem(aoi_dem, crs = crs)

  # Compute valley, spatial network, corridor and segments
  valley <- CRiSp::get_valley(dem, c(river_centerline, river_surface))
  network <- dplyr::bind_rows(streets, railways) |>
    CRiSp::as_network()
  corridor <- CRiSp::delineate_corridor(network, river_centerline, river_surface,
                                        aoi = sf::st_transform(aoi, crs), dem = dem,
                                        capping_method = "shortest-path")

  segments <- get_segments(corridor, network, river_centerline)
  # corridor_1 <- CRiSp::delineate_corridor(network, river_centerline, river_surface,
  #                                         aoi = sf::st_transform(aoi, crs), dem = dem,
  #                                         capping_method = "shortest-path", max_iterations = 1)
  # segments_1 <- get_segments(corridor, network, river_centerline)

  list(
    aoi = aoi,
    # city_boundary = city_boundary,
    river_centerline = river_centerline,
    river_surface = river_surface,
    streets = streets,
    railways = railways,
    dem = dem,
    valley = valley,
    network = network,
    corridor = corridor,
    segments = segments # ,
    # corridor_1 = corridor_1,
    # segments_1 = segments_1
  )
}

plot_delineations <- function(x) {
  plot(x$corridor)
  plot(x$valley, col = "gold", border = FALSE, add = T)
  plot(x$river_surface, col = "lightblue", border = NA, add = T)
  plot(x$river_centerline, col = "lightblue", lwd = 2, add = T)
  plot(x$streets$geometry, col = "grey", lwd = 0.5, add = T)
  plot(x$railways$geometry, col = "darkgrey", lwd = 0.5, add = T)
  # plot(x$segments_1, border = "green", col = NA, add = T)
  # plot(x$corridor_1, border = "darkred", col = NA, lwd = 2, add = T)
  plot(x$segments, border = "blue", col = NA, add = T)
  plot(x$corridor, border = "red", col = NA, lwd = 2, add = T)
  # plot(city_boundary, add = T)
}

save_delineations <- function(x, city_name, river_name) {
  bbox <- sf::st_bbox(x$aoi)
  c(x$river_centerline, x$river_surface) |>
    save_fig(bbox, city_name, river_name, "river", col = "#2e809e",
             main = paste0(city_name, ", ", river_name))
  sf::st_as_sf(x$network, "edges") |>
    save_fig(bbox, city_name, river_name, "network", col = "black", lwd = 0.5,
             main = paste0(city_name, ", ", river_name))
  save_fig(x$dem, bbox, city_name, river_name, "dem",
           main = paste0(city_name, ", ", river_name))
  save_fig(x$valley, bbox, city_name, river_name, "valley", col = "gold",
           main = paste0(city_name, ", ", river_name))
  save_fig(x$corridor, bbox, city_name, river_name, "corridor",
           border = "orange", col = NA, lwd = 2,
           main = paste0(city_name, ", ", river_name))
  save_fig(x$segments, bbox, city_name, river_name, "segments",
           border = "orange", col = NA,
           main = paste0(city_name, ", ", river_name))
}


# Delineate all corridors
delineations <- vector("list", length = nrow(cr))
for (i in 1:length(delineations)) {
  delineations[[i]] <- delineate(cr[i, ]$city_name, cr[i, ]$river_name)
}


# # Check all delineations visually
# for (i in 1:length(delineations)) {
#   plot_delineations(delineations[[i]])
# }


# Save all delineations
for (i in 1:length(delineations)) {
  save_delineations(delineations[[i]], cr[i, ]$city_name, cr[i, ]$river_name)
}
