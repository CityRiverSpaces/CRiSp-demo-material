cr <- tibble::tribble(
  ~city_name, ~river_name,
  "Bucharest", "Dâmbovița",
  "Bucharest", "Colentina",
  "Iaşi",      "Bahlui",
  "Köln",      "Rhein",
  "Warsaw",    "Wisła",
  "Arnhem",    "Nederrijn",
  "Graz",      "Mur",
  "Prague",    "Vltava",
  # "Roma",      "Tevere",  # AOI not split
  # "Cairo",    "Nile River",  # Segmentation fails
  # "Madrid",   "Manzanares",  # Corridor delineation fails
  # "Turin",    "Fiume Po",    # Left corridor edge overflows
  # "Sheffield, UK", "River Don",  # Error when retrieving river end points
  # "Miercurea Ciuc", "Olt",  # River on the edge of city returns single segment
  # "Los Angeles", "Los Angeles River",  # AOI not split
  # "Ljubljana", "Ljubljanica",  # AOI not split
)


#' Retry function call
#'
#' Functions running API calls sometimes fail and need to be retried. This
#' wrapper function re-runs such functions a given number of times with a delay
#' in-between calls.
#'
#' @param func  Function to re-run
#' @param ...  Function parameters
#' @param max_retries  Maximum number of retries
#' @param delay  Delay between retries in seconds
#'
#' @return Value returned by wrapped function
retry <- function(func, ..., max_retries = 5, delay = 2) {
  attempt <- 1
  while (attempt <= max_retries) {
    result <- tryCatch({
      func(...)  # Call the function with arguments
    }, error = function(e) {
      message(sprintf("Attempt %d failed: %s", attempt, e$message))
      return(NULL)
    })

    if (!is.null(result)) {
      return(result)  # Successfully retrieved result
    }

    message(sprintf("Retrying in %d seconds...", delay))
    Sys.sleep(delay)
    attempt <- attempt + 1
  }

  stop("Function failed after multiple attempts.")
}


get_river_centerline <- function(bbox, river_name, crs,
                                 force_download = FALSE) {
  river_centerline <- CRiSp::osmdata_as_sf("waterway", "river", bbox,
                                           force_download = force_download)
  river_centerline <- river_centerline$osm_multilines |>
    dplyr::filter(.data$name == river_name) |>
    # the query can return more features than actually intersecting the bb
    sf::st_filter(sf::st_as_sfc(bbox), .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_transform(crs)
}


get_aoi <- function(bbox, river_centerline, buffer) {
  bbox_repr <- sf::st_transform(bbox, sf::st_crs(river_centerline))
  river_centerline |>
    sf::st_crop(bbox_repr) |>
    sf::st_buffer(buffer, endCapStyle = "FLAT") |>
    sf::st_transform(4326) |>
    sf::st_bbox()
}


get_river_surface <- function(bbox, river_centerline, crs,
                              force_download = FALSE) {
  river_surface <- CRiSp::osmdata_as_sf("natural", "water", bbox,
                                        force_download = force_download)
  dplyr::bind_rows(river_surface$osm_polygons,
                   river_surface$osm_multipolygons) |>
    sf::st_geometry() |>
    sf::st_as_sf() |>
    sf::st_make_valid() |>
    sf::st_crop(bbox) |>
    sf::st_transform(crs) |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_union()
}


get_segments <- function(corridor, network, river_centerline) {
  corridor_buffer <- sf::st_buffer(corridor, 100)
  network_filtered <- filter_network(network, corridor_buffer)
  CRiSp::delineate_segments(corridor, network_filtered, river_centerline)
}


filter_network <- function(network, target) {
  network |>
    tidygraph::activate("nodes") |>
    tidygraph::filter(sfnetworks::node_intersects(target)) |>
    # keep only the main connected component of the network
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::group_components() == 1)
}


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
  crs <- retry(CRiSp::get_utm_zone, bbox)
  city_boundary <- retry(CRiSp::get_osm_city_boundary, bbox, city_name,
                         force_download = TRUE) |>
    sf::st_transform(crs)
  river_centerline <- retry(get_river_centerline, bbox, river_name, crs,
                            force_download = TRUE)
  aoi <- get_aoi(bbox, river_centerline, buffer = 2000)
  river_surface <- retry(get_river_surface, aoi, river_centerline, crs,
                         force_download = TRUE)
  streets <- retry(CRiSp::get_osm_streets, aoi, crs)
  railways <- retry(CRiSp::get_osm_railways, aoi, crs)
  dem <- CRiSp::get_dem(aoi) |> terra::project(paste0("EPSG:", crs))

  # Compute valley, spatial network, corridor and segments
  valley <- CRiSp::get_valley(dem, c(river_centerline, river_surface))
  network <- dplyr::bind_rows(streets, railways) |>
    CRiSp::as_network()
  corridor <- CRiSp::delineate_corridor(network, river_centerline, river_surface,
                                        bbox = sf::st_transform(aoi, crs), dem = dem,
                                        capping_method = "shortest-path")

  segments <- get_segments(corridor, network, river_centerline)
  corridor_1 <- CRiSp::delineate_corridor(network, river_centerline, river_surface,
                                          bbox = sf::st_transform(aoi, crs), dem = dem,
                                          capping_method = "shortest-path", max_iterations = 1)
  segments_1 <- get_segments(corridor, network, river_centerline)

  list(
    aoi = aoi,
    city_boundary = city_boundary,
    river_centerline = river_centerline,
    river_surface = river_surface,
    streets = streets,
    railways = railways,
    dem = dem,
    valley = valley,
    network = network,
    corridor = corridor,
    segments = segments,
    corridor_1 = corridor_1,
    segments_1 = segments_1
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
  c(x$river_centerline, x$river_surface) |>
    save_fig(x$aoi, city_name, river_name, "river", col = "#2e809e",
             main = paste0(city_name, ", ", river_name))
  sf::st_as_sf(x$network, "edges") |>
    save_fig(x$aoi, city_name, river_name, "network", col = "black", lwd = 0.5,
             main = paste0(city_name, ", ", river_name))
  save_fig(x$dem, x$aoi, city_name, river_name, "dem",
           main = paste0(city_name, ", ", river_name))
  save_fig(x$valley, x$aoi, city_name, river_name, "valley", col = "gold",
           main = paste0(city_name, ", ", river_name))
  save_fig(x$corridor, x$aoi, city_name, river_name, "corridor",
           border = "orange", col = NA, lwd = 2,
           main = paste0(city_name, ", ", river_name))
  save_fig(x$segments, x$aoi, city_name, river_name, "segments",
           border = "orange", col = NA,
           main = paste0(city_name, ", ", river_name))
}


# Delineate all corridors
delineations <- vector("list", length = nrow(cr))
for (i in 1:nrow(cr)) {
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
