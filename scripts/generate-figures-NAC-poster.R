city_name <- "Bucharest"
river_name <- "Dâmbovița"
network_buffer <- 3500
dem_buffer <- 3500
buildings_buffer <- 200

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


get_segments <- function(corridor, network, river_centerline) {
  corridor_buffer <- sf::st_buffer(corridor, 100)
  network_filtered <- CRiSp:::filter_network(network, corridor_buffer)
  CRiSp::delineate_segments(corridor, network_filtered, river_centerline)
}

delineate <- function(city_name, river_name) {
  # Download datasets and setup
  bbox <- retry(CRiSp::get_osm_bb, city_name)
  crs <- retry(CRiSp::get_utm_zone, bbox)
  river <- retry(CRiSp::get_osm_river, bbox, river_name, crs = crs)
  river_centerline <- river$centerline
  river_surface <- river$surface
  aoi_network <- CRiSp::get_river_aoi(
    river, bbox, buffer_distance = network_buffer
  )
  streets <- retry(CRiSp::get_osm_streets, aoi_network, crs)
  railways <- retry(CRiSp::get_osm_railways, aoi_network, crs)
  aoi_buildings <- CRiSp::get_river_aoi(
    river, bbox, buffer_distance = buildings_buffer
  )
  buildings <- retry(CRiSp::get_osm_buildings, aoi_buildings, crs)
  aoi_dem <- sf::st_buffer(aoi_network, dem_buffer)
  dem <- CRiSp::get_dem(aoi_dem, crs = crs)

  # Compute valley and spatial network
  valley <- CRiSp::get_valley(dem, c(river_centerline, river_surface))
  network <- CRiSp::as_network(dplyr::bind_rows(streets, railways))

  # Run delineations
  corridor <- CRiSp::delineate_corridor(
    network, river_centerline, river_surface,
    aoi = CRiSp::reproject(aoi_network, crs), max_width = network_buffer,
    dem = dem, capping_method = "shortest-path"
  )
  segments <- get_segments(corridor, network, river_centerline)
  riverspace <- CRiSp::delineate_riverspace(buildings, river_surface)

  list(
    river_centerline = river_centerline,
    river_surface = river_surface,
    streets = streets,
    railways = railways,
    dem = dem,
    valley = valley,
    buildings = buildings,
    corridor = corridor,
    segments = segments,
    riverspace = riverspace
  )
}

make_fig <- function(x, city_name, river_name, type,
                     out_dir = file.path(".", "figures", "poster")) {
  # Create directory and build output file path
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  filename <- paste(city_name, river_name, type, sep = "-") |>
    paste("png", sep = ".")
  filepath <- file.path(out_dir, filename)

  # Create canvas
  png(filename = filepath, width = 5, height = 5, res = 600, units = "in",
      bg = NA)

  # Extract image boundaries from bounding box
  bbox <- sf::st_bbox(sf::st_buffer(x$corridor, 100))
  xlim <- bbox[c("xmin", "xmax")]
  ylim <- bbox[c("ymin", "ymax")]

  if (type == "valley") {
    # setup plot as for a sf object to have it exactly superimposable
    point <- sf::st_centroid(sf::st_as_sfc(bbox))
    plot(point, xlim = xlim, ylim = ylim)
    terra::plot(x$dem, col = RColorBrewer::brewer.pal(9, name = "PuBu"),
                add = TRUE, legend = FALSE)
    plot(x$valley, col = rgb(0, 0.3, 1, 0.7), border = FALSE, add = TRUE)
    plot(x$river_centerline, col = "blue", add = TRUE)
    plot(x$river_surface, col = "blue", border = "blue", add = TRUE)
  } else if (type == "corridor") {
    plot(x$river_centerline, col = "blue", xlim = xlim, ylim = ylim)
    plot(x$river_surface, col = "blue", border = "blue", add = TRUE)
    plot(x$streets, col = "black", lwd = 0.5,
         xlim = xlim, ylim = ylim, add = TRUE)
    plot(x$railways, col = "gray", lwd = 0.5,
         xlim = xlim, ylim = ylim, add = TRUE)
    plot(x$corridor, col = NA, border = "red", lwd = 2,
         xlim = xlim, ylim = ylim, add = TRUE)
    plot(x$segments, col = NA, border = "red",
         xlim = xlim, ylim = ylim, add = TRUE)
  } else if (type == "riverspace") {
    # Manual zoom in
    xlim <- c(423958, 429583)
    ylim <- c(4918234, 4921234)
    plot(x$river_centerline, col = "blue", xlim = xlim, ylim = ylim)
    plot(x$river_surface, col = "blue", border = "blue", add = TRUE)
    plot(x$buildings, col = "black", border = NA, add = TRUE)
    plot(x$riverspace, col = NA, border = "orange", lwd = 2, add = TRUE)
  }

  # Close figure
  dev.off()
}

delineation <- delineate(city_name, river_name)
make_fig(delineation, city_name, river_name, "valley")
make_fig(delineation, city_name, river_name, "corridor")
make_fig(delineation, city_name, river_name, "riverspace")
