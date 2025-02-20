# Input city and river
city_name <- "Bucharest"
river_name <- "Dâmbovița"


get_river_centerline <- function(bbox, river_name, crs) {
  river_centerline <- CRiSp::osmdata_as_sf("waterway", "river", bbox)
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


get_river_surface <- function(bbox, river_centerline, crs) {
  river_surface <- CRiSp::osmdata_as_sf("natural", "water", bbox)
  dplyr::bind_rows(river_surface$osm_polygons,
                   river_surface$osm_multipolygons) |>
    sf::st_geometry() |>
    sf::st_as_sf() |>
    sf::st_crop(bbox) |>
    sf::st_transform(crs) |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_union()
}


get_segments <- function(corridor, network, river_centerline) {
  corridor_buffer <- sf::st_buffer(corridor, 100)
  network_filtered <- filter_network(network, corridor_buffer)
  CRiSp::get_segments(corridor, network_filtered, river_centerline)
}


filter_network <- function(network, target) {
  network |>
    tidygraph::activate("nodes") |>
    tidygraph::filter(sfnetworks::node_intersects(target)) |>
    # keep only the main connected component of the network
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::group_components() == 1)
}


save_fig <- function(x, bbox, city_name, label, col, border = col,
                     out_dir = file.path(".", "figures", "generate-corridor")) {
  # Create directory and build output file path
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  filename <- paste(city_name, label, sep = "-") |>
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
         xlim = xlim, ylim = ylim)
  } else if (inherits(x, "SpatRaster")) {
    # setup plot as for a sf object to have it exactly superimposable
    point <- sf::st_as_sfc(bbox_repr) |>
      sf::st_centroid()
    plot(point, xlim = xlim, ylim = ylim)
    terra::plot(dem, add = TRUE, legend = FALSE)
  }

  # Close figure
  dev.off()
}


# Download datasets and setup
bbox <- CRiSp::get_osm_bb(city_name)
crs <- CRiSp::get_utm_zone(bbox)
river_centerline <- get_river_centerline(bbox, river_name, crs)
aoi <- get_aoi(bbox, river_centerline, buffer = 2000)
river_surface <- get_river_surface(aoi, river_centerline, crs)
streets <- CRiSp::get_osm_streets(aoi, crs)
railways <- CRiSp::get_osm_railways(aoi, crs)
dem <- CRiSp::get_dem(aoi, crs = crs)

# Compute valley, spatial network, corridor and segments
valley <- CRiSp::get_valley(dem, c(river_centerline, river_surface))
network <- dplyr::bind_rows(streets, railways) |>
  CRiSp::as_network()
corridor <- CRiSp::get_corridor(network, river_centerline, river_surface,
                                bbox = sf::st_transform(aoi, crs), dem = dem,
                                capping_method = "shortest-path")
segments <- get_segments(corridor, network, river_centerline)


# Save output
c(river_centerline, river_surface) |>
  save_fig(aoi, city_name, "river", col = "blue")
sf::st_as_sf(network, "edges") |>
  save_fig(aoi, city_name, "network", col = "black")
save_fig(dem, aoi, city_name, "dem")
save_fig(valley, aoi, city_name, "valley", col = "red")
save_fig(corridor, aoi, city_name, "corridor", col = "yellow")
save_fig(segments, aoi, city_name, "segments", col = "yellow", border = "black")
