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
