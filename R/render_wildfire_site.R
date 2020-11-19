#' Render Wildfire Website
#'
#' @return
#' @export
#'
#' @examples
render_wildfire_site <- function() {
  rmarkdown::render_site(input = "inst/website")
}

