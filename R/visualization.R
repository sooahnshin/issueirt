#' Plot the Ideal Point Estimates
#'
#' This function creates a ggplot object to visualize ideal point estimates in a two-dimensional space, distinguishing groups by color and shape.
#'
#' @param ideal_point_1d A vector of ideal point estimates in the horizontal dimension.
#' @param ideal_point_2d A vector of ideal point estimates in the vertical dimension.
#' @param group A vector of group membership.
#' @param p.title The title of the plot (optional).
#' @param breaks.group A vector of group names for legend breaks (optional).
#' @param values.shape A vector of shape values for each group (optional).
#' @param values.color A vector of color values for each group (optional).
#' @return A ggplot object representing the ideal point estimates.
#' @importFrom ggplot2 ggplot geom_point coord_equal labs scale_shape_manual scale_color_manual aes
#' @importFrom dplyr tibble
#' @export
plot_ideal <- function(ideal_point_1d, ideal_point_2d, group,
                       p.title = NULL, breaks.group = NULL, values.shape = NULL, values.color = NULL) {

  df <- tibble(ideal_point_1d = ideal_point_1d, ideal_point_2d = ideal_point_2d, group = group)
  p <- df |>
    ggplot() +
    geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, color = as.factor(group), shape = as.factor(group))) +
    coord_equal(ratio = sum(abs(range(df$ideal_point_1d)))/sum(abs(range(df$ideal_point_2d))))  +
    labs(x = "Dimension 1", y = "Dimension 2", title = p.title)
  if(!is.null(values.shape)) {
    p <- p + scale_shape_manual(name = "Group", breaks = breaks.group, values = values.shape)
  }
  if(!is.null(values.color)) {
    p <- p + scale_color_manual(name = "Group", breaks = breaks.group, values = values.color)
  }
  return(p)
}
#' Get Congress Years
#'
#' This function calculates the years during which a specified number of the U.S. Congress was in session.
#' The calculation is based on the fact that the 1st Congress started in 1789, and each Congress spans two years.
#'
#' @param congress_number The number of the U.S. Congress.
#' @return A string representing the years during which the specified Congress was in session.
#' @export
#' @examples
#' get_congress_years(117) # Returns "2021 - 2022" for the 117th Congress
get_congress_years <- function(congress_number) {
  # The 1st Congress started in 1789
  start_year_of_first_congress <- 1789

  # Calculate the start year of the given Congress
  start_year <- start_year_of_first_congress + (congress_number - 1) * 2

  # The end year is one year less than the start year of the next Congress
  end_year <- start_year + 1

  # Return the years as a string
  return(paste(start_year, "-", end_year))
}
