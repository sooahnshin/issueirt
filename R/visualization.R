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
#' Plot the Issue Specific Axes
#'
#' This function creates a ggplot object to visualize issue specific axes using the stan input and the output of the \code{make_posterior_summary_postprocessed} function.
#'
#' @param stan_input An issueirt_stan_input object.
#' @param posterior_summary An issueirt_posterior_summary object.
#' @param p.title The title of the plot (optional).
#' @param group A vector of group membership (optional).
#' @param breaks.group A vector of group names for legend breaks (optional).
#' @param values.shape A vector of shape values for each group (optional).
#' @param values.color A vector of color values for each group (optional).
#' @return A list of ggplot objects representing the issue specific axes.
#' @importFrom ggplot2 ggplot geom_point coord_equal labs scale_shape_manual scale_color_manual aes
#' @importFrom dplyr tibble
#' @export
plot_issueaxis <- function(stan_input, posterior_summary,
                       p.title = NULL,
                       group = NULL, breaks.group = NULL, values.shape = NULL, values.color = NULL) {
  if(inherits(stan_input, "issueirt_stan_input") == FALSE) {
    stop("stan_input must be a issueirt_stan_input object")
  }
  if(inherits(posterior_summary, "issueirt_posterior_summary") == FALSE) {
    stop("posterior_summary must be a issueirt_posterior_summary object")
  }
  z_vec <- stan_input$data$z
  df <- tibble(ideal_point_1d = posterior_summary$x_postprocessed |> filter(dimension == 1) |> pull(mean),
               ideal_point_2d = posterior_summary$x_postprocessed |> filter(dimension == 2) |> pull(mean),
               group = group)

  if(is.null(group)) {
    p <- df |>
      ggplot() +
      geom_point(aes(x = ideal_point_1d, y = ideal_point_2d), color = "grey") +
      coord_equal(ratio = sum(abs(range(df$ideal_point_1d)))/sum(abs(range(df$ideal_point_2d))))  +
      labs(x = "Dimension 1", y = "Dimension 2")
  } else {
    p <- df |>
      ggplot() +
      geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, color = as.factor(group), shape = as.factor(group))) +
      coord_equal(ratio = sum(abs(range(df$ideal_point_1d)))/sum(abs(range(df$ideal_point_2d))))  +
      labs(x = "Dimension 1", y = "Dimension 2")
  }
  if(is.null(p.title)) {
    p.title <- "Issue Specific Axis"
  }

  if(!is.null(values.shape)) {
    p <- p + scale_shape_manual(name = "Group", breaks = breaks.group, values = values.shape)
  }
  if(!is.null(values.color)) {
    p <- p + scale_color_manual(name = "Group", breaks = breaks.group, values = values.color)
  }

  theta_df <- posterior_summary$theta_postprocessed
  u_df <- posterior_summary$u_postprocessed |>
    mutate(issue_index = z_vec)
  issue_label <- theta_df |>
    arrange(issue_index) |>
    pull(issue_label)
  p_ls <- list()
  for(z in 1:max(z_vec)) {
    theta_z <- theta_df |> filter(.data$issue_index == z) |>
      mutate(tan_z = tan(.data$mean),
             cos_z = cos(.data$mean),
             sin_z = sin(.data$mean))

    u_z <- u_df |> filter(.data$issue_index == z)  |>
      mutate(tan_u = tan(.data$mean),
             cos_u = cos(.data$mean),
             sin_u = sin(.data$mean))
    p_z <- p +
      geom_abline(aes(slope = tan_u, intercept = 0),
                  alpha = 0.1,
                  data = u_z) +
      geom_segment(lineend = "butt", linejoin = "round",
                   color = "orange", alpha = 0.5,
                   arrow = arrow(length = unit(0.05, "inches")),
                   aes(x = 0, y = 0, xend = cos_u, yend = sin_u),
                   data = u_z) +
      geom_abline(aes(slope = tan_z, intercept = 0), linewidth = 1.4, data = theta_z) +
      geom_segment(lineend = "butt", linejoin = "round",
                   color = "darkorange", linewidth = 1.5,
                   arrow = arrow(length = unit(0.2, "inches")),
                   aes(x = 0, y = 0, xend = cos_z, yend = sin_z), data = theta_z) +
      labs(title = paste0(p.title, " (", issue_label[z], ")"))
    p_ls[[z]] <- p_z
  }
  names(p_ls) <- issue_label

  return(p_ls)
}

#' Plot the Issue Specific Ideal Points
#'
#' This function creates a ggplot object to visualize issue specific ideal points using the output of the \code{get_ideal_points} function.
#'
#' @param issueirt An issueirt_ideal_points object.
#' @param p.title The title of the plot (optional).
#' @param breaks.group A vector of group names for legend breaks (optional).
#' @param values.shape A vector of shape values for each group (optional).
#' @param values.color A vector of color values for each group (optional).
#' @return A ggplot object representing the issue specific ideal points.
#' @importFrom ggplot2 ggplot geom_point labs scale_shape_manual scale_color_manual aes
#' @importFrom dplyr select distinct arrange mutate
#' @export
plot_issueirt <- function(issueirt,
                          p.title = NULL, breaks.group = NULL, values.shape = NULL, values.color = NULL) {
  if(inherits(issueirt, "issueirt_ideal_points") == FALSE) {
    stop("issueirt must be a issueirt_ideal_points object")
  }
  issue_sorted <- issueirt |>
    select(.data$issue_index, .data$issue_label) |>
    distinct() |>
    arrange(desc(.data$issue_index))
  if("legis_group" %in% colnames(issueirt)) {
    p <- issueirt |>
      mutate(issue_index_plot = factor(issue_index,
                                       levels = issue_sorted$issue_index,
                                       labels = issue_sorted$issue_label)) |>
      mutate(Group = as.factor(legis_group)) |>
      ggplot(aes(x = mean, y = issue_index_plot, color = Group, shape = Group)) +
      geom_point(size = 3) +
      labs(x = "Issue Specific Ideal Point", y = "Issue", title = p.title)
  } else {
    p <- issueirt |>
      mutate(issue_index_plot = factor(issue_index,
                                       levels = issue_sorted$issue_index,
                                       labels = issue_sorted$issue_label)) |>
      ggplot(aes(x = mean, y = issue_index_plot)) +
      geom_point(size = 3) +
      labs(x = "Issue Specific Ideal Point", y = "Issue", title = p.title)
  }
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
