#' Generate Label for Missing Indicator from Roll Call Data
#'
#' This function creates labels for missing indicators in roll call data.
#' It identifies missing votes and assigns labels based on legislator and bill IDs.
#'
#' @param rollcall A roll call object (output of `pscl::rollcall`).
#'
#' @return A vector of labels for missing indicators, formatted as 'legis_id bill_id'.
#' @importFrom dplyr mutate pull as_tibble
#' @importFrom rlang .data
#' @export
make_missing_indicator <- function(rollcall) {
  votes <- rollcall$votes
  yea_code <- rollcall$codes$yea
  nay_code <- rollcall$codes$nay
  votes[votes == yea_code] <- 1
  votes[votes == nay_code] <- 0
  votes[!votes %in% c(yea_code, nay_code)] <- NA
  na_indices <- which(is.na(votes), arr.ind = TRUE) |> as_tibble()
  na_indices <- na_indices |>
    mutate(legis_id = rownames(votes)[na_indices$row],
           bill_id = colnames(votes)[na_indices$col])
  missing_label <- na_indices |>
    mutate(missing_label = paste(.data$legis_id, .data$bill_id)) |>
    pull(.data$missing_label)
  return(missing_label)
}
#' Generate Posterior Summary of Stan Output
#'
#' This function processes the output from a Stan model and generates a summary of posterior distributions.
#' It organizes the summary into different categories based on the parameters of the model.
#'
#' @param stan_fit A Stan fit object.
#' @param issue_label A vector of issue labels.
#' @param rc_label A vector of roll call labels.
#' @param legis_label A vector of legislator labels.
#' @param missing_label A vector of missing indicator labels.
#'
#' @return A list of tibbles containing the posterior summary for various model parameters, including theta,
#'   theta's auxiliary variables, u, u's auxiliary variables, x, other bill parameters, and imputed missing data.
#' @importFrom rstan summary
#' @importFrom dplyr filter pull mutate relocate as_tibble rename
#' @importFrom tidyr separate
#' @importFrom stringr str_remove str_detect
#' @importFrom rlang .data
#' @export
make_posterior_summary <- function(stan_fit, issue_label = NULL, rc_label = NULL, legis_label = NULL, missing_label = NULL) {
  posterior_summary <- rstan::summary(stan_fit)
  posterior_df <- posterior_summary$summary |>
    as_tibble() |>
    mutate(parameter = rownames(posterior_summary$summary)) |>
    mutate(stan_fit_label = .data$parameter)
  posterior_df <- posterior_df |>
    separate(.data$parameter, into = c("parameter", "dimension"), sep = "\\[", extra = "merge", fill = "right") |>
    mutate(dimension = str_remove(.data$dimension, "\\]"))

  theta_df <- posterior_df |>
    filter(str_detect(.data$parameter, "theta")) |>
    rename(issue_index = .data$dimension) |>
    mutate(issue_index = as.integer(.data$issue_index))
  k <- theta_df |>
    pull(.data$issue_index) |>
    unique() |>
    length()

  if (!is.null(issue_label)) {
    theta_df <- theta_df |>
      left_join(tibble(issue_index = 1:k, issue_label = issue_label), by = "issue_index")
  } else {
    theta_df <- theta_df |>
      mutate(issue_label = paste0("Issue ", .data$issue_index))
  }
  theta_df <- theta_df |>
    relocate(.data$parameter, .data$issue_index, .data$issue_label)

  theta <- theta_df |>
    filter(.data$parameter == "theta")
  theta_aux <- theta_df |>
    filter(.data$parameter != "theta")

  u_df <- posterior_df |>
    filter(str_detect(.data$parameter, "u")) |>
    rename(rc_index = .data$dimension) |>
    mutate(rc_index = as.integer(.data$rc_index))
  m <- u_df |>
    pull(.data$rc_index) |>
    unique() |>
    length()

  if (!is.null(rc_label)) {
    u_df <- u_df |>
      left_join(tibble(rc_index = 1:m, rc_label = rc_label), by = "rc_index")
  } else {
    u_df <- u_df |>
      mutate(rc_label = paste0("Roll Call ", .data$rc_index))
  }
  u_df <- u_df |>
    relocate(.data$parameter, .data$rc_index, .data$rc_label)

  u <- u_df |>
    filter(.data$parameter == "u")
  u_aux <- u_df |>
    filter(.data$parameter != "u")

  x <- posterior_df |>
    filter(.data$parameter == "x") |>
    separate(.data$dimension, into = c("legis_index", "dimension"), sep = ",") |>
    mutate(legis_index = as.integer(.data$legis_index), dimension = as.integer(.data$dimension))

  n <- x |>
    pull(.data$legis_index) |>
    unique() |>
    length()

  if (!is.null(legis_label)) {
    x <- x |>
      left_join(tibble(legis_index = 1:n, legis_label = legis_label), by = "legis_index")
  } else {
    x <- x |>
      mutate(legis_label = paste0("Legislator ", .data$legis_index))
  }
  x <- x |>
    relocate(.data$parameter, .data$dimension, .data$legis_index, legis_label)

  other_bill <- posterior_df |>
    filter(.data$parameter %in% c("alpha", "w", "rho")) |>
    rename(rc_index = .data$dimension) |>
    mutate(rc_index = as.integer(.data$rc_index))

  if (!is.null(rc_label)) {
    other_bill <- other_bill |>
      left_join(tibble(rc_index = 1:m, rc_label = rc_label), by = "rc_index")
  } else {
    other_bill <- other_bill |>
      mutate(rc_label = paste0("Roll Call ", .data$rc_index))
  }
  other_bill <- other_bill |>
    relocate(.data$parameter, .data$rc_index, .data$rc_label)

  y_imputed <- posterior_df |>
    filter(.data$parameter == "y_mis") |>
    rename(missing_index = .data$dimension) |>
    mutate(missing_index = as.integer(.data$missing_index))
  n_mis <- length(y_imputed$missing_index)

  if (!is.null(missing_label)) {
    y_imputed <- y_imputed |>
      left_join(tibble(missing_index = 1:n_mis, missing_label = missing_label), by = "missing_index")
  } else {
    y_imputed <- y_imputed |>
      mutate(missing_label = paste0("Legislator ", .data$missing_index))
  }
  y_imputed <- y_imputed |>
    relocate(.data$parameter, .data$missing_index, .data$missing_label)

  res <- list(
    theta = theta, theta_aux = theta_aux,
    u = u, u_aux = u_aux,
    x = x,
    other_bill = other_bill,
    y_imputed = y_imputed
  )
  return(res)
}
