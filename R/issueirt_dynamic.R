#' One-Shot Dynamic IssueIRT Model Fitting
#'
#' This is a convenience function that wraps the entire workflow for fitting a dynamic IssueIRT model.
#' It handles preprocessing, initial value estimation, model fitting, and post-processing
#' in a single function call.
#'
#' @param votes_list A list of vote matrices, one for each time period/congress.
#'   Each matrix should have rows as legislators and columns as roll calls.
#' @param issue_list A list of issue code objects (from \code{make_issue_code}), one for each period.
#' @param legis_list A list of legislator data frames, one for each period.
#' @param colname_legis Column name in legis_list containing legislator IDs.
#' @param colname_party Optional column name in legis_list containing party codes.
#' @param bills_list Optional list of bill data frames, one for each period.
#' @param colname_bills Optional column name in bills_list containing bill IDs.
#' @param term_name Optional vector of term/period names (e.g., c("H52", "H53", "H54")).
#' @param left_code Code for yea/left votes (default: 1).
#' @param right_code Code for nay/right votes (default: 0).
#' @param missing_code Code(s) for missing votes (default: c(NA, 2)).
#' @param notInLegis_code Code for legislators not in chamber (default: 9).
#' @param anchors_name Optional vector of legislator IDs to use as anchors for post-processing.
#' @param liberal_code Party code for the liberal/left party (used for constraint selection).
#' @param conservative_code Party code for the conservative/right party (used for constraint selection).
#' @param a Hyperparameter for the prior of rho (default: 0.01).
#' @param b Hyperparameter for the prior of theta (default: 0.001).
#' @param rho_init Initial value for rho (default: 10).
#' @param sd_dynamic Standard deviation for dynamic prior (default: 0.01).
#' @param chains Number of MCMC chains (default: 2).
#' @param iter Total number of iterations per chain (default: 1000).
#' @param warmup Number of warmup iterations per chain (default: 500).
#' @param thin Thinning rate (default: 1).
#' @param seed Random seed (default: 1).
#' @param ideal_maxiter Maximum iterations for initial Bayesian IRT fit (default: 5000).
#' @param ideal_burnin Burnin for initial Bayesian IRT fit (default: 4800).
#' @param verbose Logical, whether to print progress messages (default: TRUE).
#' @param ... Additional arguments passed to \code{rstan::sampling}.
#'
#' @return An \code{issueirt_dynamic_fit} object containing:
#'   \item{stan_fit}{The Stan fit object}
#'   \item{stan_input}{The Stan input object}
#'   \item{posterior_summary}{The posterior summary with post-processed estimates}
#'   \item{ideal_points}{Issue-specific ideal points}
#'   \item{constraints}{The constraints used for post-processing}
#'   \item{dynamic_rollcall}{The dynamic rollcall object}
#'   \item{legis_data}{The processed legislator data}
#'   \item{call}{The function call}
#'
#' @examples
#' \dontrun{
#' # Using the included 1890s U.S. House data
#' data("us1890s_votes")
#' data("us1890s_issue")
#' data("us1890s_legis")
#' data("us1890s_bills")
#'
#' # Prepare issue codes
#' issue_codes <- lapply(us1890s_issue, function(x) {
#'   make_issue_code(x$issue_label)
#' })
#'
#' # Fit the dynamic model
#' fit <- issueirt_dynamic(
#'   votes_list = us1890s_votes,
#'   issue_list = issue_codes,
#'   legis_list = us1890s_legis,
#'   colname_legis = "icpsr",
#'   colname_party = "party_name",
#'   bills_list = us1890s_bills,
#'   colname_bills = "rollnumber",
#'   term_name = c("H52", "H53", "H54"),
#'   chains = 2,
#'   iter = 100,
#'   warmup = 50
#' )
#'
#' # View summary
#' summary(fit)
#' }
#'
#' @seealso
#' \code{\link{issueirt_dynamic_stan}} for the underlying Stan model,
#' \code{\link{make_dynamic_stan_input}} for manual input preparation,
#' \code{\link{make_dynamic_rollcall}} for creating dynamic rollcall objects
#'
#' @importFrom pscl ideal
#' @importFrom purrr map
#' @importFrom stringr str_subset
#' @export
issueirt_dynamic <- function(
    votes_list,
    issue_list,
    legis_list,
    colname_legis,
    colname_party = NULL,
    bills_list = NULL,
    colname_bills = NULL,
    term_name = NULL,
    left_code = 1,
    right_code = 0,
    missing_code = c(NA, 2),
    notInLegis_code = 9,
    anchors_name = NULL,
    liberal_code = "Democrat",
    conservative_code = "Republican",
    a = 0.01,
    b = 0.001,
    rho_init = 10,
    sd_dynamic = 0.01,
    chains = 2,
    iter = 1000,
    warmup = 500,
    thin = 1,
    seed = 1,
    ideal_maxiter = 5000,
    ideal_burnin = 4800,
    verbose = TRUE,
    ...) {

  call <- match.call()

  # Set default term names
  if (is.null(term_name)) {
    term_name <- paste0("T", seq_along(votes_list))
  }

  if (verbose) message("Step 1/7: Creating dynamic rollcall object...")

  # Create dynamic rollcall
  dynamic_rc <- make_dynamic_rollcall(
    votes_list = votes_list,
    issue_list = issue_list,
    legis_list = legis_list,
    colname_legis = colname_legis,
    colname_party = colname_party,
    bills_list = bills_list,
    colname_bills = colname_bills,
    term_name = term_name,
    left_code = left_code,
    right_code = right_code,
    missing_code = missing_code,
    notInLegis_code = notInLegis_code
  )

  if (verbose) message("Step 2/7: Fitting initial Bayesian IRT model...")

  # Fit initial Bayesian IRT
  set.seed(seed)
  ideal_fit <- suppressMessages(suppressWarnings(
    pscl::ideal(
      dynamic_rc$rollcall,
      dropList = list(lop = 0, legisMin = 0),
      priors = NULL, startvals = "eigen",
      d = 2, maxiter = ideal_maxiter, thin = 1, burnin = ideal_burnin,
      impute = FALSE, normalize = FALSE,
      store.item = TRUE, file = NULL, verbose = FALSE
    )
  ))

  if (verbose) message("Step 3/7: Finding constraints...")

  # Find constraints
  if (!is.null(colname_party)) {
    pol_rc1 <- find_pol_rc_horizontal(
      dynamic_rc$rollcall,
      party_code_col = colname_party,
      liberal_code = liberal_code,
      conservative_code = conservative_code,
      na_threshold = 0.5
    )
    pol_rc2 <- find_pol_rc_vertical(
      ideal_fit, dynamic_rc$rollcall, pol_rc1,
      party_code_col = colname_party,
      liberal_code = liberal_code,
      conservative_code = conservative_code,
      na_threshold = 0.5, lop_threshold = 0.1
    )
    const_ls <- find_constraints(
      ideal_fit, dynamic_rc$rollcall,
      pol_rc1 = pol_rc1, pol_rc2 = pol_rc2,
      party_code_col = colname_party,
      left_party_code = liberal_code,
      as_list = TRUE
    )
  } else {
    # Use simple constraints based on extreme ideal points
    xbar <- ideal_fit$xbar
    idx_left <- which.min(xbar[, 1])
    idx_right <- which.max(xbar[, 1])
    idx_top <- which.max(xbar[, 2])

    legis_names <- rownames(xbar)
    const_ls <- list()
    const_ls[[legis_names[idx_left]]] <- c(xbar[idx_left, 1], xbar[idx_left, 2])
    const_ls[[legis_names[idx_right]]] <- c(xbar[idx_right, 1], xbar[idx_right, 2])
    const_ls[[legis_names[idx_top]]] <- c(xbar[idx_top, 1], xbar[idx_top, 2])
  }

  if (verbose) message("Step 4/7: Post-processing initial fit...")

  # Post-process initial fit
  invisible(capture.output({
    ideal_pp <- pscl::postProcess(ideal_fit, constraints = const_ls)
  }))

  if (verbose) message("Step 5/7: Creating Stan input...")

  # Create dynamic Stan input
  dynamic_stan_input <- make_dynamic_stan_input(
    dynamic_rollcall = dynamic_rc,
    ideal = ideal_pp,
    a = a,
    b = b,
    rho_init = rho_init,
    sd_dynamic = sd_dynamic
  )

  if (verbose) message("Step 6/7: Fitting Dynamic IssueIRT model (this may take a while)...")

  # Fit Stan model
  stan_fit <- issueirt_dynamic_stan(
    data = dynamic_stan_input$data,
    init = rep(list(dynamic_stan_input$init), chains),
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    seed = seed,
    ...
  )

  if (verbose) message("Step 7/7: Post-processing results...")

  # Get dynamic constraints for post-processing
  if (is.null(anchors_name)) {
    anchors_name <- purrr::map(names(const_ls), ~stringr::str_subset(dynamic_stan_input$misc$legis_term, .x) %>% .[1]) |> unlist()
  }
  const_dynamic <- get_dynamic_constraints(stan_fit, dynamic_stan_input, anchors_name)

  # Generate posterior summary
  posterior_summary <- make_posterior_summary_postprocessed(
    stan_fit = stan_fit,
    constraints = const_dynamic,
    issue_label = dynamic_stan_input$misc$issue_code_df$codebook$label,
    rc_label = dynamic_stan_input$misc$bills_df$term_rollnumber,
    legis_label = dynamic_stan_input$misc$legis_term,
    missing_label = NULL
  )

  # Create posterior data frame for group assignment
  posterior_df <- tibble(
    legis_term = dynamic_stan_input$misc$legis_term,
    ideal_point_1d = posterior_summary$x_postprocessed[posterior_summary$x_postprocessed$dimension == 1, ]$mean,
    ideal_point_2d = posterior_summary$x_postprocessed[posterior_summary$x_postprocessed$dimension == 2, ]$mean
  )
  posterior_df <- posterior_df |>
    tidyr::separate(.data$legis_term, into = c("legis_id", "term"), sep = "_", remove = FALSE)

  # Get legislator groups
  legis_group <- NULL
  if (!is.null(colname_party)) {
    group_df <- dynamic_stan_input$misc$legis_df[, c(colname_legis, colname_party, "congress")]
    colnames(group_df) <- c("legis_id", "group", "term")
    group_df$legis_id <- as.character(group_df$legis_id)
    group_df$term <- as.character(match(group_df$term, sort(unique(group_df$term))))
    posterior_df <- dplyr::left_join(posterior_df, group_df, by = c("legis_id", "term"))
    legis_group <- posterior_df$group
  }

  # Get ideal points
  ideal_points <- get_ideal_points(
    stan_fit = stan_fit,
    issue_label = dynamic_stan_input$misc$issue_code_df$codebook$label,
    legis_label = dynamic_stan_input$misc$legis_term,
    legis_group = legis_group,
    dynamic = TRUE
  )

  if (verbose) message("Done!")

  # Create result object
  result <- list(
    stan_fit = stan_fit,
    stan_input = dynamic_stan_input,
    posterior_summary = posterior_summary,
    ideal_points = ideal_points,
    constraints = const_dynamic,
    dynamic_rollcall = dynamic_rc,
    legis_data = posterior_df,
    call = call
  )

  class(result) <- c("issueirt_dynamic_fit", "list")

  return(result)
}

#' Print Method for issueirt_dynamic_fit Objects
#'
#' @param x An \code{issueirt_dynamic_fit} object.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object.
#' @export
print.issueirt_dynamic_fit <- function(x, ...) {
  cat("Dynamic IssueIRT Model Fit\n")
  cat("==========================\n\n")

  cat("Data:\n")
  cat("  Legislator-terms:", x$stan_input$data$J, "\n")
  cat("  Roll calls:", x$stan_input$data$M, "\n")
  cat("  Issue categories:", x$stan_input$data$K, "\n")
  cat("  Observations:", x$stan_input$data$N_obs, "\n\n")

  cat("Model:\n")
  cat("  Chains:", x$stan_fit@sim$chains, "\n")
  cat("  Iterations per chain:", x$stan_fit@sim$iter, "\n")
  cat("  Warmup:", x$stan_fit@sim$warmup, "\n\n")

  cat("Issue categories:\n")
  for (i in seq_len(nrow(x$stan_input$misc$issue_code_df$codebook))) {
    cat("  ", x$stan_input$misc$issue_code_df$codebook$code[i], ": ",
        x$stan_input$misc$issue_code_df$codebook$label[i], "\n", sep = "")
  }

  invisible(x)
}

#' Summary Method for issueirt_dynamic_fit Objects
#'
#' @param object An \code{issueirt_dynamic_fit} object.
#' @param ... Additional arguments (not used).
#' @return A list containing summary information.
#' @importFrom rstan get_elapsed_time
#' @export
summary.issueirt_dynamic_fit <- function(object, ...) {
  # Compute elapsed time
  elapsed_times <- rstan::get_elapsed_time(object$stan_fit)
  total_time <- sum(elapsed_times)

  # Get theta summary
  theta_summary <- object$posterior_summary$theta_postprocessed

  cat("Dynamic IssueIRT Model Summary\n")
  cat("==============================\n\n")

  cat("Data Summary:\n")
  cat("  Legislator-terms:", object$stan_input$data$J, "\n")
  cat("  Roll calls:", object$stan_input$data$M, "\n")
  cat("  Issue categories:", object$stan_input$data$K, "\n")
  cat("  Total observations:", object$stan_input$data$N_obs, "\n\n")

  cat("Computation:\n")
  cat("  Total time:", round(total_time / 60, 2), "minutes\n\n")

  cat("Issue-Specific Axes (theta):\n")
  print(theta_summary[, c("issue_index", "issue_label", "mean", "sd", "2.5%", "97.5%")], n = Inf)
  cat("\n")

  cat("Use plot() to visualize the results.\n")
  cat("Use $ideal_points to access issue-specific ideal points.\n")
  cat("Use $posterior_summary for full posterior statistics.\n")

  invisible(list(
    n_legis_terms = object$stan_input$data$J,
    n_rc = object$stan_input$data$M,
    n_issues = object$stan_input$data$K,
    n_obs = object$stan_input$data$N_obs,
    elapsed_time = total_time,
    theta_summary = theta_summary
  ))
}

#' Plot Method for issueirt_dynamic_fit Objects
#'
#' @param x An \code{issueirt_dynamic_fit} object.
#' @param type Type of plot to create. Options are "ideal_points" (default),
#'   "axes", or "issue_specific".
#' @param ... Additional arguments passed to the underlying plot functions.
#' @return A ggplot object or list of ggplot objects.
#' @export
plot.issueirt_dynamic_fit <- function(x, type = c("ideal_points", "axes", "issue_specific"), ...) {
  type <- match.arg(type)

  group <- if ("group" %in% colnames(x$legis_data)) {
    x$legis_data$group
  } else {
    NULL
  }

  if (type == "ideal_points") {
    plot_ideal(
      ideal_point_1d = x$legis_data$ideal_point_1d,
      ideal_point_2d = x$legis_data$ideal_point_2d,
      group = group,
      p.title = "Dynamic IssueIRT Ideal Points",
      ...
    )
  } else if (type == "axes") {
    plot_issueaxis(
      stan_input = x$stan_input,
      posterior_summary = x$posterior_summary,
      group = group,
      p.title = "Issue-Specific Axes",
      plot.each.congress = TRUE,
      ...
    )
  } else if (type == "issue_specific") {
    plot_issueirt(
      issueirt = x$ideal_points,
      p.title = "Issue-Specific Ideal Points",
      ...
    )
  }
}
