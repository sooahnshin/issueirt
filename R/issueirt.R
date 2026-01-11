#' One-Shot IssueIRT Model Fitting
#'
#' This is a convenience function that wraps the entire workflow for fitting an IssueIRT model.
#' It handles preprocessing, initial value estimation, model fitting, and post-processing
#' in a single function call.
#'
#' @param votes A matrix of roll call votes where rows are legislators and columns are roll calls.
#'   Values should be 1 for yea, 0 for nay, and NA for missing.
#' @param issue_codes A character or numeric vector of issue codes for each roll call.
#'   Length must equal the number of columns in `votes`.
#' @param legis_data Optional data frame containing legislator information.
#'   Must have the same number of rows as `votes`. If provided and `party_col` is specified,
#'   it will be used for automatic constraint selection.
#' @param party_col Optional column name in `legis_data` containing party codes.
#'   Used for automatic constraint selection.
#' @param liberal_code Party code for the liberal/left party (used for constraint selection).
#' @param conservative_code Party code for the conservative/right party (used for constraint selection).
#' @param top_party_code Optional party code for the party expected to be at the top of dimension 2.
#' @param constraints Optional list of 3 constraints for post-processing. If NULL, constraints
#'   will be automatically determined based on party information.
#' @param lop Minimum level of support threshold for filtering votes (default: 0).
#' @param minvotes Minimum number of votes a legislator must cast (default: 20).
#' @param issue_levels Optional vector specifying the order of issue labels.
#' @param a Hyperparameter for the prior of rho (default: 0.01).
#' @param b Hyperparameter for the prior of theta (default: 0.001).
#' @param rho_init Initial value for rho (default: 10).
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
#' @return An \code{issueirt_fit} object containing:
#'   \item{stan_fit}{The Stan fit object}
#'   \item{stan_input}{The Stan input object}
#'   \item{posterior_summary}{The posterior summary with post-processed estimates}
#'   \item{ideal_points}{Issue-specific ideal points}
#'   \item{constraints}{The constraints used for post-processing}
#'   \item{rollcall}{The pscl rollcall object}
#'   \item{filtered}{Logical vectors indicating which legislators and bills were included}
#'   \item{issue_code}{The processed issue code object}
#'   \item{legis_data}{The filtered legislator data}
#'   \item{call}{The function call}
#'
#' @examples
#' \dontrun{
#' # Generate synthetic data
#' synth <- generate_data(seed = 1, n = 50, m = 100, k = 3)
#'
#' # Fit the model with minimal arguments
#' fit <- issueirt(
#'   votes = synth$data$Y,
#'   issue_codes = as.character(synth$stan$z),
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
#' \code{\link{issueirt_stan}} for the underlying Stan model,
#' \code{\link{make_stan_input}} for manual input preparation,
#' \code{\link{get_ideal_points}} for extracting ideal points
#'
#' @importFrom pscl rollcall ideal
#' @importFrom dplyr tibble
#' @export
issueirt <- function(
    votes,
    issue_codes,
    legis_data = NULL,
    party_col = NULL,
    liberal_code = 100,
    conservative_code = 200,
    top_party_code = NULL,
    constraints = NULL,
    lop = 0,
    minvotes = 20,
    issue_levels = NULL,
    a = 0.01,
    b = 0.001,
    rho_init = 10,
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

  # Input validation
  if (!is.matrix(votes)) {
    votes <- as.matrix(votes)
  }
  if (ncol(votes) != length(issue_codes)) {
    stop("Length of issue_codes must equal the number of columns in votes")
  }

  # Create legislator IDs if not provided
  if (is.null(rownames(votes))) {
    legis_ids <- paste0("Legislator_", seq_len(nrow(votes)))
    rownames(votes) <- legis_ids
  } else {
    legis_ids <- rownames(votes)
  }

  # Create bill IDs if not provided
  if (is.null(colnames(votes))) {
    bill_ids <- paste0("Bill_", seq_len(ncol(votes)))
    colnames(votes) <- bill_ids
  } else {
    bill_ids <- colnames(votes)
  }

  # Create legis_data if not provided
  if (is.null(legis_data)) {
    legis_data <- data.frame(id = legis_ids)
    party_col <- NULL
  } else {
    if (nrow(legis_data) != nrow(votes)) {
      stop("Number of rows in legis_data must equal number of rows in votes")
    }
    if (!is.null(party_col) && !party_col %in% colnames(legis_data)) {
      stop(paste0("Column '", party_col, "' not found in legis_data"))
    }
  }

  if (verbose) message("Step 1/7: Creating rollcall object and filtering votes...")

  # Create rollcall object
  rc <- pscl::rollcall(
    votes,
    yea = 1, nay = 0, missing = NA,
    legis.names = legis_ids,
    legis.data = legis_data,
    vote.names = bill_ids
  )

  # Filter votes
  filtered <- filter_votes(rc, lop = lop, minvotes = minvotes)
  legis_filtered <- legis_data[filtered$legis, , drop = FALSE]
  votes_filtered <- votes[filtered$legis, filtered$bills]
  issue_codes_filtered <- issue_codes[filtered$bills]

  # Create filtered rollcall
  rc_filtered <- pscl::rollcall(
    votes_filtered,
    yea = 1, nay = 0, missing = NA,
    legis.names = rownames(votes_filtered),
    legis.data = legis_filtered,
    vote.names = colnames(votes_filtered)
  )

  if (verbose) message("Step 2/7: Processing issue codes...")

  # Process issue codes
  if (is.numeric(issue_codes_filtered)) {
    issue_codes_filtered <- as.character(issue_codes_filtered)
  }
  issue_code <- make_issue_code(issue_code_vec = issue_codes_filtered, levels = issue_levels)

  if (verbose) message("Step 3/7: Fitting initial Bayesian IRT model...")

  # Fit initial Bayesian IRT
  set.seed(seed)
  ideal_fit <- suppressMessages(suppressWarnings(
    pscl::ideal(
      rc_filtered,
      dropList = list(lop = 0, legisMin = 0),
      priors = NULL, startvals = "eigen",
      d = 2, maxiter = ideal_maxiter, thin = 1, burnin = ideal_burnin,
      impute = FALSE, normalize = FALSE,
      store.item = TRUE, file = NULL, verbose = FALSE
    )
  ))

  if (verbose) message("Step 4/7: Finding constraints and post-processing initial fit...")

  # Find constraints if not provided
  if (is.null(constraints)) {
    if (!is.null(party_col)) {
      pol_rc1 <- find_pol_rc_horizontal(
        rc_filtered,
        party_code_col = party_col,
        liberal_code = liberal_code,
        conservative_code = conservative_code,
        na_threshold = 0.5
      )
      pol_rc2 <- find_pol_rc_vertical(
        ideal_fit, rc_filtered, pol_rc1,
        party_code_col = party_col,
        liberal_code = liberal_code,
        conservative_code = conservative_code,
        na_threshold = 0.5, lop_threshold = 0.1
      )
      constraints <- find_constraints(
        ideal_fit, rc_filtered,
        pol_rc1 = pol_rc1, pol_rc2 = pol_rc2,
        party_code_col = party_col,
        left_party_code = liberal_code,
        top_party_code = top_party_code,
        as_list = TRUE
      )
    } else {
      # Use simple constraints based on extreme ideal points
      xbar <- ideal_fit$xbar
      # Find extreme legislators
      idx_left <- which.min(xbar[, 1])
      idx_right <- which.max(xbar[, 1])
      idx_top <- which.max(xbar[, 2])

      legis_names <- rownames(xbar)
      constraints <- list()
      constraints[[legis_names[idx_left]]] <- c(xbar[idx_left, 1], xbar[idx_left, 2])
      constraints[[legis_names[idx_right]]] <- c(xbar[idx_right, 1], xbar[idx_right, 2])
      constraints[[legis_names[idx_top]]] <- c(xbar[idx_top, 1], xbar[idx_top, 2])
    }
  }

  # Post-process initial fit
  invisible(capture.output({
    ideal_pp <- pscl::postProcess(ideal_fit, constraints = constraints)
  }))

  if (verbose) message("Step 5/7: Creating Stan input...")

  # Create Stan input
  stan_input <- make_stan_input(
    issue_code_vec = issue_code$issue_code_vec,
    rollcall = rc_filtered,
    ideal = ideal_pp,
    a = a, b = b, rho_init = rho_init
  )

  # Store additional information
  stan_input$misc <- list(
    issue_code_df = issue_code,
    legis_label = rownames(votes_filtered),
    rc_label = colnames(votes_filtered)
  )

  if (verbose) message("Step 6/7: Fitting IssueIRT model (this may take a while)...")

  # Fit Stan model
  stan_fit <- issueirt_stan(
    data = stan_input$data,
    init = rep(list(stan_input$init), chains),
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    seed = seed,
    ...
  )

  if (verbose) message("Step 7/7: Post-processing results...")

  # Generate posterior summary
  posterior_summary <- make_posterior_summary_postprocessed(
    stan_fit = stan_fit,
    constraints = constraints,
    issue_label = issue_code$codebook$label,
    rc_label = colnames(votes_filtered),
    legis_label = rownames(votes_filtered),
    missing_label = NULL
  )

  # Get ideal points
  legis_group <- if (!is.null(party_col)) legis_filtered[[party_col]] else NULL
  ideal_points <- get_ideal_points(
    stan_fit = stan_fit,
    issue_label = issue_code$codebook$label,
    legis_label = rownames(votes_filtered),
    legis_group = legis_group,
    dynamic = FALSE
  )

  if (verbose) message("Done!")

  # Create result object
  result <- list(
    stan_fit = stan_fit,
    stan_input = stan_input,
    posterior_summary = posterior_summary,
    ideal_points = ideal_points,
    constraints = constraints,
    rollcall = rc_filtered,
    filtered = filtered,
    issue_code = issue_code,
    legis_data = legis_filtered,
    call = call
  )

  class(result) <- c("issueirt_fit", "list")

  return(result)
}

#' Print Method for issueirt_fit Objects
#'
#' @param x An \code{issueirt_fit} object.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns the input object.
#' @export
print.issueirt_fit <- function(x, ...) {
  cat("IssueIRT Model Fit\n")
  cat("==================\n\n")

  cat("Data:\n")
  cat("  Legislators:", x$stan_input$data$J, "\n")
  cat("  Roll calls:", x$stan_input$data$M, "\n")
  cat("  Issue categories:", x$stan_input$data$K, "\n")
  cat("  Observations:", x$stan_input$data$N_obs, "\n\n")

  cat("Model:\n")
  cat("  Chains:", x$stan_fit@sim$chains, "\n")
  cat("  Iterations per chain:", x$stan_fit@sim$iter, "\n")
  cat("  Warmup:", x$stan_fit@sim$warmup, "\n\n")

  cat("Issue categories:\n")
  for (i in seq_len(nrow(x$issue_code$codebook))) {
    cat("  ", x$issue_code$codebook$code[i], ": ", x$issue_code$codebook$label[i], "\n", sep = "")
  }

  invisible(x)
}

#' Summary Method for issueirt_fit Objects
#'
#' @param object An \code{issueirt_fit} object.
#' @param ... Additional arguments (not used).
#' @return A list containing summary information.
#' @importFrom rstan get_elapsed_time
#' @export
summary.issueirt_fit <- function(object, ...) {
  # Compute elapsed time
  elapsed_times <- rstan::get_elapsed_time(object$stan_fit)
  total_time <- sum(elapsed_times)

  # Get theta summary
  theta_summary <- object$posterior_summary$theta_postprocessed

  # Get x summary (mean across dimensions)
  x_summary <- object$posterior_summary$x_postprocessed

  cat("IssueIRT Model Summary\n")
  cat("======================\n\n")

  cat("Data Summary:\n")
  cat("  Legislators:", object$stan_input$data$J, "\n")
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
    n_legis = object$stan_input$data$J,
    n_rc = object$stan_input$data$M,
    n_issues = object$stan_input$data$K,
    n_obs = object$stan_input$data$N_obs,
    elapsed_time = total_time,
    theta_summary = theta_summary,
    x_summary = x_summary
  ))
}

#' Plot Method for issueirt_fit Objects
#'
#' @param x An \code{issueirt_fit} object.
#' @param type Type of plot to create. Options are "ideal_points" (default),
#'   "axes", or "issue_specific".
#' @param ... Additional arguments passed to the underlying plot functions.
#' @return A ggplot object.
#' @export
plot.issueirt_fit <- function(x, type = c("ideal_points", "axes", "issue_specific"), ...) {
  type <- match.arg(type)

  # Get group from ideal_points if it exists, otherwise NULL
  group <- if ("legis_group" %in% colnames(x$ideal_points)) {
    x$ideal_points$legis_group[match(rownames(x$rollcall$votes), x$ideal_points$legis_label)]
  } else {
    NULL
  }

  if (type == "ideal_points") {
    plot_ideal(
      ideal_point_1d = x$posterior_summary$x_postprocessed[x$posterior_summary$x_postprocessed$dimension == 1, ]$mean,
      ideal_point_2d = x$posterior_summary$x_postprocessed[x$posterior_summary$x_postprocessed$dimension == 2, ]$mean,
      group = group,
      p.title = "IssueIRT Ideal Points",
      ...
    )
  } else if (type == "axes") {
    plot_issueaxis(
      stan_input = x$stan_input,
      posterior_summary = x$posterior_summary,
      group = group,
      p.title = "Issue-Specific Axes",
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
