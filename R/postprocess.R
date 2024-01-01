#' Find Affine Transformation
#'
#' This function calculates the affine transformation matrix to map a matrix of
#' source coordinates to a matrix of target coordinates. This function is originally
#' from the pscl package.
#'
#' @param x A matrix of source coordinates.
#' @param target A matrix of target coordinates.
#'
#' @return A matrix of affine transformation.
#'
#' @export
affine_transformation <- function(x, target) {
  d <- dim(x)[2]
  x0 <- cbind(x, 1)
  if (d > 1) {
    zeroMat <- 0 * x0
    A <- rbind(cbind(x0, zeroMat),
               cbind(zeroMat, x0))
    b <- as.vector(target)
  }
  if (d == 1) {
    A <- x0
    b <- target
  }
  foo <- solve(A) %*% b
  foo <- matrix(foo, nrow = d + 1)
  return(foo)
}
#' Post-process Function Based on pscl Package
#'
#' This function post-processes the posterior samples from a Stan fit object to obtain
#' samples of x, u, and theta. It also applies affine transformations based on constraints.
#'
#' @param stan_fit A stan fit object.
#' @param constraints A list of constraints.
#' @param legis_label A vector of legislator labels.
#' @param as_mcmc Whether to return the posterior samples as mcmc objects.
#'
#' @return A list of posterior samples of x, u, and theta that are post-processed.
#' @importFrom rstan extract
#' @importFrom coda mcmc mcmc.list
#' @export
post_process <- function(stan_fit,
                         constraints,
                         legis_label,
                         as_mcmc = FALSE) {
  if (!is.list(constraints)) stop("constraints must be a list")
  extracted_samples <- rstan::extract(stan_fit)
  x_samples <- extracted_samples$x
  n_iter <- dim(x_samples)[1]
  n <- dim(x_samples)[2]
  if (dim(x_samples)[2] != length(legis_label)) stop("length of legis_label must be the same as the number of legislators")
  if (any(!names(constraints) %in% legis_label)) stop("names of constraints are not included in legis_label")

  m <- stan_fit@par_dims$u
  k <- stan_fit@par_dims$theta

  # Create an array
  u_cartesian_samples <- array(dim = c(n_iter, m, 2))
  u_cartesian_samples[, , 1] <- extracted_samples$x_coord_u / extracted_samples$r_u
  u_cartesian_samples[, , 2] <- extracted_samples$y_coord_u / extracted_samples$r_u
  theta_cartesian_samples <- array(dim = c(n_iter, k, 2))
  theta_cartesian_samples[, , 1] <- extracted_samples$x_coord_theta / extracted_samples$r_theta
  theta_cartesian_samples[, , 2] <- extracted_samples$y_coord_theta / extracted_samples$r_theta

  origin_samples <- array(0, dim = c(n_iter, m, 2))

  # Form target matrix
  target <- matrix(NA, nrow = 3, ncol = 2)
  for (i in 1:3) {
    target[i, ] <- constraints[[i]]
  }

  # Initialize output objects
  newX <- array(NA, dim = c(n_iter, n, 2))
  newU <- array(NA, dim = c(n_iter, m, 2))
  newTheta <- array(NA, dim = c(n_iter, k, 2))
  newO <- array(NA, dim = c(n_iter, m, 2))

  index <- match(names(constraints), legis_label)

  # Now loop over iterations
  for (iter in 1:n_iter) {
    x0 <- x_samples[iter, index, , drop = TRUE]
    x0 <- matrix(x0, 3, 2)
    tMat <- affine_transformation(x0, target = target)

    thisX <- cbind(x_samples[iter, , ], 1)
    tmpX <- thisX %*% tMat
    newX[iter, , ] <- tmpX

    thisU <- cbind(u_cartesian_samples[iter, , ], 1)
    tmpU <- thisU %*% tMat
    newU[iter, , ] <- tmpU

    thisTheta <- cbind(theta_cartesian_samples[iter, , ], 1)
    tmpTheta <- thisTheta %*% tMat
    newTheta[iter, , ] <- tmpTheta

    thisO <- cbind(origin_samples[iter, , ], 1)
    tmpO <- thisO %*% tMat
    newO[iter, , ] <- tmpO
  }

  # Re-orient with the origin
  newU <- newU - newO
  newTheta <- newTheta - newO[, 1:k, ]

  # Convert to polar coordinates
  newU <- atan2(newU[, , 2], newU[, , 1])
  newTheta <- atan2(newTheta[, , 2], newTheta[, , 1])

  res <- list(
    x = newX,
    u = newU,
    theta = newTheta
  )
  if (as_mcmc) {
    n_chains <- stan_fit@sim$chains
    iter_per_chain <- stan_fit@sim$iter / n_chains
    # Create a list of chains
    u_mcmc <- theta_mcmc <- x2_mcmc <- x1_mcmc <- list()
    for (i in 1:n_chains) {
      start_idx <- (i - 1) * iter_per_chain + 1
      end_idx <- i * iter_per_chain
      x1_mcmc[[i]] <- coda::mcmc(newX[start_idx:end_idx, , 1])
      x2_mcmc[[i]] <- coda::mcmc(newX[start_idx:end_idx, , 2])
      theta_mcmc[[i]] <- coda::mcmc(newTheta[start_idx:end_idx, ])
      u_mcmc[[i]] <- coda::mcmc(newU[start_idx:end_idx, ])
    }
    x1_mcmc <- coda::mcmc.list(x1_mcmc)
    x2_mcmc <- coda::mcmc.list(x2_mcmc)
    theta_mcmc <- coda::mcmc.list(theta_mcmc)
    u_mcmc <- coda::mcmc.list(u_mcmc)
    res <- list(
      x1 = x1_mcmc,
      x2 = x2_mcmc,
      u = u_mcmc,
      theta = theta_mcmc
    )
  }

  return(res)
}
#' Process Data (for Internal Use of `make_posterior_summary_postprocessed`)
#'
#' This function processes data for generating posterior summaries.
#'
#' @param data A matrix of data.
#' @param parameter A character of parameter name.
#' @param index_label A character of index label.
#' @param label_data A vector of labels.
#' @param dimension A vector of dimension.
#'
#' @return A tibble of posterior summary.
#'
#' @importFrom dplyr summarise across everything relocate select all_of sym
#' @importFrom tidyr pivot_longer
#' @importFrom data.table :=
#' @export
process_data <- function(data, parameter, index_label, label_data = NULL, dimension = NULL) {
  colnames(data) <- 1:ncol(data)
  label_column_name <- paste0(str_remove(index_label, "_index"), "_label")

  data |>
    as_tibble() |>
    summarise(across(everything(), list(
      mean = ~mean(.),
      sd = ~sd(.),
      `2.5%` = ~quantile(., probs = 0.025),
      `25%` = ~quantile(., probs = 0.25),
      `50%` = ~quantile(., probs = 0.5),
      `75%` = ~quantile(., probs = 0.75),
      `97.5%` = ~quantile(., probs = 0.975)
    )), .groups = "drop") |>
    pivot_longer(everything(), names_to = c(index_label, ".value"), names_sep = "_") |>
    mutate(
      !!sym(index_label) := as.integer(str_remove(!!sym(index_label), "V")),
      !!label_column_name := if (!is.null(label_data)) label_data else NA_character_,
      parameter = parameter,
      dimension = dimension
    ) |>
    relocate(parameter, if (!is.null(dimension)) all_of(dimension) else NULL, !!sym(index_label), !!sym(label_column_name), everything()) |>
    select(parameter, everything())
}
#' Generate Posterior Summary of Post-Processed Stan Output
#'
#' This function generates posterior summaries of post-processed Stan output, including samples of x, u, and theta. It applies affine transformations based on constraints and provides posterior summaries for various parameters.
#'
#' @param stan_fit A Stan fit object.
#' @param constraints A list of constraints for post-processing.
#' @param issue_label A vector of issue labels (optional).
#' @param rc_label A vector of roll call labels (optional).
#' @param legis_label A vector of legislator labels (optional).
#' @param missing_label A vector of missing indicator labels (optional).
#'
#' @return A list of tibbles containing posterior summaries:
#'   - theta: Posterior summary of theta.
#'   - theta_aux: Posterior summary of auxiliary variables of theta (x_coord_theta, y_coord_theta, r_theta).
#'   - u: Posterior summary of u.
#'   - u_aux: Posterior summary of auxiliary variables of u (x_coord_u, y_coord_u, r_u).
#'   - x: Posterior summary of x.
#'   - other_bill: Posterior summary of other bill parameters (alpha, w, rho).
#'   - y_imputed: Posterior summary of imputed missing data.
#'   - theta_postprocessed: Posterior summary of theta (post-processed).
#'   - u_postprocessed: Posterior summary of u (post-processed).
#'   - x_postprocessed: Posterior summary of x (post-processed).
#'
#' @importFrom dplyr bind_rows arrange filter pull
#' @importFrom rlang .data
#' @export
make_posterior_summary_postprocessed <- function(stan_fit, constraints, issue_label = NULL, rc_label = NULL, legis_label = NULL, missing_label = NULL) {
  posterior_summary <- make_posterior_summary(stan_fit = stan_fit, issue_label = issue_label, rc_label = rc_label, legis_label = legis_label, missing_label = missing_label)
  samples_pp <- post_process(stan_fit = stan_fit, constraints = constraints, legis_label = legis_label)
  theta_pp <- process_data(samples_pp$theta, "theta", "issue_index", posterior_summary$theta$issue_label)
  u_pp <- process_data(samples_pp$u, "u", "rc_index", posterior_summary$u$rc_label)
  x1_pp <- process_data(samples_pp$x[, , 1], "x", "legis_index", posterior_summary$x |> filter(.data$dimension == 1) |> pull(.data$legis_label), dimension = 1)
  x2_pp <- process_data(samples_pp$x[, , 2], "x", "legis_index", posterior_summary$x |> filter(.data$dimension == 2) |> pull(.data$legis_label), dimension = 2)

  x_pp <- bind_rows(x1_pp, x2_pp) |>
    arrange(.data$legis_index)

  posterior_summary$theta_postprocessed <- theta_pp
  posterior_summary$u_postprocessed <- u_pp
  posterior_summary$x_postprocessed <- x_pp
  return(posterior_summary)
}


