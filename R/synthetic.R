#' Generate Synthetic Roll Call Data
#'
#' This function generates synthetic roll call data for a specified number of legislators and bills,
#' along with the necessary inputs for a Stan model. It simulates issue vectors, roll call vectors,
#' difficulty parameters, and legislator ideal points.
#'
#' @param seed Random seed for reproducibility.
#' @param n Number of legislators.
#' @param m Number of bills.
#' @param k Number of topics.
#' @param kappa Standard deviation of w.
#' @param rho Concentration parameter of von Mises distribution.
#' @param a Hyperparameter for the prior of rho (0 < b < a).
#' @param b Hyperparameter for the prior of theta (0 < b < a).
#' @param theta Issue vectors (in angle), optional.
#' @param X Legislator ideal points (in 2D), optional.
#' @param lop Lower bound of proportion of votes, used in filtering.
#' @param minvotes Minimum number of votes required for a legislator to be included.
#' @param missingness_rate Rate of missingness in the data.
#' @return A list containing 'data' for general use and 'stan' for inputs to a Stan model.
#' @importFrom stats rnorm runif
#' @importFrom mvtnorm rmvnorm
#' @importFrom dplyr tibble
#' @importFrom pscl rollcall
#' @importFrom Rfast rvonmises
#' @export
generate_data <- function(seed = 02138, n = 20, m = 100, k = 5, kappa = 5, rho = 10, a = 0.01,
                          b = 0.001, theta = NULL, X = NULL, lop = 0, minvotes = 20, missingness_rate = 0.05) {
  if(missingness_rate < 0 | missingness_rate > 1) stop("missingness_rate must be between 0 and 1")
  set.seed(seed)

  # scale parameter
  w <- abs(rnorm(m, mean = 0, sd = kappa))

  # issue vectors
  if (is.null(theta)) theta <- rvonmises(k, 0, 0)

  # issue codes
  z <- sample(1:k, m, replace = T)
  while (length(unique(z)) != k) {
    z <- sample(1:k, m, replace = T)
  }

  # roll call vectors
  u <- rep(NA, m)
  for (j in 1:m) {
    u[j] <- rvonmises(1, theta[z[j]], rho)
  }

  # difficulty parameters
  alpha <- rnorm(m, mean = 0, sd = kappa)

  # legislator ideal points
  if(is.null(X)) X <- mvtnorm::rmvnorm(n, mean = rep(0, 2), sigma = diag(1, 2))

  # legislator votes
  Ystar <- matrix(NA, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      Ystar[i, j] <- rnorm(1, mean = w[j] * c(cos(u[j]), sin(u[j])) %*% X[i, ] - alpha[j], sd = 1)
    }
  }
  Ustar <- matrix(runif(m * n, 0, 1), n, m)
  Y <- Ustar < Ystar
  Y <- 1 * Y

  # missing data
  N <- m * n
  N_obs <- (1-missingness_rate) * N
  N_mis <- missingness_rate * N

  # index of missing data
  idx_mis <- sort(sample(1:N, N_mis))
  # index of observed data
  idx_obs <- c(1:N)[!(1:N %in% idx_mis)]
  y_mis <- Y
  y_mis[idx_mis] <- NA
  rc <- pscl::rollcall(y_mis, yea = 1, nay = 0, missing = NA)
  filt <- filter_votes(rollcall = rc, lop = lop, minvotes = minvotes)
  n <- sum(filt$legis)
  m <- sum(filt$bills)
  y_vec <- as.vector(y_mis[filt$legis, filt$bills])
  n_mis <- sum(is.na(y_vec))
  n_obs <- m * n - n_mis
  y_vec_obs <- y_vec[!is.na(y_vec)]
  filt_idx_obs <- which(!is.na(y_vec))
  filt_idx_mis <- which(is.na(y_vec))

  res <- list()
  res$data <- list(
    Y = y_mis,
    X = X,
    Ystar = Ystar,
    alpha = alpha,
    u = u,
    theta = theta,
    w = w,
    z = z,
    filtered_legis = filt$legis,
    filtered_bills = filt$bills,
    code = match.call()
  )
  res$stan <- list(
    J = n,
    M = m,
    N = m * n,
    N_obs = n_obs,
    N_mis = n_mis,
    K = k,
    j = rep(1:n, m),
    m = rep(1:m, each = n),
    y_obs = y_vec_obs,
    idx_obs = filt_idx_obs,
    idx_mis = filt_idx_mis,
    z = z[filt$bills],
    a = a,
    b = b
  )
  return(res)
}

#' Generate Synthetic Roll Call Data (Dynamic)
#'
#' This function generates synthetic roll call data for a specified number of legislators and bills,
#' along with the necessary inputs for a Stan model. It simulates issue vectors, roll call vectors,
#' difficulty parameters, and legislator ideal points.
#'
#' @param seed Random seed for reproducibility.
#' @param n Number of legislators.
#' @param m Number of bills.
#' @param n_terms Number of terms.
#' @param k_ls Number of topics in each term.
#' @param kappa Standard deviation of w.
#' @param rho Concentration parameter of von Mises distribution.
#' @param a Hyperparameter for the prior of rho (0 < b < a).
#' @param b Hyperparameter for the prior of theta (0 < b < a).
#' @param theta_ls Issue vectors (in angle), optional.
#' @param X Legislator ideal points (in 2D), optional.
#' @param lop Lower bound of proportion of votes, used in filtering.
#' @param minvotes Minimum number of votes required for a legislator to be included.
#' @param t_ls A list of terms for each legislator.
#' @param missingness_rate Rate of missingness in the data.
#' @return A list containing 'data' for general use and 'stan' for inputs to a Stan model.
#' @importFrom stats rnorm runif
#' @importFrom mvtnorm rmvnorm
#' @importFrom dplyr tibble
#' @importFrom pscl rollcall
#' @importFrom Rfast rvonmises
#' @importFrom purrr map
#' @export
generate_dynamic_data <- function(seed = 02138, n = 30, m = 270,
                                  n_terms = 3, k_ls = c(3, 3, 3),
                                  kappa = 5, rho = 10, a = 0.01, b = 0.001,
                                  theta_ls = NULL,
                                  X = NULL, lop = 0, minvotes = 20,
                                  t_ls = replicate(30, 1:3, simplify = FALSE), missingness_rate = 0.001) {
  if(missingness_rate < 0 | missingness_rate > 1) stop("missingness_rate must be between 0 and 1")
  set.seed(seed)

  # scale parameter
  w <- abs(rnorm(m, mean = 0, sd = kappa))

  # issue vectors
  k <- sum(k_ls)
  k_end <- cumsum(k_ls)
  k_start <- c(1, k_end[-length(k_end)] + 1)
  k_ranges <- map(1:n_terms, ~ k_start[.x]:k_end[.x]) # range of issue codes for each term

  # issue axis
  if (is.null(theta_ls)) {
    theta <- rvonmises(k, 0, 0)
  } else {
    theta <- unlist(theta_ls)
  }

  # issue codes
  z_ls <- split(1:m, ceiling(seq_along(1:m) / ceiling(m / k))) # divide it into k parts
  z <- map(1:length(z_ls), ~rep(.x, length(z_ls[[.x]]))) |>
    unlist()

  # roll call vectors
  u <- rep(NA, m)
  for (j in 1:m) {
    u[j] <- rvonmises(1, theta[z[j]], rho)
  }

  # difficulty parameters
  alpha <- rnorm(m, mean = 0, sd = kappa)

  # legislator ideal points
  legis_terms <- map(1:length(t_ls), ~paste0(.x, "_", t_ls[[.x]])) |> unlist() # a list of unique legis-term
  n_legis_terms <- length(legis_terms) # number of unique legis-term
  # create an index for the prior
  prior_idx <- map(1:n, function(i) {
    v <- t_ls[[i]]
    t <- length(v)
    if(t == 1) {
      res <- NA
    } else {
      res <- rep(NA, t)
      for (j in 2:t) {
        res[j] <- which(legis_terms == paste0(i, "_", v[j-1]))
      }
    }
    return(res)
  }) |>
    unlist()

  if(is.null(X)) {
    X <- mvtnorm::rmvnorm(n_legis_terms, mean = rep(0, 2), sigma = diag(1, 2))
    for(i in 1:n_legis_terms) {
      if(!is.na(prior_idx[i])) {
        X[i,] <- mvtnorm::rmvnorm(1, mean = X[prior_idx[i],], sigma = diag(0.01, 2))
      }
    }
    rownames(X) <- legis_terms
  }
  Ystar_ls <- list()
  Y_ls <- list()
  for(t in 1:n_terms) {
    legis_vec <- which(map(t_ls, ~t %in% .x) |> unlist())
    legis_terms_t <- paste0(legis_vec, "_", t) # list of legis-term for term t (character)
    m_t <- map(k_ranges[[t]], ~which(z %in% .x)) |> unlist() # list of roll calls for term t (numeric)
    Ystar <- matrix(NA, nrow = length(legis_terms_t), ncol = length(m_t))
    rownames(Ystar) <- legis_terms_t
    colnames(Ystar) <- m_t
    for (i in legis_terms_t) {
      for (j in m_t) {
        Ystar[i, as.character(j)] <- rnorm(1, mean = w[j] * c(cos(u[j]), sin(u[j])) %*% X[i, ] - alpha[j], sd = 1)
      }
    }
    Ustar <- matrix(runif(length(m_t) * length(legis_terms_t), 0, 1), length(legis_terms_t), length(m_t))
    Y <- Ustar < Ystar
    Y <- 1 * Y
    Y_ls[[t]] <- Y
    Ystar_ls[[t]] <- Ystar
  }

  y_mis_ls <- list()
  filt_legis_ls <- list()
  filt_bills_ls <- list()
  n_mat <- matrix(NA, nrow = n_terms, ncol = 4)
  colnames(n_mat) <- c("n", "m", "n_obs", "n_mis")
  y_vec_obs_ls <- list()
  y_vec_ls <- list()

  for(t in 1:n_terms) {
    y_mis <- Y_ls[[t]]

    # missing data
    N <- ncol(y_mis) * nrow(y_mis)
    N_obs <- (1-missingness_rate) * N
    N_mis <- missingness_rate * N

    # index of missing data
    idx_mis <- sort(sample(1:N, N_mis))
    # index of observed data
    idx_obs <- c(1:N)[!(1:N %in% idx_mis)]
    y_mis[idx_mis] <- NA
    rc <- pscl::rollcall(y_mis, yea = 1, nay = 0, missing = NA)
    filt <- filter_votes(rollcall = rc, lop = lop, minvotes = minvotes)
    n <- sum(filt$legis)
    m <- sum(filt$bills)
    y_vec <- as.vector(y_mis[filt$legis, filt$bills])
    n_mis <- sum(is.na(y_vec))
    n_obs <- m * n - n_mis
    y_mis_ls[[t]] <- y_mis
    n_mat[t,] <- c(n, m, n_obs, n_mis)
    y_vec_ls[[t]] <- y_vec
    y_vec_obs_ls[[t]] <- y_vec[!is.na(y_vec)]
    filt_legis_ls[[t]] <- filt$legis
    filt_bills_ls[[t]] <- filt$bills
  }
  n_df <- as.data.frame(n_mat) |>
    dplyr::mutate(N = n*m)
  j_vec <- map(1:n_terms, ~paste0(rep(1:n_df$n[.x], n_df$m[.x]), "_", .x)) |> unlist()
  j_vec <- as.numeric(factor(j_vec, levels = legis_terms))
  m_cumsum <- n_df$m |> cumsum()
  m_cumsum_begin <- c(1, m_cumsum[1:(n_terms - 1)] + 1)
  m_vec <- map(1:n_terms, ~rep(m_cumsum_begin[.x]:m_cumsum[.x], each = n_df$n[.x])) |> unlist()
  omega_vec <- rep(1, length(prior_idx))
  omega_vec[!is.na(prior_idx)] <- 0.01
  prior_idx[is.na(prior_idx)] <- sum(n_df$n) + 1

  res <- list()
  res$data <- list(
    Y = y_mis_ls,
    X = X,
    Ystar = Ystar_ls,
    alpha = alpha,
    u = u,
    theta = theta,
    w = w,
    z = z,
    filtered_legis = filt_legis_ls,
    filtered_bills = filt_bills_ls,
    legis_term = legis_terms,
    code = match.call()
  )
  res$stan <- list(
    J = sum(n_df$n),
    M = sum(n_df$m),
    N = sum(n_df$N),
    N_obs = sum(n_df$n_obs),
    # N_mis = sum(n_df$n_mis),
    K = k,
    j = j_vec,
    m = m_vec,
    y_obs = unlist(y_vec_obs_ls),
    idx_obs = which(!is.na(unlist(y_vec_ls))),
    # idx_mis = unlist(filt_idx_mis_ls),
    z = z[unlist(filt_bills_ls)],
    a = a,
    b = b,
    omega = omega_vec,
    idx = prior_idx
  )
  return(res)
}

