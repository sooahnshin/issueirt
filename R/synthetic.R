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
#' @return A list containing 'data' for general use and 'stan' for inputs to a Stan model.
#' @importFrom stats rnorm runif
#' @importFrom mvtnorm rmvnorm
#' @importFrom dplyr tibble
#' @importFrom pscl rollcall
#' @importFrom Rfast rvonmises
#' @export
generate_data <- function(seed = 02138, n = 20, m = 100, k = 5, kappa = 5, rho = 10, a = 0.01,
                          b = 0.001, theta = NULL, X = NULL, lop = 0, minvotes = 20) {
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
  N_obs <- 0.95 * N
  N_mis <- 0.05 * N

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
