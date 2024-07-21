
#' Check Unanimous Votes
#'
#' This function checks if there are any roll calls where all legislators vote the same way.
#'
#' @param votes A matrix of votes (output of `pscl::rollcall(...)$votes`; recommended to use `filter_votes` first).
#' @param yea_code A vector of values in `votes` that are coded as yea (default: 1).
#' @param nay_code A vector of values in `votes` that are coded as nay (default: 0).
#' @return A vector of roll call names where all legislators vote the same way.
#' @export
check_unanimous <- function(votes, yea_code = 1, nay_code = 0) {
  votes_vec <- as.vector(votes)
  votes_vec[!votes_vec %in% c(yea_code, nay_code)] <- NA
  votes_vec[votes_vec == yea_code] <- 1
  votes_vec[votes_vec == nay_code] <- 0
  votes_matrix <- matrix(votes_vec, nrow = nrow(votes), ncol = ncol(votes))

  unanimous_yea_cols <- colnames(votes)[which(colSums(votes_matrix == 1, na.rm = TRUE) == 0)]
  unanimous_nay_cols <- colnames(votes)[which(colSums(votes_matrix == 0, na.rm = TRUE) == 0)]

  unanimous_cols <- unique(c(unanimous_yea_cols, unanimous_nay_cols))

  if (length(unanimous_cols) > 0) {
    return(unanimous_cols)
  } else {
    return(NULL)
  }
}
#' Preprocess Roll Call Data for Analysis
#'
#' This function preprocesses roll call data by filtering out votes based on specified criteria.
#' It drops votes according to the level of support or opposition for a bill and the minimum number of votes a legislator must cast.
#'
#' @param rollcall A roll call object (output of `pscl::rollcall`).
#' @param lop The minimum level of support (or oppose) for a bill to be included in the analysis (default is 0).
#' @param minvotes The minimum number of votes a legislator must cast for them to be included in the analysis (default is 0).
#' @return A list with two logical vectors: `legis` and `bills` indicating which legislators and bills should be included in the analysis.
#' @importFrom pscl rollcall
#' @export
filter_votes <- function(rollcall, lop = 0, minvotes = 0) {
  votes <- rollcall$votes
  yea_code <- rollcall$codes$yea
  nay_code <- rollcall$codes$nay
  legis_filtered <- apply(matrix(votes %in% c(yea_code, nay_code), nrow(votes), ncol(votes)), 1, sum) >= minvotes
  lop_denominator <- pmin(apply(matrix(votes %in% yea_code, nrow(votes), ncol(votes)), 2, sum),
                          apply(matrix(votes %in% nay_code, nrow(votes), ncol(votes)), 2, sum))
  lop_numerator <- apply(matrix(votes %in% c(yea_code, nay_code), nrow(votes), ncol(votes)), 2, sum)
  votes_lop <- lop_denominator / lop_numerator
  votes_lop[lop_numerator == 0] <- 0
  votes_filtered <- votes_lop > lop

  tmp <- rollcall$votes[legis_filtered, votes_filtered]
  flag <- check_unanimous(votes = tmp, yea_code = yea_code, nay_code = nay_code)
  if(!is.null(flag)) {
    votes_filtered[which(colnames(votes) %in% flag)] <- FALSE
  }

  return(list(legis = legis_filtered, bills = votes_filtered))
}
#' Recode Roll Call Votes
#'
#' This function preprocesses roll call vote data by recoding votes to prevent sign flipping. It is designed to work with matrices of votes, typically obtained from `pscl::rollcall`.
#'
#' @param votes A matrix of votes (output of `pscl::rollcall(...)$votes`; recommended to use `filter_votes` first).
#' @param party_code A vector of party codes for each legislator.
#' @param liberal_code Party code for the liberal party (default: 100).
#' @param conservative_code Party code for the conservative party (default: 200).
#' @param yea A vector of values in `votes` that are coded as yea (default: 1).
#' @param nay A vector of values in `votes` that are coded as nay (default: 0).
#' @param missing A vector of values in `votes` that are coded as missing (default: c(NA, 2)).
#' @param na_threshold The maximum ratio of missing data for a bill to be recoded (default: 0.5).
#' @return A matrix of recoded votes. 1 indicates a liberal majority or conservative minority, and 0 indicates a conservative majority or liberal minority.
#' @importFrom dplyr as_tibble
#' @export
recode_votes <- function(votes,
                         party_code,
                         liberal_code = 100,
                         conservative_code = 200,
                         yea = 1,
                         nay = 0,
                         missing = c(NA, 2),
                         na_threshold = 0.5) {
  # recode votes
  votes[votes %in% yea] <- 1
  votes[votes %in% nay] <- 0
  votes[votes %in% missing] <- NA

  # Flipping based on liberal majority
  # create reference votes
  ## create subset of votes
  liberal_votes <- as.matrix(votes[party_code == liberal_code,])
  ## calculate party majority vote
  liberal_mean <- as_tibble(liberal_votes) |>
    colMeans(na.rm = TRUE)
  ## create missing data ratio
  liberal_na <- liberal_votes
  liberal_na[!is.na(liberal_na)] <- 0
  liberal_na[is.na(liberal_na)] <- 1
  liberal_na <- as_tibble(liberal_na) |>
    colMeans()

  ## subset votes that need to be recoded: liberal_mean > 0.5 & liberal_na < threshold
  ref_votes <- as.matrix(votes[,liberal_mean > 0.5 & liberal_na < na_threshold])
  recoded_votes <- ref_votes
  recoded_votes[!is.na(ref_votes) & ref_votes == 0] <- 1
  recoded_votes[!is.na(ref_votes) & ref_votes == 1] <- 0

  votes[,liberal_mean > 0.5 & liberal_na < na_threshold] <- recoded_votes

  # Flipping based on conservative majority
  # create reference votes
  ## create subset of votes
  conservative_votes <- as.matrix(votes[party_code == conservative_code,])
  ## calculate party majority vote
  conservative_mean <- as_tibble(conservative_votes) |>
    colMeans(na.rm = TRUE)
  ## create missing data ratio
  conservative_na <- conservative_votes
  conservative_na[!is.na(conservative_na)] <- 0
  conservative_na[is.na(conservative_na)] <- 1
  conservative_na <- as_tibble(conservative_na) |>
    colMeans()

  ## subset votes that need to be recoded: conservative_mean < 0.5 & conservative_na < threshold
  ref_votes <- as.matrix(votes[,conservative_mean < 0.5 & conservative_na < na_threshold])
  recoded_votes <- ref_votes
  recoded_votes[!is.na(ref_votes) & ref_votes == 0] <- 1
  recoded_votes[!is.na(ref_votes) & ref_votes == 1] <- 0

  votes[,conservative_mean < 0.5 & conservative_na < na_threshold] <- recoded_votes

  ## 1: liberal majority or conservative minority
  ## 0: conservative majority or liberal minority
  return(votes)
}
#' Recode Issue Codes for Stan
#'
#' This function recodes a vector of issue codes, typically as part of pre-processing for Stan model input.
#' It also generates a codebook for the recoded issue codes.
#'
#' @param issue_code_vec A vector of issue codes, expected to be a character vector.
#' @param levels An optional vector specifying the order of issue labels. If provided, it must be a character vector
#'        and have a length equal to the number of unique values in `issue_code_vec`.
#'
#' @return A list containing the recoded issue code vector and a codebook (as a tibble) mapping the codes to labels.
#' @importFrom dplyr tibble
#' @export
make_issue_code <- function(issue_code_vec, levels = NULL) {
  if(any(is.na(issue_code_vec))) stop("issue_code_vec contains NA. Please recode this as a single group or separate groups.")
  if(!is.character(issue_code_vec)) stop("issue_code_vec must be a character vector.")
  if(!is.null(levels)) {
    if(!is.character(levels)) stop("levels must be a character vector.")
    if(length(levels) != length(unique(issue_code_vec))) stop("length of levels must be equal to the number of unique values in issue_code_vec.")
    if(any(!levels %in% issue_code_vec)) stop("levels must be a subset of issue_code_vec.")
  }
  if(is.null(levels)) levels <- sort(unique(issue_code_vec))

  issue_code_factor <- factor(issue_code_vec, levels = levels)

  res <- list(
    issue_code_vec = as.numeric(issue_code_factor),
    codebook = tibble(code = 1:length(levels), label = levels)
  )
  return(res)
}

#' Generate data and initial values for stan
#'
#' This function generates data and initial values for Stan modeling.
#'
#' @param issue_code_vec A vector of issue codes
#' @param rollcall A roll call object (output of `pscl::rollcall`)
#' @param ideal An ideal object (output of `pscl::ideal`)
#' @param a hyperparameter for the prior of rho 0<b<a
#' @param b hyperparameter for the prior of theta 0<b<a
#' @param rho_init initial value for rho
#'
#' @return A list of data and initial values for Stan
#'
#' @export
make_stan_input <- function(issue_code_vec, rollcall, ideal, a = 0.01, b = 0.001, rho_init = 10) {
  z <- issue_code_vec
  K <- length(unique(z))
  votes <- rollcall$votes
  J <- nrow(votes)
  M <- ncol(votes)
  if (!is.numeric(z)) stop("z must be a numeric vector.")
  if (length(unique(z)) != max(z)) stop("z must be a vector of consecutive integers starting from 1.")
  if (length(z) != M) {
    stop("Length of z and the number of roll calls must be the same.")
  }
  if (dim(ideal$betabar)[1] != M) stop("Number of rollcalls in rollcall and ideal must be the same.")
  N <- J * M
  j <- rep(1:J, M)
  m <- rep(1:M, each = J)
  votes_vec <- as.vector(votes)
  yea_code <- rollcall$codes$yea
  nay_code <- rollcall$codes$nay
  votes_vec[votes_vec == yea_code] <- 1
  votes_vec[votes_vec == nay_code] <- 0
  votes_vec[!votes_vec %in% c(yea_code, nay_code)] <- NA
  N_obs <- sum(!is.na(votes_vec))
  N_mis <- sum(is.na(votes_vec))
  y_obs <- votes_vec[!is.na(votes_vec)]
  idx_obs <- which(!is.na(votes_vec))
  idx_mis <- which(is.na(votes_vec))

  stan <- list(
    J = J,
    M = M,
    N = N,
    N_obs = N_obs,
    N_mis = N_mis,
    K = K,
    j = j,
    m = m,
    y_obs = y_obs,
    idx_obs = idx_obs,
    idx_mis = idx_mis,
    z = z,
    a = a,
    b = b
  )

  # ideal points
  x <- ideal$xbar
  # rho
  rho <- rho_init
  # difficulty and discrimination (in x,y coordinates)
  alpha <- ideal$betabar[, "Difficulty"]
  beta1 <- ideal$betabar[, "Discrimination D1"]
  beta2 <- ideal$betabar[, "Discrimination D2"]
  w <- sqrt(beta1^2 + beta2^2)
  x_coord_u <- beta1 / w
  y_coord_u <- beta2 / w

  # convert to polar coordinates
  polar_u <- cartesian_to_polar(x = x_coord_u, y = y_coord_u)
  r_u <- polar_u$r # by the design of w and x_coord_u, y_coord_u, r_u = 1 for all u
  u <- polar_u$theta

  # theta initial values
  theta <- tapply(u, z, mean)
  r_theta <- rep(1, K)

  # convert to cartesian coordinates
  cart_theta <- polar_to_cartesian(theta = theta, r = r_theta)
  x_coord_theta <- cart_theta$x
  y_coord_theta <- cart_theta$y

  init <- list(
    alpha = alpha,
    rho = rho,
    u = u,
    x = x,
    theta = theta,
    w = w,
    r_theta = r_theta,
    r_u = r_u,
    x_coord_theta = x_coord_theta,
    y_coord_theta = y_coord_theta,
    x_coord_u = x_coord_u,
    y_coord_u = y_coord_u
  )

  res <- list(data = stan, init = init)

  return(res)
}

