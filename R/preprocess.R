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
