#' Find Most Polarized Bills in Horizontal Dimension
#'
#' This function identifies the most polarized bills based on voting patterns in a horizontal dimension from roll call data.
#'
#' @param rollcall A roll call object (output of `pscl::rollcall`; yea_code = 1, nay_code = 0).
#' @param party_code_col A character string specifying the column name for party code (default: "party_code").
#' @param liberal_code Party code for the liberal party (default: 100).
#' @param conservative_code Party code for the conservative party (default: 200).
#' @param na_threshold The maximum ratio of missing data for a bill to be considered (default: 0.3).
#' @return A character string representing the roll call ID of the most polarized bill.
#' @importFrom dplyr filter group_by summarize mutate arrange left_join case_when desc
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils head
#' @importFrom rlang .data
#' @export
find_pol_rc_horizontal <- function(rollcall,
                                   party_code_col = "party_code",
                                   liberal_code = 100,
                                   conservative_code = 200,
                                   na_threshold = 0.3) {
  party_code <- rollcall$legis.data[[party_code_col]]
  votes <- rollcall$votes
  legis_id <- rownames(votes)
  votes_long <- votes |>
    as_tibble() |>
    mutate(legis_id = legis_id) |>
    pivot_longer(cols = -legis_id, names_to = "bill_id", values_to = "vote")

  # summarize ratio of na
  cand_bill_id <- votes_long |>
    group_by(.data$bill_id) |>
    summarize(na_ratio = mean(is.na(.data$vote)), .groups = "drop") |>
    # filter bills with na ratio less than threshold
    filter(.data$na_ratio < na_threshold)

  liberal_legis <- legis_id[party_code == liberal_code]
  conservative_legis <- legis_id[party_code == conservative_code]

  # identify the most polarized bill
  rc_id <- votes_long |>
    filter(.data$bill_id %in% cand_bill_id$bill_id) |>
    mutate(party = case_when(.data$legis_id %in% liberal_legis ~ "D",
                             .data$legis_id %in% conservative_legis ~ "R",
                             TRUE ~ NA_character_)) |>
    group_by(.data$bill_id, .data$party) |>
    summarize(vote = mean(.data$vote, na.rm = TRUE), .groups = "drop") |>
    filter(!is.na(.data$party)) |>
    pivot_wider(names_from = .data$party, values_from = .data$vote) |>
    mutate(diff = .data$R - .data$D) |>
    filter((.data$D > 0.5 & .data$R < 0.5) | (.data$D < 0.5 & .data$R > 0.5)) |>
    left_join(cand_bill_id, by = "bill_id") |>
    arrange(desc(.data$diff), .data$na_ratio) |>
    head(1) |>
    pull(.data$bill_id)

  return(rc_id)
}


#' Find Most Polarized Bills in Vertical Dimension
#'
#' This function identifies the most polarized bills in the vertical dimension based on ideal point estimates and roll call data.
#'
#' @param ideal An ideal object (output of `pscl::ideal`).
#' @param rollcall A roll call object (output of `pscl::rollcall`; yea_code = 1, nay_code = 0).
#' @param pol_rc_horizontal A character vector of roll call IDs for the most polarized bill in the horizontal dimension (default: NULL, will be computed if not provided).
#' @param party_code_col A character string specifying the column name for party code (default: "party_code").
#' @param liberal_code Party code for the liberal party (default: 100).
#' @param conservative_code Party code for the conservative party (default: 200).
#' @param na_threshold The maximum ratio of missing data for a bill to be considered (default: 0.3).
#' @param lop_threshold The minimum level of support (or oppose) for a bill to be considered (default: 0.1).
#' @return A character string representing the roll call ID of the most polarized bill in the vertical dimension.
#' @importFrom dplyr filter group_by summarize mutate arrange left_join if_else pull relocate ungroup rowwise
#' @importFrom tidyr pivot_longer
#' @importFrom utils head
#' @importFrom rlang .data
#' @export
find_pol_rc_vertical <- function(ideal, rollcall,
                                 pol_rc_horizontal = NULL,
                                 party_code_col = "party_code",
                                 liberal_code = 100,
                                 conservative_code = 200,
                                 na_threshold = 0.3,
                                 lop_threshold = 0.1) {
  if (is.null(pol_rc_horizontal)) {
    pol_rc_horizontal <- find_pol_rc_horizontal(rollcall = rollcall,
                                                party_code_col = party_code_col,
                                                liberal_code = liberal_code,
                                                conservative_code = conservative_code,
                                                na_threshold = na_threshold)
  }
  votes <- rollcall$votes
  legis_id <- rownames(votes)
  votes_long <- votes |>
    as_tibble() |>
    mutate(legis_id = legis_id) |>
    pivot_longer(cols = -legis_id, names_to = "bill_id", values_to = "vote")

  # summarize ratio of na
  cand_bill_id <- votes_long |>
    group_by(.data$bill_id) |>
    summarize(na_ratio = mean(is.na(.data$vote)), n1 = sum(.data$vote, na.rm = TRUE),
              n0 = sum(.data$vote == 0, na.rm = TRUE), .groups = "drop") |>
    # filter bills with na ratio less than threshold
    filter(.data$na_ratio < na_threshold)

  # get discrimination parameters of the most polarized bill in horizontal dimension
  disc_rc1 <- ideal$betabar[pol_rc_horizontal, 1:2]
  # compute cosine similarity of discrimination parameters of each bill with pol_rc_horizontal
  betabar <- ideal$betabar |>
    as_tibble() |>
    mutate(bill_id = rownames(ideal$betabar)) |>
    rowwise() |>
    mutate(cossim = cosine_similarity(disc_rc1[1], disc_rc1[2],
                                      .data$`Discrimination D1`, .data$`Discrimination D2`)) |>
    ungroup()

  # find a roll call whose discrimination is the most orthogonal to the most polarized bill in horizontal dimension
  rc_id <- betabar |>
    filter(.data$bill_id %in% cand_bill_id$bill_id) |>
    left_join(cand_bill_id, by = "bill_id") |>
    arrange(abs(.data$cossim)) |>
    mutate(lop = if_else(.data$n1 > .data$n0, .data$n0 / (.data$n1 + .data$n0), .data$n1 / (.data$n1 + .data$n0))) |>
    filter(.data$lop > lop_threshold) |>
    relocate(.data$bill_id, .data$na_ratio, .data$lop, .data$cossim) |>
    head(1) |>
    pull(.data$bill_id)

  return(rc_id)
}

#' Rotate and Normalize Posterior Mean of Bayesian IRT
#'
#' This function rotates and normalizes the posterior mean of Bayesian Item Response Theory (IRT)
#' based on specified criteria. It adjusts the orientation of the IRT space based on the most
#' polarized bills and ensures that specified parties are correctly positioned in the rotated space.
#'
#' @param ideal An ideal object (output of `pscl::ideal`).
#' @param rollcall A roll call object (output of `pscl::rollcall`; yea_code = 1, nay_code = 0).
#' @param pol_rc1 A character representing the roll call ID for the most polarized bill in the horizontal dimension.
#' @param pol_rc2 A character representing the roll call ID for the most polarized bill in the vertical dimension (optional).
#' @param party_code_col A character specifying the column name for party code.
#' @param left_party_code A party code for the party that should be on the left of the horizontal dimension.
#' @param top_party_code A party code for the party that should be on top of the vertical dimension (optional).
#' @return A matrix of the posterior mean of Bayesian IRT with two columns (x1 and x2), each normalized to have mean 0 and standard deviation 1.
#' @export
postprocess_xbar <- function(ideal, rollcall, pol_rc1, pol_rc2 = NULL, party_code_col = "party_code",
                             left_party_code = 100,
                             top_party_code = NULL) {
  if(is.null(pol_rc1)) stop("pol_rc1 must be specified")
  if(is.null(pol_rc2) & is.null(top_party_code)) stop("pol_rc2 or top_party_code must be specified")

  party_code <- rollcall$legis.data[[party_code_col]]
  disc_rc1 <- ideal$betabar[pol_rc1, c("Discrimination D1", "Discrimination D2")]
  xbar_rotated <- rotate_matrix(ideal$xbar, slope = disc_rc1[2]/disc_rc1[1], origin = c(0,0))

  # Check whether specified legislators are on left, if not, flip
  horiz_flip <- FALSE
  if(!is.null(left_party_code)) {
    votes <- rollcall$votes
    legis_id <- rownames(votes)
    left_legis <- legis_id[party_code == left_party_code]
    horiz_flip <- (mean(xbar_rotated[left_legis,1]) > 0)
  }

  if(horiz_flip) {
    xbar_rotated[,1] <- -xbar_rotated[,1]
  }

  # Check whether specified legislators are on top, if not, flip
  vert_flip <- FALSE
  if(!is.null(top_party_code)) {
    votes <- rollcall$votes
    legis_id <- rownames(votes)
    top_legis <- legis_id[party_code == top_party_code]
    vert_flip <- mean(xbar_rotated[top_legis,2]) < 0
  } else {
    votes <- rollcall$votes
    vert_flip <- mean(xbar_rotated[which(votes[,pol_rc2] == 0),2]) < mean(xbar_rotated[which(votes[,pol_rc2] == 1),2])
  }

  if(vert_flip) {
    xbar_rotated[,2] <- -xbar_rotated[,2]
  }

  # Normalize each column of xbar_rotated to have mean 0 and sd 1
  xbar_normalized <- scale(xbar_rotated, center = TRUE, scale = TRUE)
  return(xbar_normalized)
}
#' Find the Constraints in Legislative Voting Data
#'
#' This function identifies constraints in legislative voting data based on polarization,
#' party codes, and specified thresholds.
#'
#' @param ideal An ideal object (output of `pscl::ideal`).
#' @param rollcall A roll call object (output of `pscl::rollcall`; yea_code = 1, nay_code = 0).
#' @param pol_rc1 A character representing the roll call ID for the most polarized bill in the horizontal dimension.
#' @param pol_rc2 A character representing the roll call ID for the most polarized bill in the vertical dimension (optional).
#' @param party_code_col A character specifying the column name for party code.
#' @param left_party_code A party code for the party that should be on the left of the horizontal dimension.
#' @param top_party_code A party code for the party that should be on top of the vertical dimension (optional).
#' @param top_perc The percentage of legislators to be included based on their total number of votes cast.
#' @param x2_threshold The threshold for the vertical dimension for the first constraint (left of triangle).
#' @param x2_threshold_second The threshold for the vertical dimension for the second constraint (right of triangle).
#' @param vert_flip Whether to flip the second coordinate.
#' @param top_n A fraction of the data you want to select. For example, if top_n is set to 0.5, it means you want to select the top 50% of the legislators.
#' @param as_list Boolean indicating whether to return the constraints as a list (default is FALSE).
#' @return A tibble or list of legislator IDs along with their x1 and x2 coordinates based on the constraints.
#' @importFrom dplyr filter arrange slice bind_rows tibble n
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom stats quantile setNames
#' @export
find_constraints <- function(ideal, rollcall, pol_rc1, pol_rc2 = NULL, party_code_col = "party_code", left_party_code = 100,
                             top_party_code = NULL, top_perc = 0.9, x2_threshold = 0, x2_threshold_second = NULL,
                             vert_flip = FALSE, top_n = 0.1, as_list = FALSE) {
  if(is.null(pol_rc1)) stop("pol_rc1 must be specified")
  if(is.null(pol_rc2) & is.null(top_party_code)) stop("pol_rc2 or top_party_code must be specified")

  # Postprocess xbar
  xbar <- postprocess_xbar(ideal = ideal, rollcall = rollcall, pol_rc1 = pol_rc1, pol_rc2 = pol_rc2,
                           party_code_col = party_code_col, left_party_code = left_party_code,
                           top_party_code = top_party_code)
  legis_total_votes <- rowSums(!is.na(rollcall$votes))

  # Select top legislators based on total number of votes cast
  xbar_top <- xbar[legis_total_votes > quantile(legis_total_votes, probs = 1 - top_perc),]
  xbar_top <- tibble(legis_id = rownames(xbar_top), x1 = xbar_top[,1], x2 = xbar_top[,2])

  if(is.null(x2_threshold_second)) {
    x2_threshold_second <- Inf
  }

  # Identify constraints based on specified criteria
  constraint <- xbar_top |>
    filter(.data$x2 < x2_threshold) |>
    arrange(.data$x1) |>
    slice(ceiling(top_n * n()))
  constraint <- xbar_top |>
    filter(.data$x2 < x2_threshold_second) |>
    arrange(desc(.data$x1)) |>
    slice(ceiling(top_n * n())) |>
    bind_rows(constraint)
  constraint <- xbar_top |>
    arrange(desc(.data$x2)) |>
    slice(ceiling(top_n * n())) |>
    bind_rows(constraint)

  # Flip vertical axis if required
  if(vert_flip) {
    constraint <- constraint |>
      mutate(x2 = -.data$x2)
  }

  # Convert to list format if required
  if(as_list) {
    constraint <- purrr::map2(constraint$x1, constraint$x2, ~ c(.x, .y)) |>
      setNames(constraint$legis_id)
  }

  return(constraint)
}

