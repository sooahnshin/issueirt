#' Generate dynamic version of rollcall object
#'
#' @param votes_list A list of roll call votes data. Each element of the list is a data frame where rows are legislators and columns are roll call votes.
#' @param issue_list A list of issue data. Each element of the list is a data frame where each row is an issue and columns are issue information.
#' @param legis_list A list of legislator data. Each element of the list is a data frame where each row is a legislator and columns are legislator information.
#' @param colname_legis A character string specifying the column name of legislator ID in legis_list.
#' @param colname_party A character string specifying the column name of party affiliation in legis_list. If NULL, party affiliation is not included in the rollcall object.
#' @param bills_list A list of bill data. Each element of the list is a data frame where each row is a bill and columns are bill information.
#' @param colname_bills A character string specifying the column name of bill ID in bills_list. If NULL, bill ID is not included in the rollcall object.
#' @param term_name A character vector specifying the name of each term. If NULL, the default term name is "T1", "T2", ..., "Tn".
#' @param left_code A numeric value specifying the code for "left-wing" votes.
#' @param right_code A numeric value specifying the code for "right-wing" votes.
#' @param missing_code A numeric value specifying the code for missing votes.
#' @param notInLegis_code A numeric value specifying the code for legislators who did not vote.
#' @return A dynamic_rollcall object.
#' @importFrom pscl rollcall
#' @importFrom dplyr arrange pull sym mutate rename_with select across distinct
#' @importFrom purrr map reduce
#' @export
make_dynamic_rollcall <- function(votes_list,
                                  issue_list,
                                  legis_list,
                                  colname_legis,
                                  colname_party = NULL,
                                  bills_list = NULL,
                                  colname_bills = NULL,
                                  term_name = NULL,
                                  left_code = 1,
                                  right_code = 0,
                                  missing_code = NA,
                                  notInLegis_code = 9) {
  ## check if all list data have the same length
  if (length(votes_list) != length(issue_list)) {
    stop("votes_list and issue_list must have the same length")
  }
  if (length(votes_list) != length(legis_list)) {
    stop("votes_list and legis_list must have the same length")
  }
  if (!is.null(bills_list) && length(votes_list) != length(bills_list)) {
    stop("votes_list and bills_list must have the same length")
  }

  if(!left_code %in% unlist(votes_list)) {
    stop("Check left_code: not appeared in votes_list")
  }
  if(!right_code %in% unlist(votes_list)) {
    stop("Check right_code: not appeared in votes_list")
  }



  n_terms <- length(votes_list)

  ## check if all list data have the same number of roll calls
  for(i in 1:n_terms) {
    if(ncol(votes_list[[i]]) != length(issue_list[[i]]$issue_code_vec)) {
      stop("Check issue_list: issue_list is an output of make_issue_code function and must have the same number of bills as votes_list")
    }
  }

  ## check if all list data have the same number of legislators & sorted by colname_legis
  for(i in 1:n_terms) {
    if(nrow(votes_list[[i]]) != nrow(legis_list[[i]])) {
      stop("votes_list and legis_list must have the same number of legislators for each term")
    }
    tmp.sorted_legis <- legis_list[[i]] |> arrange(!!sym(colname_legis)) |> pull(colname_legis)
    tmp.legis <- legis_list[[i]] |> pull(colname_legis)
    if(!identical(tmp.sorted_legis, tmp.legis)) {
      stop("Each element of legis_list must be sorted by colname_legis. Please make sure that the roll call votes data are also sorted by colname_legis")
    }
  }

  ## check if all list data have the same number of bills
  if (!is.null(bills_list)) {
    for(i in 1:n_terms) {
      if(ncol(votes_list[[i]]) != nrow(bills_list[[i]])) {
        stop("votes_list and bills_list must have the same number of bills for each term")
      }
    }
  }

  ## check if colname_legis is in legis_list
  for(i in 1:n_terms) {
    if (!colname_legis %in% colnames(legis_list[[i]])) {
      stop("colname_legis must be in legis_list")
    }
  }

  ## check if colname_bills is in bills_list
  if (!is.null(bills_list)) {
    for(i in 1:n_terms) {
      if (!colname_bills %in% colnames(bills_list[[i]])) {
        stop("colname_bills must be in bills_list")
      }
    }
  }

  ## set names
  if(is.null(term_name)) {
    term_name <- paste0("T", 1:n_terms)
  }
  if(is.null(colname_bills)) {
    colname_bills <- "rollnumber"
    bills_list <- map(1:n_terms, ~data.frame(rollnumber = 1:ncol(votes_list[[.x]])))
  }

  ## combine votes data
  votes_df_list <- map(1:n_terms, function(t) {
    votes_list[[t]] |>
      as_tibble() |>
      rename_with(~ paste0(term_name[t], "_", bills_list[[t]] |> pull(colname_bills))) |>
      mutate(legis_id = legis_list[[t]] |> pull(colname_legis))
  })

  votes_df <- reduce(votes_df_list, full_join, by = "legis_id") |>
    arrange(legis_id)
  votes_mat <- votes_df |> select(-legis_id) |> as.matrix()
  rownames(votes_mat) <- votes_df$legis_id

  if(is.null(colname_party)) {
    legis.data <- legis_list |>
      map(~ select(.x, all_of(colname_legis))) |>
      bind_rows() |>
      distinct() |>
      arrange(across(all_of(colname_legis)))
  } else {
    legis.data <- legis_list |>
      map(~ select(.x, all_of(c(colname_legis, colname_party)))) |>
      bind_rows() |>
      distinct() |>
      arrange(across(all_of(colname_legis)))
  }

  ## check if colname_legis is duplicated
  if (any(duplicated(legis.data |> pull(colname_legis)))) {
    message("Check legislator data: change of party affiliation is detected. The output `rollcall` object records the first party affiliation.")
    legis.data <- legis.data |> distinct(across(all_of(colname_legis)), .keep_all = TRUE)
  }

  rc_input <- pscl::rollcall(votes_mat,
                             yea = left_code,
                             nay = right_code,
                             missing = missing_code,
                             notInLegis = notInLegis_code,
                             legis.names = legis.data |> pull(colname_legis),
                             legis.data = as.data.frame(legis.data),
                             vote.names = colnames(votes_mat))
  res <- list(
    rollcall = rc_input,
    votes_list = votes_list,
    issue_list = issue_list,
    legis_list = legis_list,
    bills_list = bills_list,
    call = match.call()
  )
  class(res) <- c("dynamic_rollcall", class(res))
  return(res)
}

#' Generate dynamic version of stan input object
#'
#' @param dynamic_rollcall A dynamic_rollcall object.
#' @param ideal An ideal object.
#' @param a hyperparameter for the prior of rho 0<b<a
#' @param b hyperparameter for the prior of theta 0<b<a
#' @param rho_init initial value for rho
#' @param sd_dynamic standard deviation for dynamic prior
#' @return A dynamic_stan_input object.
#' @importFrom dplyr bind_rows mutate relocate select distinct arrange bind_rows left_join rename join_by
#' @importFrom tidyr separate
#' @importFrom purrr map
#' @export
make_dynamic_stan_input <- function(dynamic_rollcall,
                                    ideal,
                                    a = 0.01,
                                    b = 0.001,
                                    rho_init = 10,
                                    sd_dynamic = 0.01) {
  if(!inherits(dynamic_rollcall, "dynamic_rollcall")) {
    stop("Input must be an object of class `dynamic_rollcall`")
  }
  if(!inherits(ideal, "ideal")) {
    stop("Input must be an object of class `ideal`")
  }
  # convert the call object to a list
  call_list <- as.list(dynamic_rollcall$call)
  # extract arguments
  args <- call_list[-1]

  votes_list <- dynamic_rollcall$votes_list
  issue_list <- dynamic_rollcall$issue_list
  legis_list <- dynamic_rollcall$legis_list
  bills_list <- dynamic_rollcall$bills_list
  colname_legis <- args$colname_legis


  if(is.null(args$term_name)) {
    term_name <- paste0("T", 1:length(dynamic_rollcall$votes_list))
  } else {
    term_name <- eval(args$term_name)
  }

  n_terms <- length(term_name)
  yea_code <- dynamic_rollcall$rollcall$codes$yea
  nay_code <- dynamic_rollcall$rollcall$codes$nay

  ## summary of each term
  n_df <- data.frame()
  y_vec_ls <- y_vec_obs_ls <- list()
  for(t in 1:n_terms) {
    votes_vec <- as.vector(votes_list[[t]])
    votes_vec[votes_vec %in% yea_code] <- 1
    votes_vec[votes_vec %in% nay_code] <- 0
    votes_vec[!votes_vec %in% c(yea_code, nay_code)] <- NA
    y_vec_ls[[t]] <- votes_vec
    y_vec_obs_ls[[t]] <- votes_vec[!is.na(votes_vec)]
    n_df <- n_df |>
      bind_rows(data.frame(n = nrow(votes_list[[t]]),
                           m = ncol(votes_list[[t]]),
                           N = nrow(votes_list[[t]]) * ncol(votes_list[[t]]),
                           n_mis = sum(is.na(votes_vec)),
                           n_obs = sum(!is.na(votes_vec))))
  }

  ## full bills data
  if(is.null(bills_list)) {
    bills_list <- map(1:n_terms, ~data.frame(term = term_name[.x], rollnumber = 1:ncol(votes_list[[.x]])))
    colname_bills <- "rollnumber"
  } else {
    colname_bills <- eval(args$colname_bills)
  }
  bills_df <- map(1:n_terms, ~bills_list[[.x]] |> mutate(tmp_term = term_name[.x])) |>
    bind_rows() |>
    mutate(term_rollnumber = paste0(tmp_term, "_", !!sym(colname_bills))) %>%
    relocate(term_rollnumber) |>
    select(-tmp_term)

  ## full legis data
  legis_df <- map(1:n_terms, ~legis_list[[.x]] |> mutate(tmp_term = term_name[.x])) |>
    bind_rows() |>
    mutate(term_legis = paste0(tmp_term, "_", !!sym(colname_legis))) %>%
    relocate(term_legis) |>
    select(-tmp_term)

  legis_term_df <- map(legis_list, ~ select(., !!sym(colname_legis))) |> bind_rows() |> distinct() |> arrange(!!sym(colname_legis))

  ## full issue data
  issue_codebook <- map(1:n_terms, ~issue_list[[.x]]$codebook |> mutate(term = term_name[.x])) |>
    bind_rows() |>
    mutate(term_issue = paste0(term, "_", code),
           term_label = paste0(term, "_", label)) %>%
    relocate(term_issue)
  issue_code_vec <- unlist(map(1:n_terms, ~paste0(term_name[.x], "_", issue_list[[.x]]$codebook$label[issue_list[[.x]]$issue_code_vec])))
  if(sum(n_df$m) != length(issue_code_vec)) {
    stop("Check issue_list: issue_list is an output of make_issue_code function and must have the same number of bills as votes_list")
  }
  issue_code_df <- make_issue_code(issue_code_vec = issue_code_vec,
                                   levels = issue_codebook$term_label)
  issue_code_df$codebook <- issue_code_df$codebook |>
    left_join(issue_codebook |>
                select(term, label, term_label),
              by = join_by(label == term_label)) |>
    rename(category = label.y)

  ## create stan data input
  for(t in 1:n_terms) {
    legis_term_df <- legis_term_df |>
      left_join(legis_list[[t]] |>
                  mutate(term = t) |>
                  select(!!sym(colname_legis), term) |>
                  rename_with(~ paste0(term_name[t]), -!!sym(colname_legis)), by = colname_legis)
  }
  t_ls <- legis_term_df %>%
    select(all_of(term_name)) %>%
    apply(1, function(row) row[!is.na(row)]) %>%
    lapply(as.numeric)
  names(t_ls) <- legis_term_df |> pull(colname_legis)

  ## legis-term index
  legis_terms <- map(1:length(t_ls), ~paste0(names(t_ls)[.x], "_", t_ls[[.x]])) |> unlist() # a list of unique legis-term
  n_legis_terms <- length(legis_terms) # number of unique legis-term
  n <- length(t_ls) # number of legislators
  # create an index for the prior
  prior_idx <- map(1:n, function(i) {
    v <- t_ls[[i]]
    t <- length(v)
    if(t == 1) {
      res <- NA
    } else {
      res <- rep(NA, t)
      for (j in 2:t) {
        res[j] <- which(legis_terms == paste0(names(t_ls)[i], "_", v[j-1]))
      }
    }
    return(res)
  }) |>
    unlist()
  omega_vec <- rep(1, length(prior_idx))
  omega_vec[!is.na(prior_idx)] <- sd_dynamic
  prior_idx[is.na(prior_idx)] <- n_legis_terms + 1

  j_vec <- map(1:n_terms, ~paste0(rep(legis_list[[.x]] |> pull(colname_legis), n_df$m[.x]), "_", .x)) |> unlist()
  j_vec <- as.numeric(factor(j_vec, levels = legis_terms))
  m_cumsum <- n_df$m |> cumsum()
  m_cumsum_begin <- c(1, m_cumsum[1:(n_terms - 1)] + 1)
  m_vec <- map(1:n_terms, ~rep(m_cumsum_begin[.x]:m_cumsum[.x], each = n_df$n[.x])) |> unlist()

  ## create stan init input
  rc_input <- dynamic_rollcall$rollcall
  stan_input <- make_stan_input(
    issue_code_vec = issue_code_df$issue_code_vec, rollcall = rc_input, ideal = ideal,
    a = a, b = b, rho_init = rho_init
  )
  stan_input$init$x <- tibble(legis = legis_terms) |>
    separate(legis, into = c("legis", "term"), sep = "_") |>
    left_join(tibble(legis = rc_input$legis.data |> pull(colname_legis) |> as.character(),
                     x1 = stan_input$init$x[,1], x2 = stan_input$init$x[,2]),
              by = "legis") |>
    select(x1, x2) |>
    as.matrix()

  res <- list()
  res$data <- list(
    J = sum(n_df$n),
    M = sum(n_df$m),
    N = sum(n_df$N),
    N_obs = sum(n_df$n_obs),
    # N_mis = sum(n_df$n_mis),
    K = nrow(issue_code_df$codebook),
    j = j_vec,
    m = m_vec,
    y_obs = unlist(y_vec_obs_ls),
    idx_obs = which(!is.na(unlist(y_vec_ls))),
    # idx_mis = unlist(filt_idx_mis_ls),
    z = issue_code_df$issue_code_vec,
    a = a,
    b = b,
    omega = omega_vec,
    idx = prior_idx
  )
  res$init <- stan_input$init
  res$misc <- list(
    n_df = n_df,
    legis_term = legis_terms,
    issue_code_df = issue_code_df,
    legis_df = legis_df,
    bills_df = bills_df,
    call = match.call()
  )
  class(res) <- c("dynamic_stan_input", class(res))
  return(res)
}

#' Generate dynamic version of constraints (for visualization of multidimensional ideal points)
#'
#' @param dynamic_stan_fit A stan fit object.
#' @param dynamic_stan_input A dynamic_stan_input object.
#' @param anchors_name A character vector specifying the name of each anchor.
#' @return A list of constraints for visualization.
#' @importFrom rstan extract
#' @export
get_dynamic_constraints <- function(dynamic_stan_fit, dynamic_stan_input, anchors_name) {
  x_samples <- rstan::extract(dynamic_stan_fit, "x")$x
  ideal_point_1d <- x_samples[,,1] |> apply(2, mean)
  ideal_point_2d <- x_samples[,,2] |> apply(2, mean)
  idx <- which(dynamic_stan_input$misc$legis_term %in% anchors_name)
  anchors_name <- dynamic_stan_input$misc$legis_term[idx]
  const_ls <- list()
  for(i in 1:length(anchors_name)) {
    const_ls[[anchors_name[i]]] <- c(ideal_point_1d[idx[i]], ideal_point_2d[idx[i]])
  }
  return(const_ls)
}
