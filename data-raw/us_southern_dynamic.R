## The Rise of Populism and Sectional Division in the 1890s (The 52nd to 54th US House of Representatives)
## See vignette("dynamic_issueirt") for more details

library(issueirt)

## to visualize the data
library(tidyverse)
theme_set(theme_classic(base_size = 15) + theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)))

southern <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "OK", "TX")
breaks.group <- c("Southern Democrat", "Republican", "Democrat", "Ind. Democrat", "Populist", "Silver")
values.shape <- c(19, 15, 19, 19, 17, 17)
values.color <- c("#e36934", "#DE0100", "#1405BD", "#635FBD", "#62bd6d", "#777777")
years <- map(52:54, ~get_congress_years(.))
years <- paste0(str_extract(years[[1]][1], "[0-9][0-9][0-9][0-9]"), " ", str_extract(years[[length(years)]][1], "- [0-9][0-9][0-9][0-9]"))
irt.title <- paste("Ideal Points Measured with BIRT\nThe U.S. House of Representatives (", years, ")", sep = "")
issueirt.title <- paste("Ideal Points Measured with Issue IRT\nThe U.S. House of Representatives (", years, ")", sep = "")
us1890s_legis <- map(1:length(us1890s_legis), ~us1890s_legis[[.x]] |>
                       mutate(group = ifelse(state_abbrev %in% southern & party_name == "Democrat", "Southern Democrat", party_name)))

## filter votes
n_terms <- length(us1890s_votes)
us1890s_rc <- map(1:n_terms, ~pscl::rollcall(us1890s_votes[[.x]], yea = 1, nay = 0, missing = c(NA, 2, 3)))
us1890s_filtered <- map(1:n_terms, ~filter_votes(rollcall = us1890s_rc[[.x]], lop = 0, minvotes = 20))
us1890s_votes_filtered <- map(1:n_terms, ~as.matrix(us1890s_votes[[.x]])[us1890s_filtered[[.x]]$legis, us1890s_filtered[[.x]]$bills])
us1890s_legis_filtered <- map(1:n_terms, ~us1890s_legis[[.x]][us1890s_filtered[[.x]]$legis, ])
us1890s_bills_filtered <- map(1:n_terms, ~us1890s_bills[[.x]][us1890s_filtered[[.x]]$bills, ])
us1890s_issue_filtered <- map(1:n_terms, ~us1890s_issue[[.x]][us1890s_filtered[[.x]]$bills, ])

us1890s_issue_code <- map(us1890s_issue_filtered, ~make_issue_code(issue_code_vec = .x$issue_label,
                                                                   level = .x |>
                                                                     arrange(issue_level) |>
                                                                     select(issue_label, issue_level) |>
                                                                     distinct() |>
                                                                     pull(issue_label)))

us1890s_votes_recoded <- map(1:n_terms, ~recode_votes(us1890s_votes_filtered[[.x]],
                                                      party_code = us1890s_legis_filtered[[.x]]$party_name,
                                                      yea = 1,
                                                      nay = 0,
                                                      missing = c(NA, 2, 3),
                                                      liberal_code = "Democrat",
                                                      conservative_code = "Republican",
                                                      na_threshold = 0.5))

dynamic_rc <- make_dynamic_rollcall(votes_list = us1890s_votes_recoded,
                                    issue_list = us1890s_issue_code,
                                    legis_list = us1890s_legis_filtered,
                                    colname_legis = "icpsr",
                                    colname_party = "group",
                                    bills_list = us1890s_bills_filtered,
                                    colname_bills = "rollnumber",
                                    term_name = c("H52", "H53", "H54"),
                                    left_code = 1,
                                    right_code = 0,
                                    missing_code = NA,
                                    notInLegis_code = 9)

## fit the Bayesian IRT model
set.seed(1)
ideal <- pscl::ideal(dynamic_rc$rollcall, dropList = list(lop = 0, legisMin = 0), # no need to further drop votes and legislators
                     priors = NULL, startvals = "eigen", # default priors and starting values
                     d = 2, maxiter = 500, thin = 1, burnin = 200,
                     impute = FALSE, normalize = FALSE,
                     store.item = TRUE, file = NULL, verbose = FALSE)

pol_rc1 <- find_pol_rc_horizontal(dynamic_rc$rollcall, party_code_col = "group",
                                  liberal_code = "Democrat", conservative_code = "Republican",
                                  na_threshold = 0.5)
pol_rc2 <- find_pol_rc_vertical(ideal, dynamic_rc$rollcall, pol_rc1,
                                party_code_col = "group",
                                na_threshold = 0.5, lop_threshold = 0.1)
const_ls <- find_constraints(ideal, dynamic_rc$rollcall, pol_rc1 = pol_rc1, pol_rc2 = pol_rc2,
                             party_code_col = "group", left_party_code = "Democrat",
                             as_list = TRUE)
print(const_ls)

## suppress redundant messages
invisible(capture.output({
  ideal_pp <- pscl::postProcess(ideal, constraints = const_ls)
}))

plot_ideal(ideal_point_1d = ideal_pp$xbar[,1], ideal_point_2d = ideal_pp$xbar[,2],
           group = dynamic_rc$rollcall$legis.data$group, p.title = irt.title,
           breaks.group = breaks.group, values.shape = values.shape, values.color = values.color)

dynamic_stan_input <- make_dynamic_stan_input(dynamic_rollcall = dynamic_rc,
                                              ideal = ideal_pp,
                                              a = 0.01,
                                              b = 0.001,
                                              rho_init = 10,
                                              sd_dynamic = 0.01)

fit_dynamic <- issueirt_dynamic_stan(
  data = dynamic_stan_input$data,
  init = list(dynamic_stan_input$init, dynamic_stan_input$init), # starting values
  chains = 2,             # number of Markov chains
  warmup = 10,            # number of warmup iterations per chain
  iter = 20,              # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
)

## save
load("R/sysdata.rda") # to load fit_sim from synthetic_fit.R
usethis::use_data(fit_dynamic, fit_sim, overwrite = TRUE, internal = TRUE)


##################################################
# Fix the sign-flip and re-run the model
##################################################

anchors <- map(names(const_ls), ~str_subset(dynamic_stan_input$misc$legis_term, .x) %>% .[1]) |> unlist()
print(anchors)
const_dynamic <- get_dynamic_constraints(fit_dynamic, dynamic_stan_input, anchors)
posterior_summary_pp <- make_posterior_summary_postprocessed(
  stan_fit = fit_dynamic,
  constraints = const_dynamic,
  issue_label = dynamic_stan_input$misc$issue_code_df$codebook$label,
  rc_label = dynamic_stan_input$misc$bills_df$term_rollnumber,
  legis_label = dynamic_stan_input$misc$legis_term,
  missing_label = NULL
)

us1890s_votes_recoded <- update_recode_votes(us1890s_votes_recoded, dynamic_stan_input, posterior_summary_pp)

dynamic_rc <- make_dynamic_rollcall(votes_list = us1890s_votes_recoded,
                                    issue_list = us1890s_issue_code,
                                    legis_list = us1890s_legis_filtered,
                                    colname_legis = "icpsr",
                                    colname_party = "group",
                                    bills_list = us1890s_bills_filtered,
                                    colname_bills = "rollnumber",
                                    term_name = c("H52", "H53", "H54"),
                                    left_code = 1,
                                    right_code = 0,
                                    missing_code = NA,
                                    notInLegis_code = 9)

## fit the Bayesian IRT model
set.seed(1)
ideal <- pscl::ideal(dynamic_rc$rollcall, dropList = list(lop = 0, legisMin = 0), # no need to further drop votes and legislators
                     priors = NULL, startvals = "eigen", # default priors and starting values
                     d = 2, maxiter = 500, thin = 1, burnin = 200,
                     impute = FALSE, normalize = FALSE,
                     store.item = TRUE, file = NULL, verbose = FALSE)

pol_rc1 <- find_pol_rc_horizontal(dynamic_rc$rollcall, party_code_col = "group",
                                  liberal_code = "Democrat", conservative_code = "Republican",
                                  na_threshold = 0.5)
pol_rc2 <- find_pol_rc_vertical(ideal, dynamic_rc$rollcall, pol_rc1,
                                party_code_col = "group",
                                na_threshold = 0.5, lop_threshold = 0.1)
const_ls <- find_constraints(ideal, dynamic_rc$rollcall, pol_rc1 = pol_rc1, pol_rc2 = pol_rc2,
                             party_code_col = "group", left_party_code = "Democrat",
                             as_list = TRUE)
print(const_ls)

## suppress redundant messages
invisible(capture.output({
  ideal_pp <- pscl::postProcess(ideal, constraints = const_ls)
}))

plot_ideal(ideal_point_1d = ideal_pp$xbar[,1], ideal_point_2d = ideal_pp$xbar[,2],
           group = dynamic_rc$rollcall$legis.data$group, p.title = irt.title,
           breaks.group = breaks.group, values.shape = values.shape, values.color = values.color)

dynamic_stan_input <- make_dynamic_stan_input(dynamic_rollcall = dynamic_rc,
                                              ideal = ideal_pp,
                                              a = 0.01,
                                              b = 0.001,
                                              rho_init = 10,
                                              sd_dynamic = 0.01)

fit_dynamic_updated <- issueirt_dynamic_stan(
  data = dynamic_stan_input$data,
  init = list(dynamic_stan_input$init, dynamic_stan_input$init), # starting values
  chains = 2,             # number of Markov chains
  warmup = 10,            # number of warmup iterations per chain
  iter = 20,              # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
)

const_dynamic <- get_dynamic_constraints(fit_dynamic_updated, dynamic_stan_input, anchors)
posterior_summary_pp <- make_posterior_summary_postprocessed(
  stan_fit = fit_dynamic_updated,
  constraints = const_dynamic,
  issue_label = dynamic_stan_input$misc$issue_code_df$codebook$label,
  rc_label = dynamic_stan_input$misc$bills_df$term_rollnumber,
  legis_label = dynamic_stan_input$misc$legis_term,
  missing_label = NULL
)

posterior_df_pp <- tibble(legis_term = dynamic_stan_input$misc$legis_term,
                          ideal_point_1d = posterior_summary_pp$x_postprocessed |> filter(dimension == 1) |> pull(mean),
                          ideal_point_2d = posterior_summary_pp$x_postprocessed |> filter(dimension == 2) |> pull(mean),
                          ideal_point_1d_se = posterior_summary_pp$x_postprocessed |> filter(dimension == 1) |> pull(sd),
                          ideal_point_2d_se = posterior_summary_pp$x_postprocessed |> filter(dimension == 2) |> pull(sd)) %>%
  separate(legis_term, into = c("icpsr", "term"), sep = "_") %>%
  mutate(congress = c(52:54)[as.numeric(term)],
         icpsr = as.numeric(icpsr)) %>%
  left_join(dynamic_stan_input$misc$legis_df, by = c("icpsr", "congress"))

plot_ideal(ideal_point_1d = posterior_df_pp$ideal_point_1d,
           ideal_point_2d = posterior_df_pp$ideal_point_2d,
           group = posterior_df_pp$group, p.title = issueirt.title,
           breaks.group = breaks.group, values.shape = values.shape, values.color = values.color)

p_ls <- plot_issueaxis(stan_input = dynamic_stan_input,
                       posterior_summary = posterior_summary_pp,
                       group = posterior_df_pp$group, p.title = "IssueIRT",
                       breaks.group = breaks.group, values.shape = values.shape, values.color = rep("grey", length(breaks.group)))
library(patchwork)
p_ls[["H52_Monetary"]] + p_ls[["H53_Monetary"]] + p_ls[["H54_Monetary"]] +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position='bottom')

## save
usethis::use_data(fit_dynamic, fit_sim,
                  fit_dynamic_updated,
                  dynamic_stan_input, const_dynamic,
                  posterior_summary_pp, posterior_df_pp,
                  overwrite = TRUE, internal = TRUE)
