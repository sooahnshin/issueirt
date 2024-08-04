## The Rise of Populism and Sectional Division in the 1890s (The 52nd to 54th US House of Representatives)
## See the article listed in citation("issueirt") for more details

library(tidyverse)
library(readxl)
setwd("data-raw")

######################
## RCV
## data source:
# Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet (2024).
# Voteview: Congressional Roll-Call Votes Database. https://voteview.com/
######################

## load data
Hvotes_list <- list.files(path = "us_southern", pattern="H.*_votes.csv", full.names = T) %>%
  lapply(., read_csv, show_col_types = FALSE)
Hmembers_list <- list.files(path = "us_southern", pattern="H.*_members.csv", full.names = T) %>%
  lapply(., read_csv, show_col_types = FALSE)
Hparties_list <- list.files(path = "us_southern", pattern="H.*_parties.csv", full.names = T) %>%
  lapply(., read_csv, show_col_types = FALSE)
Hrollcalls_list <- list.files(path = "us_southern", pattern="H.*_rollcalls.csv", full.names = T) %>%
  lapply(., read_csv, show_col_types = FALSE)

us1890s_votes <- us1890s_legis <- us1890s_bills <- list()
## preprocess data
for(i in 1:length(Hvotes_list)) {
  # 1) filter out president
  # 2) change coding scheme to 0, 1, 2, 3
  # 3) make wide format
  president_icpsr <- Hmembers_list[[i]] %>%
    filter(chamber == "President") %>%
    pull(icpsr)
  H_votes <- Hvotes_list[[i]] %>%
    filter(chamber == "House") %>%
    filter(!icpsr %in% president_icpsr) %>%
    dplyr::select(icpsr, rollnumber, cast_code) %>%
    mutate(cast_code = case_when(
      as.numeric(cast_code) %in% c(1, 2, 3) ~ 1, # yea
      as.numeric(cast_code) %in% c(4, 5, 6) ~ 0, # nay
      as.numeric(cast_code) %in% c(7, 8, 9) ~ 2, # abstention
      as.numeric(cast_code) == 0 ~ 3 # not a member
    )) %>%
    pivot_wider(names_from = "rollnumber", values_from = "cast_code") %>%
    as.data.frame() %>%
    arrange(icpsr)
  icpsr_vec <- H_votes$icpsr
  H_votes <- H_votes %>% select(-icpsr)
  rownames(H_votes) <- icpsr_vec
  rollnumber_vec <- as.numeric(colnames(H_votes))
  H_legis <- Hmembers_list[[i]] %>%
    filter(icpsr %in% icpsr_vec) %>%
    arrange(match(icpsr, icpsr_vec)) %>%
    left_join(Hparties_list[[i]] %>% select(party_code, party_name) %>% distinct(),
              by = "party_code")
  H_bills <- Hrollcalls_list[[i]] %>%
    filter(rollnumber %in% rollnumber_vec) %>%
    arrange(match(rollnumber, rollnumber_vec))

  us1890s_votes[[i]] <- H_votes
  us1890s_legis[[i]] <- H_legis
  us1890s_bills[[i]] <- H_bills
}

names(us1890s_votes) <- names(us1890s_legis) <- names(us1890s_bills) <- c("H52", "H53", "H54")

## save
usethis::use_data(us1890s_votes, overwrite = TRUE)
usethis::use_data(us1890s_legis, overwrite = TRUE)
usethis::use_data(us1890s_bills, overwrite = TRUE)

######################
## Issue Label
## data source:
# Bateman, David A, Ira Katznelson and John S Lapinski. 2022. Issue codings for roll call in the
# U.S. House of Representatives and Senate between 1877 and 2011.
# American Institutions Project. http://www.davidalexbateman.net/congressional-data.html.
######################

issue_all <- read_excel("us_southern/house.rollcall.reduced.xlsx")
coding_scheme <- read_excel("us_southern/coding_scheme.xlsx")

us1890s_issue <- list()
for(i in 1:length(Hvotes_list)) {
  bills <- us1890s_bills[[i]]
  house_number <- unique(bills$congress)
  issue <- issue_all |>
    filter(Congress == house_number) |>
    mutate(rollnumber = Rollcall) |>
    select(rollnumber, RC_TIER1, RC_TIER2, RC_TIER3) |>
    filter(rollnumber %in% bills$rollnumber) |>
    arrange(match(rollnumber, bills$rollnumber)) |>
    mutate(issue_code = paste("T3", RC_TIER3, sep = "_")) |>
    mutate(issue_code = case_when(
      RC_TIER1 == 5 ~ "T1_5",
      RC_TIER1 == 6 ~ "T1_6",
      RC_TIER1 == 7 ~ "T1_7",
      RC_TIER1 == 8 ~ "T1_8",
      TRUE ~ issue_code
    ))
  issue_label_df <- coding_scheme |>
    select(RC_TIER1, Tier1_label, RC_TIER3, Tier3_label) |>
    mutate(issue_label = if_else(RC_TIER1 %in% c(5, 6, 7, 8), Tier1_label, Tier3_label)) |>
    mutate(issue_code = paste("T3", RC_TIER3, sep = "_")) |>
    mutate(issue_code = case_when(
      RC_TIER1 == 5 ~ "T1_5",
      RC_TIER1 == 6 ~ "T1_6",
      RC_TIER1 == 7 ~ "T1_7",
      RC_TIER1 == 8 ~ "T1_8",
      TRUE ~ issue_code
    )) |>
    select(issue_code, issue_label)
  issue_level_df <- issue |>
    arrange(RC_TIER3, RC_TIER1) |>
    select(issue_code) |>
    distinct() |>
    mutate(issue_level = 1:n())
  issue <- issue |>
    left_join(issue_label_df, by = "issue_code") |>
    left_join(issue_level_df, by = "issue_code")
  us1890s_issue[[i]] <- issue
}
names(us1890s_issue) <- c("H52", "H53", "H54")

## save
usethis::use_data(us1890s_issue, overwrite = TRUE)
