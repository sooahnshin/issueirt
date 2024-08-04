#' Roll Call Votes in U.S. House of Representatives, 1890s (52nd to 54th)
#'
#' This dataset contains roll call votes in the U.S. House of Representatives from the 52nd to the 54th Congress.
#' Each element in the list is a data frame where rows correspond to representatives (identified by ICPSR) and columns correspond to votes (identified by roll number).
#'
#' @name us1890s_votes
#' @usage data(us1890s_votes)
#' @docType data
#' @format A list of three \code{data.frame}s, one for each Congress (52nd to 54th).
#' Each data frame has rows representing representatives (ICPSR) and columns representing votes (roll number).
#' @seealso \code{\link{us1890s_legis}}, \code{\link{us1890s_bills}}, \code{\link{us1890s_issue}}
#' @references Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet. (2024)
#'  \emph{Voteview: Congressional Roll-Call Votes Database.}
#'  \url{https://voteview.com/}
#' @keywords dataset
"us1890s_votes"

#' Legislators in U.S. House of Representatives, 1890s (52nd to 54th)
#'
#' This dataset contains information on legislators in the U.S. House of Representatives from the 52nd to the 54th Congress.
#'
#' @name us1890s_legis
#' @usage data(us1890s_legis)
#' @docType data
#' @format A list of three \code{data.frame}s, one for each Congress (52nd to 54th).
#'
#' @seealso \code{\link{us1890s_votes}}, \code{\link{us1890s_bills}}, \code{\link{us1890s_issue}}
#' @references Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet. (2024)
#' \emph{Voteview: Congressional Roll-Call Votes Database.}
#' \url{https://voteview.com/}
#' @keywords dataset
"us1890s_legis"

#' Roll-Calls in U.S. House of Representatives, 1890s (52nd to 54th)
#'
#' This dataset contains information on roll-call votes in the U.S. House of Representatives from the 52nd to the 54th Congress.
#'
#' @name us1890s_bills
#' @usage data(us1890s_bills)
#' @docType data
#' @format A list of three \code{data.frame}s, one for each Congress (52nd to 54th).
#'
#' @seealso \code{\link{us1890s_votes}}, \code{\link{us1890s_legis}}, \code{\link{us1890s_issue}}
#' @references Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet. (2024)
#' \emph{Voteview: Congressional Roll-Call Votes Database.}
#' \url{https://voteview.com/}
#' @keywords dataset
"us1890s_bills"

#' Issue Codes in U.S. House of Representatives, 1890s (52nd to 54th)
#'
#' This dataset contains information on issue codes in the U.S. House of Representatives from the 52nd to the 54th Congress.
#'
#' @name us1890s_issue
#' @usage data(us1890s_issue)
#' @docType data
#' @format A list of three \code{data.frame}s, one for each Congress (52nd to 54th).
#'
#' @seealso \code{\link{us1890s_votes}}, \code{\link{us1890s_legis}}, \code{\link{us1890s_bills}}
#' @references Bateman, David A, Ira Katznelson and John S Lapinski. (2022)
#' "Issue codings for roll call in the U.S. House of Representatives and Senate between 1877 and 2011"
#' \emph{American Institutions Project.}
#' \url{http://www.davidalexbateman.net/congressional-data.html.}
#' @keywords dataset
"us1890s_issue"
