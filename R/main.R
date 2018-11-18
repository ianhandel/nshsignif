#' Report significance
#'
#' This function processes p-value text to report significance against a critical p-value
#'
#'
#' @param pvals Character vector of p-values to parse.
#' @param alpha Critical test size for deciding on significance.
#' @return Character vector of "S" (for significant), "NS" (not significant)
#' and "not-tested"
#'
#' @export
parse_pvalue <- function(pvals, alpha = 0.05) {
  pvals <- toupper(pvals)
  pvals <- stringr::str_remove_all(pvals, "( +|P *|=)")
  pvals <- dplyr::if_else(stringr::str_detect(pvals, "TO"),
                          purrr::map_chr(pvals, ~stringr::str_split(.x, "TO", simplify = TRUE)[[1]]),
                          pvals
  )
  pval_num <- suppressWarnings(readr::parse_double(stringr::str_remove_all(pvals, "(<|>)")))

  dplyr::case_when(stringr::str_detect(pvals, "<") & pval_num <= alpha  ~ "S",
                   !stringr::str_detect(pvals, ">") & pval_num < alpha  ~ "S",
                   pval_num >= alpha ~ "NS",
                   is.na(pval_num) ~ "not-tested")
}


#' Make study name
#'
#' Given bridge ID, author and year amek a unique study name. If author and year
#' exist use that adding a number to make unique (if duplicate author_year for)
#' a bridge ID. If author and year absent use bridge ID
#'
#' @param bridge Character vector of bridge id's.
#' @param author Character vector of authors.
#' @param year Character vector of years.
#' @return Character vector of unique study id's.
#'
#' @export
make_studyid <- function(bridge, author, year){

  # CRAN global variable fix
  author_year <- study <- NULL

  bridge <- as.character(bridge)
  df <- dplyr::data_frame(bridge, author, year)
  df <- dplyr::distinct(df)
  df <- dplyr::mutate(df, author_year = dplyr::if_else(!is.na(author) & !is.na(year),
                                                paste0(author, "_", year),
                                                NA_character_))
  df <- dplyr::group_by(df, author_year)
  df <- dplyr::mutate(df, study = make_unique(author_year))
  df <- dplyr::mutate(df, study = dplyr::coalesce(study, bridge))
  df$study[match(bridge, df$bridge)]
}
