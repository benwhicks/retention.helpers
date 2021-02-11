#' chi and odds
#'
#' Takes in a a n x 3 data frame and calculates
#' the chi squared statistic, the p-value of this
#' and the odds ratio.
#'
#' First column must be the category names.
#' Second column must be observed values (a count)
#' Third column must be the expected values (also a count, but can be weights or probabilities)
#'
#' @param m A n x 3 data frame or tibble. Should have variables in a particular order: category_name, observed_count, expected_count
#'
#' @examples
#' df <- tibble::tibble(field = c('Yes', 'No') , observed = c(34, 88), expected = c(707, 1021))
#' chi_and_odds(df)
#'
#' # or from clipboard
#' df <- clipr::read_clip_tbl()
#' chi_and_odds(df)
#'
#' @export chi_and_odds
chi_and_odds <- function(m) {
  if (ncol(m) != 3) {
    stop("Expected 3 column data frame with variables category_name, observed_count, expected_count")
  }
  field_name <- names(m)[[1]]
  odds_field_result <- dplyr::pull(m, 1)[[1]]

  data_name <- names(m)[[2]]
  expected_name <- names(m)[[3]]

  observed <- dplyr::pull(m, 2)
  expected <- dplyr::pull(m, 3)

  if (min(observed) <= 5) {
    #res <- fisher.test()
    res <- "Not valid with results 5 or under"
  } else {
    res <- chisq.test(x = observed, p = expected,
                      rescale.p = TRUE, correct = FALSE)
  }
  if (nrow(m) == 2) {
    odds_ratio <- (m[1,2] / m[1, 3]) / (m[2, 2] / m[2,3])
    if (odds_ratio == 1) {
      odds_interpretation <- paste0(odds_field_result, " is equally likely to happen in ",
                                    data_name, " as in ", expected_name)
    } else if (odds_ratio > 1) {
      odds_interpretation <- paste0(odds_field_result, " is ", odds_ratio, " times more likely to happen in ",
                                    data_name, " than in ", expected_name)
    } else {
      odds_interpretation <- paste0(odds_field_result, " is ", 1/odds_ratio, " times less likely to happen in ",
                                    data_name, " than in ", expected_name)
    }
  } else {
    cat("#--------------------------------------------------------------------------------#\n",
        paste0("Odds ratio only available for 2 x 2 matrix, dichotomise ", field_name, " to find odds ratio."),
        "\n#--------------------------------------------------------------------------------#\n\n")
    odds_ratio = NA_character_
    odds_interpretation = NA_character_
  }
  print(m)
  cat("\n")
  structure(list(
    chisq.test.result = res,
    odds_ratio = odds_ratio,
    odds_interpretation = odds_interpretation
  ))
}
