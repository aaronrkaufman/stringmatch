#' Generate string distance metrics for all combinations of two vectors
#'
#' @param string1 A character vector
#' @param string2 A character vector
#' @return A data frame of rows length(string1) * length(string2) with columns for each string distance metric
#' @examples
#' get_features(string1 = c("hello", "goodbye"),
#'              string2 = c("alpha", "beta"))


get_features <-
  function(string1, string2 = NULL) {
    if (is.null(string2)) {
      tmp = string1 |>
        length() |>
        combn(2) |>
        split(1:2) |>
        lapply(\(x) string1[x]) |>
        as.data.frame() |>
        setNames(c("Var1", "Var2"))
    } else {
      tmp = expand.grid(string1, string2)
    }
    tmp$osa = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "osa")
    tmp$cosine = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "cosine")
    tmp$jaccard = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "jaccard")
    tmp$jw = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "jw")
    tmp$lcs = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "lcs")
    tmp$qgram = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "qgram")
    tmp$soundex = stringdist::stringdist(tmp[, 1], tmp[, 2], method = "soundex")
    return(tmp)
  }
