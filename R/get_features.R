#' Generate string distance metrics for all combinations of two vectors
#'
#' @param string1 A character vector
#' @param string2 A character vector
#' @return A data frame of rows length(string1) * length(string2) with columns for each string distance metric
#' @examples
#' get_features(string1 = c("hello", "goodbye"),
#'              string2 = c("alpha", "beta"))


get_features <-
function(string1, string2){
  tmp = expand_grid(string1, string2)
  tmp$osa = stringdist::stringdist(string1, string2, method = "osa")
  tmp$cosine = stringdist::stringdist(string1, string2, method = "cosine")
  tmp$jaccard = stringdist::stringdist(string1, string2, method = "jaccard")
  tmp$jw = stringdist::stringdist(string1, string2, method = "jw")
  tmp$lcs = stringdist::stringdist(string1, string2, method = "lcs")
  tmp$qgram = stringdist::stringdist(string1, string2, method = "qgram")
  tmp$soundex = stringdist::stringdist(string1, string2, method = "soundex")
  return(tmp)  
}
