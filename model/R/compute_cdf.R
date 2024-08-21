#' @title compute_cdf
#'
#' @description Computes the CDF based on a queueingAnalyzeR PDF structure
#'
#' @param dist The input distribution in PDF format
#'
#' @return The CDF of the input PDF
#'
#' @examples
#' dist <- generate_pdf(params = list(dist = "pois", lambda = 10))
#' compute_cdf(dist)
#'
#' @export
#'

compute_cdf <- function(dist) {
  dist$prob <- cumsum(dist$prob)
  return(dist)
}
