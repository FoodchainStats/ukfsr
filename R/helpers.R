#' Location of the UKFSR2024 Amazon S3 bucket 
#' 
#' The bucket is used to store final publication data and graphics, as well as
#' underlying raw data.
#'
#' @return A string containing the bucket location
#' @export
#'
#' @examples
#' s3_bucket()
s3_bucket <- function() {
  bucket <- "s3-ranch-054"
  return(bucket)
}