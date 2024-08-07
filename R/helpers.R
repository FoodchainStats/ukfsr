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




#' Get contents of the UKFSR S3 bucket 
#'
#' @param bucket An S3 bucket
#' @param file_ext A file extension, or NULL to return the entire bucket
#'
#' @return A dataframe of the contents of the bucket, with indicator ids and
#'   descriptions parsed from the filenames
#' @export
#'
#' @examples
#' \dontrun{
#' x <- bucket_manifest(file_ext = "csv")
#' }
bucket_manifest <- function(bucket = ukfsr::s3_bucket(), file_ext = "png") {
  
  if(!aws.s3::bucket_exists(bucket)){
    rlang::abort(paste(bucket, "is not a valid AWS S3 bucket."))
  }
  
  not_null <- Negate(is.null)
  
  if(not_null(file_ext)) {
    if(is.na(file_ext) | file_ext == "" | !is.character(file_ext)) {
      rlang::abort("file_ext must be a string or NULL.")
    }
  }
  
  manifest <- aws.s3::get_bucket_df(bucket)
  
  files <- manifest |> 
    dplyr::select("Key") |>
    dplyr::mutate(folder = dirname(.data$Key),
                  file = basename(.data$Key),
                  indicator_id = stringr::str_extract(file, "(fsi|[1-5])_[1-9]_[1-9][0-9]?[a-z]{0,1}") |> 
                    (\(z)(stringr::str_replace_all(z,"_", ".")))(),
                  title = stringr::str_remove(file, "(fsi|[1-5])_[1-9]_[1-9][0-9]?[a-z]{0,1}_") |> 
                    (\(z)(stringr::str_replace_all(z, pattern = "_", replacement = " ")))() |> 
                    (\(z)(stringr::str_replace_all(z, pattern = "(\\.svg|\\.png|\\.csv|\\.ods|\\.xlsx|\\.json|\\.geojson)", replacement = " ")))() |> 
                    (\(z)(stringr::str_to_sentence(z)))()
    ) |> 
    dplyr::select("indicator_id", "title", "file", "folder", "path" = "Key")
  
  if(is.null(file_ext)) {
    return(files)
  } else {
    
    files <- files |> 
    dplyr::filter(stringr::str_ends(.data$path, file_ext))
    
    return(files)  
  }
  
  
}
