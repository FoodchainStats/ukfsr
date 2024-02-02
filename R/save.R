#' Test whether a string is a valid UKFSR indicator id
#'
#' @description
#' A UKFSR indicator can be uniquely identified from 3 main hierarchical components separated by periods:
#' 
#' * theme (1-5)
#' * section (1-9)
#' * indicator (1-99)
#' 
#' with an optional variant suffix in the form (a -z). Thus a valid indicator id
#' is a string in the form "T.S.Iv". Some examples are "1.3.7a". "4.2.11".
#' check_indicator tests an indicator id string for compliance and returns TRUE
#' or FALSE.
#' 
#' @param string String containing a UKFSR indicator id
#'
#' @return A logical
#' @seealso [parse_indicator()] extracts the components of an indicator id
#' @export
#'
#' @examples
#' # A valid id
#' check_indicator("2.3.5a")
#' 
#' # An invalid id
#' check_indicator("10.3.1")
check_indicator <- function(string) {
  # check that the first chars fit the pattern
  check <- stringr::str_starts(string, pattern = "[1-5]\\.[1-9]\\.[1-9][0-9]?[a-z]{0,1}")
  # fail if there are extra chars at the end
  if(check) {
    if(nchar(stringr::str_extract(string, pattern = "[1-5]\\.[1-9]\\.[1-9][0-9]?[a-z]{0,1}")) == nchar(string)){
      check = TRUE
    } else{
      check = FALSE
    }
      
    
  }
  return(check)
}


# Remove punctuation and make camel case
clean_string <- function(string) {
  string <- stringr::str_to_lower(string)
  string <- stringr::str_remove_all(string, "[:punct:]")
  string <- stringr::str_replace_all(string, " ", "_")
  return(string)
}


make_filename <- function(id, desc) {
  desc <- clean_string(desc)
  check <- check_indicator(id)
  if(check == FALSE) {
    rlang::abort(paste(id, "is not a valid indicator id. See`check_indicator`."))
  }
  id<- stringr::str_split_1(id, pattern = "\\.")
  id <- stringr::str_flatten(id, collapse = "_")
  filename <- paste(id, desc, sep = "_")
  return(filename)
}


#' Parse a UKFSR indicator id into its components
#'
#' @param indicator_id A valid UKFSR indicator id
#'
#' @return A named list of the indicator id components
#' @seealso [check_indicator()] tests whether an indicator id is valid
#' @export
#'
#' @examples
#' i <- parse_indicator("3.2.1b")
#' i$theme
#' i$section
#' i$indicator
#' i$variant
parse_indicator <- function(indicator_id) {
  
  check <- check_indicator(indicator_id)
  if(check == FALSE) {
    rlang::abort(paste(indicator_id, "is not a valid indicator id. See`check_indicator`."))
  }
  
  t <- stringr::str_sub(indicator_id, 1,1)
  s <- stringr::str_sub(indicator_id,3,3)
  i <- stringr::str_extract(stringr::str_sub(indicator_id, 5, stringr::str_length(indicator_id)), "[0-9]*")
  v <- stringr::str_extract(stringr::str_sub(indicator_id, -1), "[a-z]")
  
  indicator <- list(theme = t,
                    section = s, 
                    indicator = i,
                    variant = v)
  return(indicator)
  
}


#' Save a UKFSR graphic to the S3 bucket
#'
#' Graphics are saved into a folder structure on the bucket as specified in the
#' UKFSR guidance. This is currently 'theme_x/tT_S_I/output/graphics'. Filenames
#' are derived from the indicator id and description. The bucket location is
#' encoded in [s3_bucket()].
#'
#' @param graphic A `ggplot` chart object
#' @param indicator_id A valid UKFSR indicator id
#' @param indicator_desc A title for the graphic
#'
#' @return Saves a png and svg file to the S3 bucket
#' @export
#'
#' @examples
#' \dontrun{
#' chart <- ggplot2::ggplot(mtcars) + 
#' ggplot2::geom_point(ggplot2::aes(x = mpg, y = wt))
#' 
#' save_graphic(chart, "1.1.1", "Test graphic")
#' }
save_graphic <- function(graphic, indicator_id, indicator_desc = "") {
  gtype <- class(graphic) %in% c("gg", "ggplot") 
  if(!all(gtype)) {
    rlang::abort("object must be a ggplot graphic")
  }
  fname <- make_filename(indicator_id, indicator_desc)
  id <- parse_indicator(indicator_id)
  s3path <- paste0("theme_", id$theme, "/t", id$theme, "_", id$section, "_", id$indicator, "/output/graphics/", fname)
  tmp <- tempfile()
  
  png <- suppressMessages(aws.s3::object_exists(paste0(s3path, ".png"), s3_bucket()))
  svg <- suppressMessages(aws.s3::object_exists(paste0(s3path, ".svg"), s3_bucket()))
  
  if(png|svg) {
    query <- readline(prompt = "A file with this name already exists. Overwrite? (Y/N)")
    if(tolower(query) != "y" ) {
      rlang::abort("Graphics not saved")
    }
  }
  
  ggplot2::ggsave(tmp, plot = graphic, device = "png", width = 960/72, height = 640/72, dpi = 72)
  aws.s3::put_object(tmp,
                     object = paste0(s3path, ".png"),
                     bucket = s3_bucket(),
                     headers = c("x-amz-acl" = "bucket-owner-full-control"))
  
  ggplot2::ggsave(tmp, plot = graphic, device = "svg", width = 960/72, height = 640/72, dpi = 72)
  aws.s3::put_object(tmp,
                     object = paste0(s3path, ".svg"),
                     bucket = s3_bucket(),
                     headers = c("x-amz-acl" = "bucket-owner-full-control"))
  
    # return(fname)
  rlang::inform(c("Saving file",
                  paste0(s3_bucket(), "/", s3path, ".png"), 
                  paste0(s3_bucket(), "/", s3path, ".png")))
}
