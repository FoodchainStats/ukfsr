# id <- "1.2.5a"
# theme <- 1
# section <- 2
# indicator <- 3
# variant <- "b"
# description <- "Import intensity, UK 2024-25"


# Indicator id is Theme(1-5).Section(1-9).Indicator(1-99)Variant(a-z)
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
    stop(paste(id, "is not an indicator id of the form T.S.Iv where T = Theme, S = Section, I = Indicator, v = optional variant (a-z)"))
  }
  id<- stringr::str_split_1(id, pattern = "\\.")
  id <- stringr::str_flatten(id, collapse = "_")
  filename <- paste(id, desc, sep = "_")
  return(filename)
}


save_graphic <- function(graphic, indicator_id, indicator_desc = "", path = getwd()) {
  gtype <- class(graphic) %in% c("gg", "ggplot") 
  if(!all(gtype)) {
    stop("object must be a ggplot graphic")
  }
  fname <- make_filename(indicator_id, indicator_desc)
  ggplot2::ggsave(filename = paste0(fname, ".png"), path = path, plot = graphic, device = "png", width = 960/72, height = 640/72, dpi = 72)
  ggplot2::ggsave(filename = paste0(fname, ".svg"), path = path, plot = graphic, device = "svg", width = 960/72, height = 640/72, dpi = 72)
  # return(fname)
  rlang::inform(c("Saving file", paste0(path, "/", fname, ".png"), paste0(path, "/", fname, ".svg")))
}
