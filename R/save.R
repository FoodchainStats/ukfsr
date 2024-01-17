# id <- "1.2.5a"
# theme <- 1
# section <- 2
# indicator <- 3
# variant <- "b"
# description <- "Import intensity, UK 2024-25"

clean_string <- function(string) {
  string <- stringr::str_to_lower(string)
  string <- stringr::str_remove_all(string, "[:punct:]")
  string <- stringr::str_replace_all(string, " ", "_")
  return(string)
}


make_filename <- function(indicator_id, description) {
  description <- clean_string(description)
  id<- stringr::str_split_1(id, pattern = "\\.")
  id <- stringr::str_flatten(id, collapse = "_")
  filename <- paste(id, description, sep = "_")
  return(filename)
}


save_graphic <- function(graphic, indicator_id, indicator_desc) {
  z <- class(c) %in% c("gg", "ggplot") 
  
  all(z)
  !all(z)
  
}
