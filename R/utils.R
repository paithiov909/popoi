#' An R implementation of NanoID
#' @export
nano <<- function(size = 21L) {
  url <- c(LETTERS, letters, as.character(0:9), "_", "-")
  bytes <- bitwAnd(as.integer(openssl::rand_bytes(size)), 63)
  id <- url[bytes + 1]
  return(paste0(id, collapse = ""))
}