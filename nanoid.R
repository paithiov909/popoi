#' An R implementation of NanoID
#'
#' @param size Integer.
#' @return string scalar.
#'
#' @export
nano <- function(size = 21L, alpha = c(LETTERS, letters, as.character(0:9), "_", "-")) {
  bytes <- bitwAnd(as.integer(openssl::rand_bytes(size)), 63)
  id <- alpha[bytes + 1]
  return(paste0(id, collapse = ""))
}
