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

#' @aliases nano
ipsum <- function(size = 6L, alpha = hana) {
  nano(size, alpha)
}

#' Mask over utils::install.packages
#'
#' @param ... Passed to \code{utils::install.packages}.
#' @param .opts Install options.
#'
#' @export
install_packages <- function(..., .opts = "--no-multiarch") {
  utils::install.packages(..., quiet = TRUE, INSTALL_opts = .opts)
}

#' Switch pkgType to 'binary'
#'
#' Switch pkgType option to `binary` if possible.
#'
#' @param pkg_type Choose one of 'binary' or 'source'
#' @return The settled pktType option is returned invisibly.
#'
#' @export
switch_pkgtype <- function(pkg_type = c("binary", "source")) {
  pkg_type <- match.arg(pkg_type)
  if (!identical(pkg_type, "source") && identical(.Platform$OS.type, "windows")) {
    options("pkgType" = "binary")
  } else {
    options("pkgType" = "source")
  }
  return(invisible(getOption("pkgType")))
}

#' Switch locale betwenn English and Japanese
#'
#' @param lang Choose one of 'en' or 'ja'
#' @return The settled locale setting is returned invisibly.
#'
#' @export
switch_locale <- function(lang = c("en", "ja")) {
  lang <- rlang::arg_match(lang)
  if (yesno::yesno("Are you sure to switch locale?")) {
    Sys.setlocale(
      "LC_ALL",
      ifelse(lang == "en",
        "english",
        "japanese"
      )
    )
    rlang::inform("Now the locale has been switched!!")
  }
  return(invisible(Sys.getlocale()))
}
