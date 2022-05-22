#' Environment for internal use
#'
#' @noRd
#' @keywords internal
.env <- rlang::env(server = NULL)

#' @noRd
reset_encoding <- function(chr, encoding = "UTF-8") {
  Encoding(chr) <- encoding
  return(chr)
}

#' Call lattice command
#'
#' A debug tool of tokenize process outputs a lattice in graphviz dot format.
#'
#' @param sentences Character vector.
#' @param ... Other arguments are passed to \code{DiagrammeR::grViz}.
#'
#' @export
get_lattice <- function(sentences, ...) {
  senteces <- stringi::stri_omit_na(sentences)
  dot <- processx::run("kagome", c("lattice", stringi::stri_c(sentences, collapse = " ")))$stdout
  DiagrammeR::grViz(reset_encoding(dot), ...)
}

#' Lanch Kagome Server
#'
#' Start or kill Kagome server process.
#' @param dict Dictionary which kagome server uses. Default value is 'ipa' (IPA-dictionary).
#' @return Logical value (whether or not the kagome server is alive?) is returned invisibly.
#'
#' @export
lanch_server <- function(dict = c("ipa", "uni")) {
  p <- rlang::env_get(.env, "server")
  if (is.null(p)) {
    dict <- rlang::arg_match(dict)
    p <- processx::process$new("kagome", c("server", "-dict", dict))

    rlang::env_bind(.env, server = p)
  } else {
    if (p$is_alive()) {
      kill <- yesno::yesno("Kagome server is already alive. Do you want to kill its process?")
    }
    if (kill) {
      p$kill()
      rlang::env_bind(.env, server = NULL)
    }
  }
  return(invisible(p$is_alive()))
}

#' Send a HEAD request to Kagome server
#'
#' @param url URL Character scalar.
#' @return httr2 response is returned invisibly.
#'
#' @export
ping <- function(url = "http://localhost:6060") {
  resp <-
    httr2::request(url) |>
    httr2::req_method("HEAD") |>
    httr2::req_perform()
  return(invisible(resp))
}

#' Put request to tokenize API
#'
#' @param sentences Character vector to be analyzed.
#' @param url URL of Kagome server.
#' @param mode One of `normal`, `search` or `extended`.
#' @return tibble
#'
#' @export
tokenize <- function(sentences,
                     url = "http://localhost:6060/tokenize",
                     mode = c("normal", "search", "extended")) {
  mode <- rlang::arg_match(mode)

  sentences <-
    stringi::stri_omit_na(sentences) |>
    stringi::stri_split_boundaries(type = "sentence") |>
    purrr::flatten_chr()

  resps <-
    furrr::future_imap_dfr(sentences, ~ tokenize_impl(.x, .y, url, mode)) |>
    dplyr::mutate(across(where(is.character), ~ dplyr::na_if(., "*"))) |>
    dplyr::mutate_at(c("doc_id", "class"), as.factor) |>
    tibble::as_tibble()

  return(resps)
}

#' @noRd
tokenize_impl <- function(msg, idx, url = "http://localhost:6060/tokenize", mode = "normal") {
  resp <-
    httr2::request(url) |>
    httr2::req_body_json(list(
      sentence = msg,
      mode = mode
    )) |>
    httr2::req_method("PUT") |>
    httr2::req_error(function(resp) httr2::resp_status(resp) > 400) |>
    httr2::req_perform()

  return(
    purrr::map_dfr(
      httr2::resp_body_json(resp)$tokens,
      ~ data.frame(
        doc_id = idx,
        id = .$id,
        start = .$start,
        end = .$end,
        class = .$class,
        token = .$surface,
        POS1 = .$pos[[1]],
        POS2 = .$pos[[2]],
        POS3 = .$pos[[3]],
        POS4 = .$pos[[4]],
        Original = .$base_form,
        Yomi1 = .$reading,
        Yomi2 = .$pronunciation
      )
    )
  )
}

.on_unload <- function(ns) {
  p <- rlang::env_get(.env, "server")
  if (!is.null(p) && p$is_alive()) {
    p$kill()
  }
}
