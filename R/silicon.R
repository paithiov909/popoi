#' Check if the silicon is available
#'
#' @return logical.
#'
#' @importFrom rlang is_empty
is_silicon_available <- function() {
  return(!rlang::is_empty(Sys.which("silicon")))
}

#' Execute Silicon Command
#'
#' @param input_file path to the source code. If provided `NULL`, render from clipboard.
#' @param flags list of flag sets.
#' @param language default value is "R".
#' @param output_path where the output is created. If `flags$to_clipboard` is TRUE, this is ignored.
#' @param ... other arguments are passed to silicon's options as `args` of \code{processx::run("silicon")}.
#'
#' @return result from \code{processx::run("silicon")} invisibly.
#'
#' @importFrom rlang abort
#' @importFrom processx run
#' @importFrom dplyr if_else
#' @importFrom purrr discard
#' @export
silicon <- function(input_file = NULL,
                    flags = list(
                      no_line_number = FALSE,
                      no_round_corner = FALSE,
                      no_window_controls = FALSE,
                      to_clipboard = TRUE
                    ),
                    language = "R",
                    output_path = getwd(),
                    ...) {
  if (!is_silicon_available()) rlang::abort("This function requires Aloxaf/silicon available.")
  args <- c(
    ## Flags
    dplyr::if_else(is.null(input_file), "--from-clipboard", NA_character_),
    dplyr::if_else(isTRUE(flags$to_clipboard), "--to-clipboard", NA_character_),
    dplyr::if_else(isTRUE(flags$no_line_number), "--no-line-number", NA_character_),
    dplyr::if_else(isTRUE(flags$no_round_corner), "--no-round-corner", NA_character_),
    dplyr::if_else(isTRUE(flags$no_window_cotrols), "--no-window-controls", NA_character_),
    ## Options
    c("--language", language),
    ...,
    ## Output and input
    dplyr::if_else(isFALSE(flags$to_clipboard), "--output", NA_character_),
    dplyr::if_else(isFALSE(flags$to_clipboard), output_path, NA_character_),
    dplyr::if_else(is.null(input_file), NA_character_, input_file)
  )
  result <- processx::run(
    "silicon",
    args = purrr::discard(args, ~ is.na(.)),
    stderr = "",
    stderr_callback = rlang::abort,
    encoding = "UTF-8"
  )
  return(invisible(result))
}
