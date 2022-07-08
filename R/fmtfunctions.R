

#' @title Colour text
#'
#' @description A collection of functions that take text and return that same text flanked by  characters
#' that will lead to its coloration/formatting when printed to terminals using message/cat. Different presets are available:
#' utilitybeltfmt::fmterror, utilitybeltfmt::fmtwarning, utilitybeltfmt::fmtsuccess, utilitybeltfmt::fmtbold.
#'
#' If greater control is required, use the crayon package
#'
#' @param ... (string/s) Text to colorise. Comma separated strings will be concatenated (no spaces) before colorisation.
#'
#' @return (string) Input text flanked by relevant Ansi escape codes
#' @export
#'
#' @examples
#' message(utilitybeltfmt::fmterror("This is a warning"))
fmterror <- function(...) { .Deprecated(package = "utilitybeltfmt", new = "utilitybeltfmt::fmterror"); return(crayon::bold(crayon::red(paste0(...))))  }

#' @title Colour text
#'
#' @description A collection of functions that take text and return that same text flanked by  characters
#' that will lead to its coloration/formatting when printed to terminals using message/cat. Different presets are available:
#' utilitybeltfmt::fmterror, utilitybeltfmt::fmtwarning, utilitybeltfmt::fmtsuccess, utilitybeltfmt::fmtbold.
#'
#' If greater control is required, use the crayon package
#'
#' @param ... (string/s) Text to colorise. Comma separated strings will be concatenated (no spaces) before colorisation.
#'
#' @return (string) Input text flanked by relevant Ansi escape codes
#' @export
#'
fmtwarning <- function(...) { .Deprecated(package = "utilitybeltfmt", new = "utilitybeltfmt::fmtwarning"); return(crayon::bold(crayon::yellow(paste0(...))))  }

#' @title Colour text
#'
#' @description A collection of functions that take text and return that same text flanked by  characters
#' that will lead to its coloration/formatting when printed to terminals using message/cat. Different presets are available:
#' utilitybeltfmt::fmterror, utilitybeltfmt::fmtwarning, utilitybeltfmt::fmtsuccess, utilitybeltfmt::fmtbold.
#'
#' If greater control is required, use the crayon package
#'
#' @param ... (string/s) Text to colorise. Comma separated strings will be concatenated (no spaces) before colorisation.
#'
#' @return (string) Input text flanked by relevant Ansi escape codes
#' @export
#'
fmtsuccess <- function(...) { .Deprecated(package = "utilitybeltfmt", new = "utilitybeltfmt::fmtsuccess"); return(crayon::bold(crayon::green(paste0(...))))  }

#' @title Colour text
#'
#' @description A collection of functions that take text and return that same text flanked by  characters
#' that will lead to its coloration/formatting when printed to terminals using message/cat. Different presets are available:
#' utilitybeltfmt::fmterror, utilitybeltfmt::fmtwarning, utilitybeltfmt::fmtsuccess, utilitybeltfmt::fmtbold.
#'
#' If greater control is required, use the crayon package
#'
#' @param ... (string/s) Text to colorise. Comma separated strings will be concatenated (no spaces) before colorisation.
#'
#' @return (string) Input text flanked by relevant Ansi escape codes
#' @export
#'
fmtbold <- function(...) {
  .Deprecated(package = "utilitybeltfmt", new = "utilitybeltfmt::fmtbold")
  return(crayon::bold(paste0(...)))
  }
