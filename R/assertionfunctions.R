#' @title Test Assertion with Invisible Return
#' @description
#' Wraps around asserthat::assert_that() but makes return value invisible (can be assigned but will not print when not assigned)
#' @param ... see ?assertthat::assert_that
#' @param env see ?assertthat::assert_that
#' @param msg see ?assertthat::assert_that
#' @return invisible (TRUE) if expression is TRUE. Will error if is FALSE
#' @family customassertions
#' @export
assert_that <- function(..., env = parent.frame(), msg = NULL){
  return(invisible(assertthat::assert_that(..., env=env, msg=msg)))
}


#' Check object is a non-empty string
#'
#' @param object Some value you want to assert is a non-empty string
#' @param msg Some message to print on failure (appended to the hard-coded message). Will automatically get wrapped in fmterror (string)
#' @return invisible(TRUE) if the object is a non-empty string. Throws an error if it is not.
#' @examples
#' possiblestring = "Billy"
#' assert_non_empty_string(possiblestring)
#'
#' @family customassertions
#'
#' @export
assert_non_empty_string <- function(object, msg=""){
  assert_that(assertthat::is.string(object), msg = fmterror("assert_non_empty_string:  The object [", substitute(object), "] must be a string, not a ", class(object), ". ", msg))
  assert_that(nchar(object) > 0, msg = fmterror("assert_non_empty_string: object [", substitute(object), "] is a string but it is empty (''). ", msg))
}


#' Check object is a Whole Number
#'
#' Checks if object is a whole number (e.g. 1, 5, 5.0, 6.000). Vectors are flagged as NOT whole numbers (intentional behaviour).
#'
#' @param object Some value you want to assert is a whole number (single scalar value)
#' @param msg Some message to print on failure (appended to the hard-coded message). Will automatically get wrapped in fmterror (string)
#' @return invisible(TRUE) if the object passes the assertion. Throws an error if it does not.
#' @examples
#' assert_is_whole_number(5)
#'
#' @family customassertions
#'
#' @export
assert_is_whole_number <- function(object, msg=""){
  assert_that(assertthat::is.number(object), msg=fmterror("assert_is_whole_number: ", "The object [", substitute(object), "] is a '", class(object), "',not a number (a length one numeric vector).", msg))
  assert_that(object - round(object) == 0, msg=fmterror("assert_is_whole_number: ", "The object [", substitute(object), "] is not a whole number (no decimal place).", msg))
}
