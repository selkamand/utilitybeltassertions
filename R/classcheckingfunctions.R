#' class_is
#'
#' Check if object has a particular class
#'
#' @param object object whose class you want to check (object)
#' @param tested_class class (string)
#'
#' @return TRUE if object class matches tested_class. FALSE if not.
#' @export
#'
class_is <- function(object, tested_class){
  assertthat::assert_that(is.character(tested_class))
  all(class(object) == tested_class)
}
