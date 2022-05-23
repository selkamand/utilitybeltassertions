#' @title Test Assertion with Invisible Return
#' @description
#' Wraps around asserthat::assertthat::assert_that() but makes return value invisible (can be assigned but will not print when not assigned)
#' @param ... see ?assertthat::assert_that
#' @param env see ?assertthat::assert_that
#' @param msg see ?assertthat::assert_that
#' @return invisible (TRUE) if expression is TRUE. Will error if is FALSE
#' @family customassertions
#' @export
assert_that_invisible <- function(..., env = parent.frame(), msg = NULL){
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
  assertthat::assert_that(assertthat::is.string(object), msg = fmterror("assert_non_empty_string:  The object [", substitute(object), "] must be a string, not a ", class(object), ". ", msg))
  assertthat::assert_that(nchar(object) > 0, msg = fmterror("assert_non_empty_string: object [", substitute(object), "] is a string but it is empty (''). ", msg))
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
  assertthat::assert_that(assertthat::is.number(object), msg=fmterror("assert_is_whole_number: ", "The object [", substitute(object), "] is a '", class(object), "',not a number (a length one numeric vector).", msg))
  assertthat::assert_that(object - round(object) == 0, msg=fmterror("assert_is_whole_number: ", "The object [", substitute(object), "] is not a whole number (no decimal place).", msg))
}

#' assert_names_include
#'
#' @param object an object (usually vector or dataframe) that you want to assert has certain names
#' @param expected_names names you expect the object to have (order doesn't matter) (character vector)
#' @param object_name_in_error_message how to refer to the object in the error message
#'
#' @export
#'
#' @examples
#' assert_names_include(mtcars, expected_names = c("mpg", "cyl"))
assert_names_include <- function(object, expected_names, object_name_in_error_message = NA){
  assertthat::assert_that(is.character(expected_names) ,msg = paste0("assert_names_include: expected names should be a 'character vector', not a '", class(expected_names) ,"'"))

  if(is.na(object_name_in_error_message)){
    object_name_in_error_message = as.character(substitute(object))
  }

  names = names(object)
  names_not_included = paste0(expected_names[!expected_names %in% names], collapse = ", ")
  assertthat::assert_that(
    all(expected_names %in% names),
    msg = fmterror("[",object_name_in_error_message,"]"," does not contain all expected names. missing [", names_not_included, "]"))
}

#' Assert a program is in path
#'
#' Check if program is available in path Should work on all operating systems
#'
#' @param program_names name/s of program to search for in path (character)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' assert_program_exists_in_path(c("grep", "wget"))
#' }
assert_program_exists_in_path <- function(program_names){
  assertthat::assert_that(is.character(program_names))

  program_paths <- Sys.which(program_names)
  programs_not_found <- program_paths[program_paths==""]
  programs_not_found_string <- paste0(names(programs_not_found), collapse = ", ")
  assertthat::assert_that(
    !any(Sys.which(program_names)==""),
    msg = fmterror("Could not find executable/s [",programs_not_found_string,"] in PATH Please ensure tool is installed, marked as executable, and available in your path")
  )
}

#' Assert file has the expected extension
#'
#' Take a filename / vector of filenames and assert that they all end with one of the user-supplied 'valid extensions'
#'
#' @param filenames filenames to assert has a valid extension (character)
#' @param valid_extensions all possible valid extensions (character)
#' @param ignore_case does the case (uppercase/lowercase) of the extensions matter? (bool)
#'
#' @export
#'
#' @examples
#' # Ensure filename has a "fasta" or 'fa' extension
#' assert_filenames_have_valid_extensions(
#'   filename="sequence.fasta",
#'   valid_extensions = c("fasta", "fa")
#' )
assert_filenames_have_valid_extensions <- function(filenames, valid_extensions, ignore_case = TRUE){
  pattern=paste0(paste0("\\.",valid_extensions, "$"), collapse = "|")

  filenames_have_valid_extension = grepl(x=filenames, pattern = pattern, ignore.case = ignore_case)
  filenames_lacking_valid_extension = filenames[!filenames_have_valid_extension]
  assertthat::assert_that(all(filenames_have_valid_extension), msg = fmterror("assert_string_has_valid_extension: Filenames [", paste0(filenames_lacking_valid_extension, collapse = ", ") ,"] do not contain valid extensions [", paste0(valid_extensions, collapse = ", ") ,"]"))
}

#' Do all files exist
#'
#' @param filepaths filepaths (character)
#' @param supplementary_error_message supplementary error message - e.g. a line describing what functions to run to produce required files (string)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' assert_files_exist(
#'   c("/path/to/file1", "/path/to/file2"),
#'   supplementary_error_message = "Please run X to produce files you need"
#' )
#' }
assert_files_exist <- function(filepaths, supplementary_error_message = ""){
  files_exist_lgl <- file.exists(filepaths)
  files_that_dont_exist <- filepaths[!files_exist_lgl]

  assertthat::assert_that(all(files_exist_lgl), msg = fmterror("assert_files_exist: Could not find the file/s [", paste0(files_that_dont_exist, collapse = ", "), "].", "\n", supplementary_error_message))
}

#' Check values are in set
#'
#' Check if test_values are one of those described in 'set' vector
#'
#' @param test_values values to check are equal to one of the values in set (character/numeric)
#' @param acceptable_values valid options for elements in test_values (character/numeric)
#' @param name name used to describe test values. Used in error message reporting i.e.  ('name' must be one of)
#'
#' @export
#'
#' @examples
#' set = c("A", "B", "C")
#' letters = c("A", "B", "B")
#' assert_all_values_are_in_set(letters, set)
#'
assert_all_values_are_in_set <- function(test_values, acceptable_values, name = rlang::caller_arg(test_values)){
  elements_not_included_in_set = unique(test_values[!test_values %in% acceptable_values])
  assertthat::assert_that(length(elements_not_included_in_set) == 0, msg = fmterror("assert_all_are_in: ",name," must only contain the elements [", paste0(unique(acceptable_values), collapse = ", "), "]. \n\nThe following values do not meet this criterion:\n", paste0(elements_not_included_in_set, collapse = "\n")))

}
