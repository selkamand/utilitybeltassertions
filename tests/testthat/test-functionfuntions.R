test_that("get_calling_function works", {
  wrapper <- function(n_function_calls_ago=1, verbose=TRUE){
    get_calling_function(n_function_calls_ago, verbose)
  }

  expect_equal(wrapper(), "wrapper()")

  wrapper_wrapper <- function(n_function_calls_ago=1, verbose=TRUE){
    wrapper(n_function_calls_ago, verbose=verbose)
  }

  expect_true(assertthat::is.string(wrapper()))
  expect_equal(wrapper_wrapper(), "wrapper(n_function_calls_ago, verbose = verbose)")
  expect_equal(wrapper_wrapper(2), "wrapper_wrapper(2)")
  expect_equal(wrapper_wrapper(100, verbose=FALSE), "base")
  expect_message(wrapper_wrapper(100, verbose=TRUE))
  expect_error(wrapper_wrapper("100"))
  expect_error(wrapper_wrapper(1:10)) #Errors now but maybe one day will return a vector of strings.

})


test_that("function argument counting works", {
  # Calling from outside a named function
  fun = function(a, b, c) { return(a+b-c) }
  expect_equal(fun_count_arguments(fun), 3)

  # Calling from outside an anonymous
  expect_equal(fun_count_arguments(function(a, b, c) { return(a+b+c) }), 3)

  # Calling from inside a named function
  my_function <- function(a, b, c, d, e, f, g) { return(fun_count_arguments(my_function)) }
  expect_equal(my_function(), 7)

  #Intentionally Error
  expect_error(fun_count_arguments("STRING"))
  expect_error(fun_count_arguments(5))
})
