# test_that("utilitybeltfmt::fmt functions return expected results", {
#     for (input in c("Hi", "", 5, -2, "ASDASD", NULL, c("PART1", "PART2"))){
#       functions <- c(utilitybeltassertions::utilitybeltfmt::fmtbold, utilitybeltfmt::fmterror, utilitybeltfmt::fmtwarning, utilitybeltfmt::fmtsuccess)
#       for (fun in functions){
#         #message("\n\ninput:", input)
#         #browser()
#         testthat::expect_true(is.character(fun(input)))
#         testthat::expect_equal(length(fun(input)), 1)
#         testthat::expect_gt(nchar(fun(input)), nchar(input))
#       }
#     }
#   })
