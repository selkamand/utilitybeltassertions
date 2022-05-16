test_that("assert_that wrapper works", {
    expect_true(assertthat::assert_that(is.character("Hi friends")))
    expect_true(assertthat::assert_that(is.numeric(5)))
    expect_true(assertthat::assert_that(is.data.frame(data.frame(hi=1:5, alpha = LETTERS[1:5]))))
    expect_error(assertthat::assert_that(is.double(c("Hi", "Friends"))))

    #Test output is invisible (see ?invisible())
    #logfile.path = paste0(system.file(package="utilitybelt"), "/assert_that_unit_test.log")
    #sink(logfile.path)
    #assertthat::assert_that(is.character("Hi friends"))
    #sink()
    #expect_true(file.size(logfile.path)==0)
    #file.remove(logfile.path)
  })

test_that("assert_non_empty_string works", {
    valid_string="Billy"
    valid_string_single_char="A"
    number = 5
    character = c("Billy", "Bob")
    empty_string=""
    expect_true(assert_non_empty_string(valid_string))
    expect_true(assert_non_empty_string(valid_string, msg = "Does adding a message anything this up?"))
    expect_true(assert_non_empty_string(valid_string_single_char))
    expect_error(assert_non_empty_string(number), "numeric")
    expect_error(assert_non_empty_string(character), "character")
    expect_error(assert_non_empty_string(empty_string), "empty")
    expect_error(assert_non_empty_string(empty_string, msg = "THIS IS MY ERROR MESSAGE"), "THIS IS MY ERROR MESSAGE")
    expect_error(assert_non_empty_string(character, msg = "THIS IS MY ERROR MESSAGE"), "THIS IS MY ERROR MESSAGE")
  })


test_that("assert_is_whole_number works", {
    expect_true(assert_is_whole_number(1))
    expect_true(assert_is_whole_number(10))
    expect_true(assert_is_whole_number(100.000))
    expect_true(assert_is_whole_number(-10))
    expect_true(assert_is_whole_number(-10.0))
    expect_error(assert_is_whole_number("STRING"))
    expect_error(assert_is_whole_number(1.2))
    expect_error(assert_is_whole_number(10000.5))
    expect_error(assert_is_whole_number(c(1, 2)))
    expect_error(assert_is_whole_number(list(1, 2)))
    expect_error(assert_is_whole_number("string", msg = "THIS IS MY ERROR MESSAGE"), "THIS IS MY ERROR MESSAGE")
  })

test_that("assert_filename_has_valid_extension works", {
  expect_true(assert_filename_has_valid_extension(filename = "test.txt", valid_extensions = "txt"))
  expect_true(assert_filename_has_valid_extension(filename = "test.txt", valid_extensions = c("txt", "tsv")))
  expect_true(assert_filename_has_valid_extension(filename = "test.txt.idx", valid_extensions = c("txt.idx")))
  expect_true(assert_filename_has_valid_extension(filename = "test.txt.idx", valid_extensions = c("idx", "txt")))
  expect_true(assert_filename_has_valid_extension(filename = c("test.txt", "test2.txt"), valid_extensions = c("txt")))
  expect_true(assert_filename_has_valid_extension(filename = c("test.txt", "test2.txt"), valid_extensions = c("idx","txt")))

  expect_error(assert_filename_has_valid_extension(filename = "test.txt.idx", valid_extensions = c("txt")))
  expect_error(assert_filename_has_valid_extension(filename = c("test.txt", "test2.txt", "test3.idx"), valid_extensions = c("txt")), regexp = "test3.idx")
  expect_error(assert_filename_has_valid_extension(filename = c("test.txt", "test3.idx", "test2.txt"), valid_extensions = c("txt")), regexp = "test3.idx")

  })


test_that("assert_files_exist", {

  # Set up test environment
  filename1=tempfile(pattern = "testfile")
  filename2=tempfile(pattern = "testfile")
  filename3=tempfile(pattern = "testfile")
  filename_nonexistant=tempfile()

  file.create(filename1)
  file.create(filename2)
  file.create(filename3)

  # Run tests
  expect_true(assert_files_exist(filepaths = filename1))
  expect_true(assert_files_exist(filepaths = c(filename1, filename2, filename3)))

  expect_error(assert_files_exist(filepaths = filename_nonexistant), regexp = filename_nonexistant)
  expect_error(assert_files_exist(filepaths = c(filename1, filename2, filename3, filename_nonexistant)), regexp = filename_nonexistant)

  # cleanup temp files
  file.remove(c(filename1, filename2, filename3))
})
