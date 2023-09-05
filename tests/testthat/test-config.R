tmpdir <- tempdir()

test_that("valid config can be loaded from package", {
  expect_equal(class(get_config(dir = tmpdir)), "list")
  expect_silent(check_config(get_config()))
})

test_that("local config can be handled properly", {
  expect_error(check_config(list()))
  expect_true(grepl("fill it in", create_config(dir = tmpdir)))
  expect_true(grepl("already exists", create_config(dir = tmpdir)))
  expect_equal(class(get_config(dir = tmpdir)), "list")
})

conf <- get_config(dir = tmpdir)
conf_file <- paste0(tmpdir, "/test.yml")
test_that("local config can be made from a list in session", {
  expect_silent(write_config(
    config = conf, dir = tmpdir,
    filename = basename(conf_file)
  ))
})

# clean up
file.remove(conf_file)
file.remove(paste0(tmpdir, "/", "_imongr.yml"))
