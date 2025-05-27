# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")
env_aws_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
env_aws_secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
env_aws_id <- Sys.getenv("distribution_id")
env_az_user_name <- Sys.getenv("AZ_USER_NAME")
env_az_secret <- Sys.getenv("AZ_SECRET")
env_az_tenant <- Sys.getenv("AZ_TENANT")
env_api_endpoint_name <- Sys.getenv("API_ENDPOINT_NAME")


# create a local config for testing
create_config()


# Database infrastructure is only guaranteed at Github Actions and
# our own dev env. Tests running on other environments should be skipped.
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("IMONGR_CONTEXT") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

test_that("check on natural numbers can be performed", {
  expect_false(natural(-1))
  expect_true(natural(1))
  expect_equal(natural(c(0, 1.1, 2)), c(TRUE, FALSE, TRUE))
  expect_false(natural("1"))
  expect_false(natural(c(1, 2, 3, NA)))
})

test_that("a consistent md5 checksum of a data frame can be provided", {
  expect_equal(
    md5_checksum(
      df = data.frame(name = "imongr"),
      ind = data.frame(ind = "indicate")
    ),
    "f788a20fc4c125063183bf9273d7c6eb"
  )
})

test_that("empty rows are removed from a data frame", {
  test_data <- data.frame(
    x = c(1, 2, 3, NA, 5, NA, 6, NA, NA),
    y = c("a", "b", "c", "", "e", "f", "g", "", ""),
    z = c(1, 2, 3, NA, 5, 6, 7, NA, NA)
  )

  expected_output <- test_data[c(-4, -8, -9), ]

  expect_equal(remove_empty_rows(test_data), expected_output)
})

# For the remianing tests we need a test database
## first off with no data
if (is.null(check_db(is_test_that = FALSE))) {
  pool <- make_pool()
  query <- paste(
    "CREATE DATABASE IF NOT EXISTS testdb CHARACTER SET = 'utf8'",
    "COLLATE = 'utf8_danish_ci';"
  )
  pool::dbExecute(pool, query)

  # new connections to testdb
  drain_pool(pool)
  Sys.setenv(IMONGR_DB_NAME = "testdb")
  pool <- make_pool()

  # add tabs to testdb
  fc <- file(system.file("2_create_tabs.sql", package = "imongr"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- strsplit(sql, ";")[[1]]
  for (i in seq_len(length(queries))) {
    pool::dbExecute(pool, queries[i])
  }
}

# we need a user and groups defined
Sys.setenv(SHINYPROXY_USERNAME = "tester")
Sys.setenv(SHINYPROXY_USERGROUPS = "G1,G2")

test_that("some html is provided for user_widget", {
  expect_equal(class(user_widget()), "shiny.navbarmenu")
})

test_that("version info is provided for imongr", {
  expect_true(grepl("imongr", version_info()))
})

test_that("opt out ok messages are provided", {
  conf <- get_config()
  expect_true(
    no_opt_out_ok() %in% conf$app_text$action_button$no_opt_out_ok
  )
})

test_that("sample data can be inserted into db", {
  check_db()
  expect_invisible(insert_sample_data())
})

test_that("sample data can be deleted from db", {
  check_db()
  expect_message(delete_all_data(prompt = FALSE))
})

# clean up
## drop tables (in case tests are re-run on the same instance)
if (is.null(check_db(is_test_that = FALSE))) {
  conf <- get_config()
  pool::dbExecute(pool, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")
  pool::dbExecute(
    pool,
    paste(
      "DROP TABLE",
      paste(names(conf$db$tab), collapse = ", "), ";"
    )
  )
}
## if db dropped on Github Actions the coverage reporting will fail...
if (is.null(check_db(is_test_that = FALSE)) &&
      Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") != "true") {
  pool::dbExecute(pool, "drop database testdb;")
}
## drain pool
if (is.null(check_db(is_test_that = FALSE))) {
  drain_pool(pool)
}
## and finally, remove local config
file.remove("_imongr.yml")

test_that("invalidate_cache is (not) working", {
  # Needs az-cli installed to test. Not installed, I guess
  expect_null(invalidate_cache())
})

# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
Sys.setenv(SHINYPROXY_USERNAME = env_user_name)
Sys.setenv(SHINYPROXY_USERGROUPS = env_user_groups)
Sys.setenv(AWS_ACCESS_KEY_ID = env_aws_key)
Sys.setenv(AWS_SECRET_ACCESS_KEY = env_aws_secret)
Sys.setenv(AZ_USER_NAME = env_az_user_name)
Sys.setenv(AZ_SECRET = env_az_secret)
Sys.setenv(AZ_TENANT = env_az_tenant)
Sys.setenv(API_ENDPOINT_NAME = env_api_endpoint_name)
Sys.setenv(distribution_id = env_aws_id)
