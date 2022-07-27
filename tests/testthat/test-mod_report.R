create_config()

conf <- get_config()

# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

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

# For the remaining tests we need a test database
## first off with no data
if (is.null(check_db(is_test_that = FALSE))) {
  pool <- make_pool()
  query <- paste("CREATE DATABASE IF NOT EXISTS testdb CHARACTER SET = 'utf8'",
                 "COLLATE = 'utf8_danish_ci';")
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

  insert_sample_data()

  # add a user with no deliveries
  query <- paste("INSERT INTO user VALUES (10, 'nobody@nowhere.com',",
                 "'Mr Nobody',",
                 "'+0000000000', 'nobody@nowhere.com', 1);")
  pool::dbExecute(pool, query)
}

### TEST

test_that("report module provides a shiny app object", {
  check_db()
  expect_equal(class(report_app()), "shiny.appobj")
})

test_that("report module ui returns returns a shiny tag list object", {
  check_db()
  expect_true("shiny.tag.list" %in% class(report_ui("id")))
})

test_that("report module server provides sensible output", {
  check_db()
  shiny::testServer(
    report_server, args = list(pool = pool, pool_verify = pool), {

      # when unknown report an empty data frame should be returned
      session$setInputs(
        report = "",
        file_format = "csv"
      )
      expect_true("data.frame" %in% class(df()))
      expect_equal(dim(df()), c(0, 0))

      session$setInputs(
        report = "Registerstatus",
        file_format = "csv"
      )
      expect_true("data.frame" %in% class(df()))
      expect_true(dim(df())[1] > 0)
    }
  )
})

# clean up
## drop tables (in case tests are re-run on the same instance)
if (is.null(check_db(is_test_that = FALSE))) {
  conf <- get_config()
  pool::dbExecute(pool,
                  paste("DROP TABLE",
                        paste(names(conf$db$tab), collapse = ", "), ";")
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

# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
Sys.setenv(SHINYPROXY_USERNAME = env_user_name)
Sys.setenv(SHINYPROXY_USERGROUPS = env_user_groups)
