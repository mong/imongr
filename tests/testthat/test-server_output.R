create_config()

# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

# Database infrastructure is only guaranteed at Travis and our own dev env.
# Tests running on other environments should be skipped
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("IMONGR_CONTEXT") == "DEV") {
    NULL
  } else if (Sys.getenv("TRAVIS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# make a sample df to be used for the remaining tests
df <- data.frame(Aar = 2016,
                 ShNavn = "UNN-Tromsoe",
                 ReshId = 601225,
                 OrgNrShus = 974795787,
                 Variabel = 0,
                 nevner = NA,
                 KvalIndID = "norgast1",
                 Register = "norgast")


# For the remianing tests we need a test database
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
}

conf <- get_config()

# make a sample df
df <- data.frame(year = 2014,
                 orgnr_hospital = 874716782,
                 ind_id = "nakke1",
                 var = 0,
                 denominator = NA,
                 registry_id = 4)

# we need a user and groups defined
Sys.setenv(SHINYPROXY_USERNAME = "mongr")
Sys.setenv(SHINYPROXY_USERGROUPS = "G1,G2")


test_that("profile_ui provides a characer string", {
  check_db()
  expect_equal(class(
    profile_ui(conf, pool, valid_user = FALSE, "TU", "TG")),
               "character")
  expect_equal(class(
    profile_ui(conf, pool, valid_user = TRUE, "imongr", "PROVIDER")),
    "character"
  )
})

test_that("select list of registries is provided (if any)", {
  check_db()
  expect_equal(class(select_registry_ui(conf, pool)), "shiny.tag")
})

test_that("submit ui is provided", {
  check_db()
  expect_null(submit_ui(conf, pool, NULL, 4, df))
  expect_equal(class(
    submit_ui(conf, pool, TRUE, 4,
              df[, !names(df) %in% c("denominator", "registry_id")]))[1],
    "shiny.tag.list"
  )
})

test_that("error report is provided", {
  check_db()
  expect_null(error_report_ui(pool, df, NULL, "norgast"))
  expect_equal(class(
    error_report_ui(pool, df[, !names(df) %in% c("nevner", "Register")], TRUE,
                    "norgast")),
    "character"
  )
})

test_that("upload sample text is provided", {
  check_db()
  expect_null(upload_sample_text_ui(pool, conf, NULL, 4))
  expect_equal(class(
    upload_sample_text_ui(pool, conf, TRUE, 4)),
    "character"
  )
})

test_that("upload sample ui is provided", {
  expect_null(upload_sample_ui(df, NULL, "norgast", 1, FALSE))
  expect_equal(class(
    upload_sample_ui(df, TRUE, "norgast", 1, FALSE)),
    "data.frame"
  )
})

test_that("text for var doc ui is provided", {
  expect_equal(class(var_doc_ui(conf)), "character")
})

# clean up
## drop tables (in case tests are re-run on the same instance)
if (is.null(check_db(is_test_that = FALSE))) {
  pool::dbExecute(pool,
                  paste("DROP TABLE",
                        paste(names(conf$db$tab), collapse = ", "), ";")
  )
}
## if db dropped on travis the coverage reporting will fail...
if (is.null(check_db(is_test_that = FALSE)) &&
    Sys.getenv("TRAVIS") != "true") {
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
