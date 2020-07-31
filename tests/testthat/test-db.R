# For these tests to work locally make sure an instance of mysql server is
# available and that the necassary user privileges are provided, e.g. as SQL:
#   \code{grant all privileges on [DATABASE].* to '[USER]'@'localhost';}
# where [DATABASE] and [USER] correspond to whatever given in imongr config:
#   \code{conf <- imongr::get_config()}
# When run at Travis build servers [USER] must be set to 'travis' and with
# an empty password (as also assumed in the above localhost example). See also
# .travis.yml

# Database infrastructure is only guaranteed at Travis and our own dev env.
# Tests running on other environments should be skipped:
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

# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")

# Prepare for tests
## now, create a connection pool
create_config()
conf <- get_config()
if (is.null(check_db(is_test_that = FALSE))) {
  pool <- make_pool()
}
test_that("a pool object can be provided", {
  check_db()
  expect_equal(class(pool), c("Pool", "R6"))
})



test_that("database server is up and test database can be made", {
  check_db()
  query <- paste("CREATE DATABASE IF NOT EXISTS testdb CHARACTER SET = 'utf8'",
                 "COLLATE = 'utf8_danish_ci';")
  expect_equal(pool::dbExecute(pool, query), 1)
  expect_equal(class(pool::dbGetQuery(pool, "SELECT 1")), "data.frame")
})

test_that("pool can be drained (closed)", {
  check_db()
  expect_invisible(drain_pool(pool))
})

# create new pool pointing at testdb
conf$db$name <- "testdb"
write_config(conf)
if (is.null(check_db(is_test_that = FALSE))) {
  pool <- make_pool()
}

# make queries for creating tables
fc <- file(system.file("2_create_tabs.sql", package = "imongr"), "r")
t <- readLines(fc)
close(fc)
sql <- paste0(t, collapse = "\n")
queries <- strsplit(sql, ";")[[1]]

test_that("relevant tables can be made in test database, table", {
  check_db()
  for (i in seq_len(length(queries))) {
    expect_equal(pool::dbExecute(pool, queries[i]), 0)
  }
})

# make queries for creating indices
fc <- file(system.file("3_create_indices.sql", package = "imongr"), "r")
t <- readLines(fc)
close(fc)
sql <- paste0(t, collapse = "\n")
queries <- strsplit(sql, ";")[[1]]

test_that("test tables can be indexed", {
  check_db()
  for (i in seq_len(length(queries))) {
    expect_equal(pool::dbExecute(pool, queries[i]), 0)
  }
})

test_that("database can be populated with test data", {
  check_db()
  expect_true(insert_tab(pool, table = "nation", df = imongr::nation))
  expect_true(insert_tab(pool, table = "rhf", df = imongr::rhf))
  expect_true(insert_tab(pool, table = "hf", df = imongr::hf))
  expect_true(insert_tab(pool, table = "shus", df = imongr::shus))
  expect_true(insert_tab(pool, table = "registry", df = imongr::registry))
  expect_true(insert_tab(pool, table = "org", df = imongr::org))
  expect_true(insert_tab(pool, table = "indicator", df = imongr::indicator))
  expect_true(insert_tab(pool, table = "user", df = imongr::user))
  expect_true(insert_tab(pool, table = "user_registry",
                         df = imongr::user_registry))
  expect_true(insert_tab(pool, table = "delivery", df = imongr::delivery))
  expect_true(insert_tab(pool, table = "data", df = imongr::data))
})

test_that("agg_data can be populated from existing (test) data", {
  check_db()
  # to save ptime, use just a sub-sample of data (reused on retreival)
  data_sample <- get_data(pool, sample = .4)
  expect_true(insert_agg_data(pool, data_sample))
})

test_that("function provides error when inserting non-consistent data", {
  check_db()
  expect_error(insert_tab(pool, table = "wrong_table", df = data.frame))
  expect_error(insert_tab(pool, table = "org",
                          df = cbind(imongr::org, unvalid_var = 1)))
  expect_error(insert_tab(pool, table = "org",
                          df = data.frame(name = "", OrgNr = 123456789,
                                          valid = 1)))
  org_wrong_name <- imongr::org
  true_name <- names(org_wrong_name)[1]
  wrong_name <- paste0(true_name, "borked")
  names(org_wrong_name)[1] <- wrong_name
  expect_error(insert_tab(pool, table = "org", df = org_wrong_name))
})

test_that("data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_data(pool)), dim(imongr::data))
  expect_true(dim(get_data(pool, sample = .1))[1] < dim(imongr::data)[1])
})

test_that("delivery data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_delivery(pool)), dim(imongr::delivery))
  expect_true(
    dim(get_delivery(pool, sample = .9))[1] <= dim(imongr::delivery)[1]
  )
})

test_that("user data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_user(pool)), dim(imongr::user))
  expect_true(dim(get_user(pool, sample = .9))[1] <= dim(imongr::user)[1])
})

test_that("org data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_org(pool))[1], dim(imongr::org)[1])
  expect_true(dim(get_org(pool, sample = .1))[1] < dim(imongr::org)[1])
})

test_that("indicator data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_indicator(pool)), dim(imongr::indicator))
  expect_true(
    dim(get_indicator(pool, sample = .1))[1] < dim(imongr::indicator)[1]
  )
})

test_that("registry name can be fetched from test database", {
  check_db()
  expect_equal(class(get_registry_name(pool, 1)), "character")
})

test_that("registries and indicators per registry can be fetched", {
  check_db()
  registries <- unique(imongr::data$registry_id)
  expect_equal(class(get_registry_indicators(pool, registries[1])),
               "data.frame")
  expect_equal(class(get_registry(pool)), "data.frame")
  expect_true(
    dim(get_registry(pool, sample = .5))[1] < dim(imongr::registry)[1]
  )
})

test_that("user_registry data can be fetched from test database", {
  check_db()
  expect_equal(class(get_user_registry(pool)), "data.frame")
  expect_true(
    dim(get_user_registry(pool, sample = .5))[1] <
      dim(imongr::user_registry)[1]
  )
})

test_that("aggregated data can be fetched from test database", {
  check_db()
  expect_equal(class(get_agg_data(pool)), "data.frame")
})

test_that("get_table wrapper function do work", {
  check_db()
  expect_equal(class(get_table(pool, "org")), "data.frame")
})

test_that("pool cannot be established when missing credentials", {
  file.remove("_imongr.yml")
  Sys.unsetenv("IMONGR_DB_HOST")
  expect_error(make_pool(), "IMONGR_DB_HOST")
  Sys.setenv(IMONGR_DB_HOST = "test")
  Sys.unsetenv("IMONGR_DB_NAME")
  expect_error(make_pool(), "IMONGR_DB_NAME")
  Sys.setenv(IMONGR_DB_NAME = "test")
  Sys.unsetenv("IMONGR_DB_USER")
  expect_error(make_pool(), "IMONGR_DB_USER")
  Sys.setenv(IMONGR_DB_USER = "test")
  Sys.unsetenv("IMONGR_DB_PASS")
  expect_error(make_pool(), "IMONGR_DB_PASS")
})

# clean up
## drop tables (in case tests are re-run on the same instance)
if (is.null(check_db(is_test_that = FALSE))) {
  pool::dbExecute(pool,
                  paste("DROP TABLE",
                        paste(names(conf$db$tab), collapse = ", "), ";")
  )
}

## if db dropped on travis the following coverage will fail...
if (is.null(check_db(is_test_that = FALSE)) &&
    Sys.getenv("TRAVIS") != "true") {
  pool::dbExecute(pool, "drop database testdb;")
}
## finally, drain pool
if (is.null(check_db(is_test_that = FALSE))) {
  drain_pool(pool)
}

# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
