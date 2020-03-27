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
check_db <- function() {
  if (Sys.getenv("IMONGR_CONTEXT") == "DEV") {
    NULL
  } else if (Sys.getenv("TRAVIS") == "true") {
    NULL
  } else {
    skip("Test skipped due to possible lack of database infrastructure")
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
if (is.null(check_db())) {
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
if (is.null(check_db())) {
  pool <- make_pool()
}

# make queries for creating tables
fc <- file(system.file("create_tabs.sql", package = "imongr"), "r")
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
fc <- file(system.file("create_indices.sql", package = "imongr"), "r")
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
  expect_true(insert_tab(pool, table = "org", df = imongr::org))
  expect_true(insert_tab(pool, table = "user", df = imongr::user))
  expect_true(insert_tab(pool, table = "delivery", df = imongr::delivery))
  expect_true(insert_tab(pool, table = "data", df = imongr::data))
})

test_that("data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_data(pool)), dim(imongr::data))
})

test_that("pool cannot be established when missing credentials", {
  Sys.unsetenv("IMONGR_DB_HOST")
  expect_error(make_pool())
  Sys.unsetenv("IMONGR_DB_NAME")
  expect_error(make_pool())
  Sys.unsetenv("IMONGR_DB_PASS")
  expect_error(make_pool())
  Sys.unsetenv("IMONGR_DB_USER")
  expect_error(make_pool())
})

# clean up
if (is.null(check_db())) {
  pool::dbExecute(pool, "drop database testdb;")
  drain_pool(pool)
}
file.remove("_imongr.yml")

# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
