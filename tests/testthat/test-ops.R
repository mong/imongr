# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

# create a local config for testing
create_config()


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


Sys.setenv(SHINYPROXY_USERNAME = "")
test_that("an error is provided when the function finds no user", {
  expect_error(get_user_name())
})

Sys.setenv(SHINYPROXY_USERNAME = "imongr")
test_that("a username can be returned", {
  expect_equal(get_user_name(), "imongr")
})

Sys.setenv(SHINYPROXY_USERGROUPS = "")
test_that("an error is provided when the function finds no groups", {
  expect_error(get_user_groups())
})

Sys.setenv(SHINYPROXY_USERGROUPS = "000000000,999999999")
test_that("user groups can be returned", {
  expect_equal(get_user_groups(), c("000000000", "999999999"))
})


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
}


test_that("user data is a data frame", {
  check_db()
  expect_equal(class(get_user_data(pool)), "data.frame")
})


test_that("error is provided when the current user is not in db", {
  check_db()
  expect_error(get_user_id(pool), "No data on the current user!")
})

## then onto some content from package ready-made data
if (is.null(check_db(is_test_that = FALSE))) {
  insert_tab(pool, table = "nation", df = imongr::nation)
  insert_tab(pool, table = "rhf", df = imongr::rhf)
  insert_tab(pool, table = "hf", df = imongr::hf)
  insert_tab(pool, table = "hospital", df = imongr::hospital)
  insert_tab(pool, table = "registry", df = imongr::registry)
  insert_tab(pool, table = "ind", df = imongr::ind)
  insert_tab(pool, table = "user", df = imongr::user)
  insert_tab(pool, table = "user_registry", df = imongr::user_registry)
  insert_tab(pool, table = "delivery", df = imongr::delivery)
  insert_tab(pool, table = "data", df = imongr::data[1:100, ])
}

Sys.setenv(SHINYPROXY_USERNAME = "mongr")
test_that("a user id can be provided", {
  check_db()
  expect_equal(get_user_id(pool), 1)
})

test_that("all users data can be provided", {
  check_db()
  expect_equal(class(get_all_user_data(pool)), "data.frame")
  expect_equal(class(get_user_registries(pool)), "character")
  expect_equal(class(get_user_deliveries(pool)), "data.frame")
})

test_that("a consistent md5 checksum of a data frame can be provided", {
  expect_equal(md5_checksum(data.frame(name = "imongr")),
               "ed91fb7bafe2bd55f90522e1104a13f1")
})

conf <- get_config()
test_that("no (real) delivery has been made yet", {
  check_db()
  expect_false(delivery_exist_in_db(pool, df = imongr::data[1:100, ]))
})

### data frame for re-use next two tests
df <- imongr::data[1:100, ]
df_var_ind <- names(df) %in% conf$db$tab$data$insert[conf$upload$data_var_ind]
df <- df[, df_var_ind]

### make a delivery
if (is.null(check_db(is_test_that = FALSE))) {
  insert_data(pool, df)
}


test_that("existing user will not be (re-)created", {
  check_db()
  expect_equal(create_imongr_user(pool, data.frame(
    user_name = "mongr",
    name = "Mongr No",
    phone = "+4747474747",
    email = "jesus@sky.com",
    stringsAsFactors = FALSE)),
    "User already exists. Nothing to do!")
})

test_that("error on delete when no 'Register' or several registries", {
  check_db()
  df <- imongr::data
  expect_error(delete_registry_data(pool, df))
  names(df)[names(df) == "Register"] <- "missing"
  expect_error(delete_registry_data(pool, df))
})

test_that("data from a registry can be fetched", {
  check_db()
  registry <- levels(as.factor(imongr::registry$id))[1]
  expect_equal(class(get_registry_data(pool, registry)), "data.frame")
})


test_that("a new user can be created", {
  check_db()
  expect_true(create_imongr_user(pool, data.frame(
    user_name = "jesus",
    name = "Jesus Christ",
    phone = "+4747474747",
    email = "jesus@sky.com",
    valid = 1,
    stringsAsFactors = FALSE)))
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
