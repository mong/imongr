# For these tests to work locally make sure an instance of mysql server is
# available and that the necassary user privileges are provided, e.g. as SQL:
#   \code{grant all privileges on [DATABASE].* to '[USER]'@'localhost';}
# where [DATABASE] and [USER] correspond to whatever given in imongr config:
#   \code{conf <- imongr::get_config()}
# When run at Github Actions build servers [USER] must be set to 'actions' and
# with an empty password (as also assumed in the above localhost example).
# See also .github/workflows/R-CMD-check.yml

# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
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
  query <- paste(
    "CREATE DATABASE IF NOT EXISTS testdb CHARACTER SET = 'utf8'",
    "COLLATE = 'utf8_danish_ci';"
  )
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
  expect_true(insert_table(pool, table = "nation", df = imongr::nation))
  expect_true(insert_table(pool, table = "rhf", df = imongr::rhf))
  expect_true(insert_table(pool, table = "hf", df = imongr::hf))
  expect_true(insert_table(pool, table = "hospital", df = imongr::hospital))
  expect_true(insert_table(pool, table = "registry", df = imongr::registry))
  expect_true(insert_table(pool, table = "ind", df = imongr::ind))
  expect_true(insert_table(pool, table = "user", df = imongr::user))
  expect_true(insert_table(pool,
    table = "user_registry",
    df = imongr::user_registry
  ))
  expect_true(insert_table(pool, table = "publish", df = imongr::publish))
  expect_true(insert_table(pool, table = "delivery", df = imongr::delivery))
  expect_true(insert_table(pool, table = "data", df = imongr::data))
  expect_true(insert_table(pool,
    table = "medfield",
    df = imongr::medfield
  ))
  expect_true(insert_table(pool,
    table = "registry_medfield",
    df = imongr::registry_medfield
  ))
})

test_that("agg_data can be populated from existing (test) data", {
  check_db()
  # to save ptime, use just a sub-sample of data (reused on retrieval)
  data_sample <- get_table(pool, "data", sample = .4)
  expect_message(insert_agg_data(pool, data_sample))
})

test_that("a complete set will be aggregated if given subset of inds", {
  check_db()
  dat <- get_table(pool, "data")
  ind <- unique(dat$ind_id)[1]
  dat <- dat[dat$ind_id == ind, ]
  expect_message(insert_agg_data(pool, dat))
})

test_that("function provides error when inserting non-consistent data", {
  check_db()
  expect_error(insert_table(pool, table = "wrong_table", df = data.frame))
  expect_error(insert_table(pool,
    table = "org",
    df = cbind(imongr::org, unvalid_var = 1)
  ))
  expect_error(insert_table(pool,
    table = "org",
    df = data.frame(
      name = "", OrgNr = 123456789,
      valid = 1
    )
  ))
  wrong_var_name <- data.frame(
    delivery_id = 100,
    unit_level = "hospital",
    orgnr = 974633574,
    year = 2018,
    var = 0,
    numerator = 1,
    ind_id = "norgast_andel_avdoede_bykspytt_tolv"
  )
  expect_error(insert_table(pool, table = "data", df = wrong_var_name))
  too_many_vars <- data.frame(
    delivery_id = 100,
    unit_level = "hospital",
    orgnr = 974633574,
    year = 2018,
    var = 0,
    denominator = 1,
    ind_id = "norgast_andel_avdoede_bykspytt_tolv",
    sucker = TRUE
  )
  expect_error(insert_table(pool, table = "data", df = too_many_vars))
})

test_that("data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_table(pool, "data")), dim(imongr::data))
  expect_true(
    dim(get_table(pool, "data", sample = .1))[1] < dim(imongr::data)[1]
  )
})

test_that("delivery data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_table(pool, "delivery")), dim(imongr::delivery))
  expect_true(
    dim(get_table(pool, "delivery", .9))[1] <= dim(imongr::delivery)[1]
  )
})

test_that("user data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_user(pool)), dim(imongr::user))
  expect_true(dim(get_user(pool, sample = .9))[1] <= dim(imongr::user)[1])
})

test_that("all user can be fetched", {
  check_db()
  expect_equal(class(get_users(pool)), "data.frame")
  expect_equal(class(get_users(pool, valid = FALSE)), "data.frame")
})

test_that("indicator data can be fetched from test database", {
  check_db()
  expect_equal(dim(get_table(pool, "ind")), dim(imongr::ind))
  expect_true(
    dim(get_table(pool, "ind", sample = .1))[1] < dim(imongr::ind)[1]
  )
})

test_that("registry name can be fetched from test database", {
  check_db()
  expect_equal(class(get_registry_name(pool, 1)), "character")
})

test_that("registries and indicators per registry can be fetched", {
  check_db()
  registries <- unique(imongr::registry$id)
  expect_equal(
    class(get_registry_indicators(pool, registries[1])),
    "data.frame"
  )
  expect_equal(class(get_table(pool, "registry")), "data.frame")
  expect_true(
    dim(get_table(pool, "registry", sample = .5))[1] < dim(imongr::registry)[1]
  )
})

test_that("an indicators registry can be found", {
  check_db()
  indicator <- get_table(pool, "ind")$id[1]
  expect_equal(class(get_indicators_registry(pool, indicator)), "integer")
})

test_that("all indicator data subset for a registry can be provided", {
  check_db()
  registry <- unique(imongr::registry$id)[1]
  expect_equal(class(get_registry_ind(pool, registry)), "data.frame")
  expect_true(dim(get_registry_ind(pool, registry))[1] > 0)
  expect_true(dim(get_registry_ind(pool, registry))[2] > 0)
})

test_that("an org name is provided from org number", {
  check_db()
  orgnr <- unique(imongr::hospital$orgnr)[1]
  expect_equal(class(get_org_name(pool, orgnr)), "character")
})

test_that("user_registry data can be fetched from test database", {
  check_db()
  expect_equal(class(get_table(pool, "user_registry")), "data.frame")
  expect_true(
    dim(get_table(pool, "user_registry", sample = .5))[1] <
      dim(imongr::user_registry)[1]
  )
})

test_that("aggregated data can be fetched from test database", {
  check_db()
  expect_equal(class(get_table(pool, "agg_data")), "data.frame")
})

test_that("medfields of registries can be fetched", {
  check_db()
  expect_equal(class(get_registry_medfield(pool, 1)), "data.frame")
})

test_that("registries medfields can be fetched", {
  check_db()
  expect_equal(class(get_medfield_registry(pool, 1)), "data.frame")
})


test_that("registry-medfield tab can be updated", {
  check_db()
  df <- imongr::registry_medfield
  expect_invisible(update_registry_medfield(pool, df))
})

test_that("users registries can be fetched", {
  check_db()
  expect_equal(class(get_registry_user(pool, 1)), "data.frame")
})

test_that("users registries can be updated", {
  check_db()
  df <- imongr::user_registry
  expect_invisible(update_registry_user(pool, df))
})

test_that("aggdata delivery timings can be provided", {
  check_db()
  aggdata_delivery <- get_aggdata_delivery(pool, c("dummy_ind_id"))
  expect_equal(class(aggdata_delivery), "data.frame")
  expect_true(class(aggdata_delivery$id) %in% c("integer"))
  expect_true(
    class(aggdata_delivery$delivery_time)[1] %in% c("POSIXct", "POSIXt")
  )
  expect_equal(class(aggdata_delivery$delivery_latest_update[1]), "Date")
  expect_equal(class(aggdata_delivery$delivery_latest_affirm[1]), "Date")
})

test_that("get_table wrapper function do work", {
  check_db()
  expect_equal(class(get_table(pool, "user")), "data.frame")
})


file.remove("_imongr.yml")

test_that("pool cannot be established when missing credentials", {
  check_db()
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
  pool::dbExecute(pool, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")
  pool::dbExecute(
    pool,
    paste(
      "DROP TABLE",
      paste(names(conf$db$tab), collapse = ", "), ";"
    )
  )
}

## if db dropped on Github Actions the following coverage will fail...
if (is.null(check_db(is_test_that = FALSE)) &&
      Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") != "true") {
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
