###########################################
##### Tester for publisering til prod #####
###########################################

##### Copied from test-mod.R and test-upload.R #####

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

# Make test databases in verify and prod
if (is.null(check_db(is_test_that = FALSE))) {

  # Queries for test data insertion
  fc <- file(system.file("2_create_tabs.sql", package = "imongr"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- strsplit(sql, ";")[[1]]

  # Users
  insert_user1 = paste("INSERT INTO user VALUES (10, 'Tom',",
                 "'Tom',",
                 "'+0000000000', 'tom@nowhere.com', 1);")

  insert_user2 = paste("INSERT INTO user VALUES (11, 'Dick',",
                 "'Dick',",
                 "'+0000000000', 'dick@nowhere.com', 1);")

  insert_user3 = paste("INSERT INTO user VALUES (12, 'Harry',",
                 "'Harry',",
                 "'+0000000000', 'harry@nowhere.com', 1);")


  make_testdb <- function(test_context) {
    pool <- make_pool(context = test_context)
    query <- paste("CREATE DATABASE IF NOT EXISTS testdb CHARACTER SET = 'utf8'",
                  "COLLATE = 'utf8_danish_ci';")
    pool::dbExecute(pool, query)

    drain_pool(pool)
    # New connections to testdb
    Sys.setenv(IMONGR_DB_NAME = "testdb")

    pool = make_pool(context = test_context)

    for (i in seq_len(length(queries))) {
      pool::dbExecute(pool, queries[i])
    }

    insert_table(pool, table = "nation", df = imongr::nation)
    insert_table(pool, table = "rhf", df = imongr::rhf)
    insert_table(pool, table = "hf", df = imongr::hf)
    insert_table(pool, table = "hospital", df = imongr::hospital)
    insert_table(pool, table = "registry", df = imongr::registry)
    insert_table(pool, table = "ind", df = imongr::ind)
    insert_table(pool, table = "user", df = imongr::user)
    insert_table(pool, table = "user_registry", df = imongr::user_registry)
    insert_table(pool, table = "delivery", df = imongr::delivery)
    insert_table(pool, table = "medfield", df = imongr::medfield)
    insert_table(pool, table = "registry_medfield",
                df = imongr::registry_medfield)
    insert_table(pool, table = "data", df = imongr::data)

    pool::dbExecute(pool, insert_user1)
    pool::dbExecute(pool, insert_user2)
    pool::dbExecute(pool, insert_user3)

    drain_pool(pool)

    Sys.setenv(IMONGR_DB_NAME = env_name)
  }


 # Make test databases

  make_testdb("verify")
  make_testdb("prod")
}



# Test delivery data


delivery1 <- data.frame(context = "caregiver",
                 year = 2018,
                 orgnr = 974633574,
                 ind_id = "nakke1",
                 var = 0,
                 denominator = 1)

delivery2 <- data.frame(context = "caregiver",
                  year = 2023,
                  orgnr = 997005562,
                  ind_id = "nakke1",
                  var = 1,
                  denominator = 2)

delivery3 <- data.frame(context = "caregiver",
                  year = 2023,
                  orgnr = 997005562,
                  ind_id = "nakke1",
                  var = 2,
                  denominator = 3)


  ##### Oppsett #####
# - Lag to testdatabaser (verify og prod) og
#   sett inn testdata.

Sys.setenv(IMONGR_DB_NAME = "testdb")
pool <- make_pool(context = "verify")

test_that("valid vars pass the check", {
  expect_true(
    length(
      check_invalid_var(registry, delivery1, data.frame(), conf, pool)$report
    ) == 0
  )
})

test_that("valid vars pass the check", {
  expect_true(
    length(
      check_invalid_var(registry, delivery2, data.frame(), conf, pool)$report
    ) == 0
  )
})

test_that("valid vars pass the check", {
  expect_true(
    length(
      check_invalid_var(registry, delivery3, data.frame(), conf, pool)$report
    ) == 0
  )
})

Sys.setenv("SHINYPROXY_USERNAME" = "Tom")

insert_data(
  pool = pool,
  df = delivery1,
  update = "2023-08-20",
  affirm = "2023-01-01"
)
insert_agg_data(pool, delivery1)

test_that("upload is working", {
  check_db()
  dat_delivery <- pool::dbGetQuery(pool, "SELECT * FROM delivery")
  dat_sorted = dat_delivery[order(dat_delivery$id, decreasing = TRUE), ]
  latest = dat_sorted[1,]
  browser()
  expect_equal(as.character(latest$latest_update), "2023-08-20")
})

##### Oppsett #####
# - Lag to testdatabaser (verify og prod) og
#   sett inn testdata.

##### Case 1 #####
# - Last opp tre leveranser til verify og publiser.
# - Sjekk i delivery at:
#   - De samme leveransene er overført til prod
#   - Har rett id til registrering i publish-tabellen
#   - Har samme checksum
#   - Har har samme bruker-id
#   - Har rett datoer
# - Sjekk i publish at:
#   - Har bruker-id til brukeren som trykker publiser
#   - Har rett register-id
#   - Har rett dato
# - Aggreger data og sjekk at agg-data er lik mellom
#   de to databasene for registeret.





# clean up
## drop tables (in case tests are re-run on the same instance)
clean_up <- function(test_context) {
  pool <- make_pool(test_context)
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
}

clean_up("verify")
clean_up("prod")

## and finally, remove local config##### End copy #####
file.remove("_imongr.yml")

# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
Sys.setenv(SHINYPROXY_USERNAME = env_user_name)
Sys.setenv(SHINYPROXY_USERGROUPS = env_user_groups)

##### End copy #####
