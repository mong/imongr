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


  ##### Queries for test data insertion #####
  fc <- file(system.file("2_create_tabs.sql", package = "imongr"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries2 <- strsplit(sql, ";")[[1]]

  fc <- file(system.file("3_create_indices.sql", package = "imongr"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries3 <- strsplit(sql, ";")[[1]]

  fc <- file(system.file("4_create_views.sql", package = "imongr"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries4 <- strsplit(sql, ";")[[1]]

  queries <- c(queries2, queries3, queries4)


  ##### Test users #####
  insert_user1 <- paste("INSERT INTO user VALUES (10, 'Tom',",
                 "'Tom',",
                 "'+0000000000', 'tom@nowhere.com', 1);")

  insert_user2 <- paste("INSERT INTO user VALUES (11, 'Dick',",
                 "'Dick',",
                 "'+0000000000', 'dick@nowhere.com', 1);")

  insert_user3 <- paste("INSERT INTO user VALUES (12, 'Harry',",
                 "'Harry',",
                 "'+0000000000', 'harry@nowhere.com', 1);")



  make_testdb <- function(db_name) {

    local_pool <- make_pool()

    query <- paste("CREATE DATABASE IF NOT EXISTS", db_name, "CHARACTER SET = 'utf8'",
                  "COLLATE = 'utf8_danish_ci';")

    pool::dbExecute(local_pool, query)

    drain_pool(local_pool)
    Sys.setenv("IMONGR_DB_NAME" = db_name)
    local_pool <- make_pool()

    for (i in seq_len(length(queries))) {
      pool::dbExecute(local_pool, queries[i])
    }

    insert_table(local_pool, table = "nation", df = imongr::nation)
    insert_table(local_pool, table = "rhf", df = imongr::rhf)
    insert_table(local_pool, table = "hf", df = imongr::hf)
    insert_table(local_pool, table = "hospital", df = imongr::hospital)
    insert_table(local_pool, table = "registry", df = imongr::registry)
    insert_table(local_pool, table = "ind", df = imongr::ind)
    insert_table(local_pool, table = "user", df = imongr::user)
    insert_table(local_pool, table = "user_registry", df = imongr::user_registry)
    insert_table(local_pool, table = "publish", df = imongr::publish)
    insert_table(local_pool, table = "delivery", df = imongr::delivery)
    insert_table(local_pool, table = "medfield", df = imongr::medfield)
    insert_table(local_pool, table = "registry_medfield",
                 df = imongr::registry_medfield)
    insert_table(local_pool, table = "data", df = imongr::data)

    pool::dbExecute(local_pool, insert_user1)
    pool::dbExecute(local_pool, insert_user2)
    pool::dbExecute(local_pool, insert_user3)

    drain_pool(local_pool)

    Sys.setenv(IMONGR_DB_NAME = env_name)
  }

  # Make test databases
  make_testdb("testdb_verify")
  make_testdb("testdb_prod")

  # Set up database connections
  Sys.setenv(IMONGR_DB_NAME = "testdb_verify")
  pool_verify <- make_pool()
  Sys.setenv(IMONGR_DB_NAME = "testdb_prod")
  pool_prod <- make_pool()
}

##### Test delivery data #####

# Registry id 8
delivery1 <- data.frame(context = "caregiver",
                 year = 2023,
                 orgnr = 974633574,
                 ind_id = "nakke1",
                 var = 0,
                 denominator = 1)

delivery2 <- data.frame(context = "caregiver",
                  year = 2023,
                  orgnr = 997005562,
                  ind_id = "nakke2",
                  var = 1,
                  denominator = 2)

delivery3 <- data.frame(context = "caregiver",
                  year = 2023,
                  orgnr = 997005562,
                  ind_id = "nakke1",
                  var = 2,
                  denominator = 3)

unrelated_delivery <- data.frame(context = "caregiver",
                  year = 2023,
                  orgnr = 974633574,
                  ind_id = "norgast_saarruptur",
                  var = 2,
                  denominator = 3)


# Check that the data is ok
test_that("valid vars pass the check", {
  expect_true(
    length(
      check_invalid_var(registry, delivery1, data.frame(), conf, pool_verify)$report
    ) == 0
  )
  expect_true(
    length(
      check_invalid_var(registry, delivery2, data.frame(), conf, pool_verify)$report
    ) == 0
  )

  expect_true(
    length(
      check_invalid_var(registry, delivery3, data.frame(), conf, pool_verify)$report
    ) == 0
  )
})

###########################################
#####            Test 1               #####
##### Check that data can be uploaded #####
###########################################

# In testdb_verify
# Tom uploads delivery 1

Sys.setenv("SHINYPROXY_USERNAME" = "Tom")

test_that("upload is working", {

  check_db()

  insert_data_verify(
  pool = pool_verify,
  df = delivery1,
  update = "2023-08-20",
  affirm = "2023-01-01"
)
  insert_agg_data(pool_verify, delivery1)
  dat_delivery <- pool::dbGetQuery(pool_verify, "SELECT * FROM delivery")

  dat_sorted <- dat_delivery[order(dat_delivery$id, decreasing = TRUE), ]
  latest <- dat_sorted[1, ]

  expect_equal(as.character(latest$latest_update), "2023-08-20")

  agg_data_date <- pool::dbGetQuery(pool_verify,
    "SELECT DISTINCT ind_id, delivery_latest_update
     FROM agg_data WHERE ind_id = 'nakke1'")

  # Same date for all unit_levels
  expect_equal(nrow(agg_data_date), 1)

  expect_equal(as.character(agg_data_date$delivery_latest_update[1]), "2023-08-20")
})

  #################################################
  #####               Test 2                  #####
  ##### Check that data are publish correctly #####
  #################################################

# In testdb_verify
# Tom publishes delivery 1

test_that("publishing is working", {
  check_db()

  dat_publish <- get_registry_data(pool_verify, 8)

  agg_data_verify <- pool::dbGetQuery(pool_verify, "SELECT * FROM agg_data")

  insert_data_prod(
        pool_verify = pool_verify,
        pool_prod = pool_prod,
        df = dat_publish,
        terms_version = version_info(newline = "")
        )

  insert_agg_data(pool_prod, dat_publish)

  agg_data_prod <- pool::dbGetQuery(pool_prod, "SELECT * FROM agg_data")

  publish <- pool::dbGetQuery(pool_prod, "SELECT * FROM publish")
  delivery <- pool::dbGetQuery(pool_prod, "SELECT * FROM delivery")

  expect_equal(agg_data_verify[, c(-13, -16)], agg_data_prod[, c(-13, -16)])
  expect_equal(nrow(publish), 2)

  latest_delivery <- delivery[delivery$id == max(delivery$id), ]

  expect_equal(latest_delivery$publish_id, publish$id[2])

  expect_equal(latest_delivery$published, 1)
})

############################################################
#####                  Test 3                          #####
##### Check that the delivery table in prod is correct #####
############################################################

# In testdb_prod
# Dick uploads a delivery to another registry, then
# delivery 2 and delivery 3. Harry publishes
# delivery 2 and delivery 3. The delivery to the different regustry
# remains unpublished

Sys.setenv("SHINYPROXY_USERNAME" = "Dick")

test_that("deliveries are correctly transferred to prod", {
  check_db()

  # Upload delivery from to a different registry
  # Delivery id 3
  insert_data_verify(
    pool = pool_verify,
    df = unrelated_delivery,
    update = "2023-08-20",
    affirm = "2023-01-01"
)
  insert_agg_data(pool_verify, delivery1)

  # Upload nakke2
  insert_data_verify(
    pool = pool_verify,
    df = delivery2,
    update = "2023-08-22",
    affirm = "2023-01-01"
  )

  insert_agg_data(pool_verify, delivery2)

  # Upload nakke1
  insert_data_verify(
    pool = pool_verify,
    df = delivery3,
    update = "2023-08-23",
    affirm = "2023-01-01"
  )

  insert_agg_data(pool_verify, delivery3)

  # Publish
  Sys.setenv("SHINYPROXY_USERNAME" = "Harry")

  dat_publish <- get_registry_data(pool_verify, 8)

  insert_data_prod( # Should iterate over deliveries
        pool_verify = pool_verify,
        pool_prod = pool_prod,
        df = dat_publish,
        terms_version = version_info(newline = "")
        )

  insert_agg_data(pool_prod, dat_publish)

  ########################################
  ##### Check the latest publish row #####
  ########################################

  dat_delivery_prod <- pool::dbGetQuery(pool_prod, "SELECT * FROM delivery ORDER BY id DESC")
  dat_publish_prod <- pool::dbGetQuery(pool_prod, "SELECT * FROM publish ORDER BY id DESC")

  latest_delivery <- dat_delivery_prod[1, ]
  latest_publish <- dat_publish_prod[1, ]

  expect_equal(as.character(latest_delivery$latest_update), "2023-08-23")
  expect_equal(nrow(dat_delivery_prod), 4) # 3 recent deliveries plus one from initialization
  expect_equal(dat_delivery_prod$published, c(1, 1, 1, 1)) # 3 recent deliveries plus one from initialization

  # Check that delivery has the user that uploaded the data
  expect_equal(latest_delivery$user_id, 11)

  # Check that publish has the user that published the data
  expect_equal(latest_publish$user_id, 12)

  # Check that the ids in delivery and publish match
  expect_equal(latest_delivery$publish_id, latest_publish$id)

  # Check that there is an unpublished delivery in verify
  dat_delivery_verify <- pool::dbGetQuery(pool_verify, "SELECT * from delivery ORDER BY id ASC")
  expect_equal(nrow(dat_delivery_verify), 5)
  expect_true(is.null(dat_delivery_verify$latest_delivery_update[3]))
  expect_equal(dat_delivery_verify$published[3], 0)


  ##################################################
  ##### Check that the dates are set correctly #####
  ##################################################

  ##### Verify #####
  agg_nakke1 <- pool::dbGetQuery(pool_verify,
    "SELECT DISTINCT ind_id, delivery_latest_update
     FROM agg_data WHERE ind_id = 'nakke1'")

  agg_nakke2 <- pool::dbGetQuery(pool_verify,
    "SELECT DISTINCT ind_id, delivery_latest_update
     FROM agg_data WHERE ind_id = 'nakke2'")

  # Same date for all unit_levels
  expect_equal(nrow(agg_nakke1), 1)
  expect_equal(nrow(agg_nakke2), 1)

  expect_equal(as.character(agg_nakke2$delivery_latest_update[1]), "2023-08-22")
  expect_equal(as.character(agg_nakke1$delivery_latest_update[1]), "2023-08-23")

  ##### Prod #####
  agg_nakke1 <- pool::dbGetQuery(pool_prod,
    "SELECT DISTINCT ind_id, delivery_latest_update
     FROM agg_data WHERE ind_id = 'nakke1'")

  agg_nakke2 <- pool::dbGetQuery(pool_prod,
    "SELECT DISTINCT ind_id, delivery_latest_update
     FROM agg_data WHERE ind_id = 'nakke2'")

  # Same date for all unit_levels
  expect_equal(nrow(agg_nakke1), 1)
  expect_equal(nrow(agg_nakke2), 1)

  expect_equal(as.character(agg_nakke2$delivery_latest_update[1]), "2023-08-22")
  expect_equal(as.character(agg_nakke1$delivery_latest_update[1]), "2023-08-23")

})

# clean up
## drop tables (in case tests are re-run on the same instance)

if (is.null(check_db(is_test_that = FALSE))) {
  conf <- get_config()

  pool::dbExecute(pool_verify, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")
  pool::dbExecute(pool_prod, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")

  pool::dbExecute(pool_verify, paste("DROP TABLE",
                        paste(names(conf$db$tab), collapse = ", "), ";"))

  pool::dbExecute(pool_prod, paste("DROP TABLE",
                        paste(names(conf$db$tab), collapse = ", "), ";"))
}
## if db dropped on Github Actions the coverage reporting will fail...
if (is.null(check_db(is_test_that = FALSE)) &&
  Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") != "true") {
    pool::dbExecute(pool_verify, "DROP DATABASE testdb_verify ;")
    pool::dbExecute(pool_prod, "DROP DATABASE testdb_prod;")
}
## drain pool
if (is.null(check_db(is_test_that = FALSE))) {
  drain_pool(pool_verify)
  drain_pool(pool_prod)
}

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
