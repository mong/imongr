# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

test_that("env is provided for user", {
  Sys.setenv(SHINYPROXY_USERNAME = "")
  expect_error(get_user_name())
  Sys.setenv(SHINYPROXY_USERNAME = "testuser")
  expect_equal(get_user_name(), "testuser")
})

test_that("env is provided for groups", {
  Sys.setenv(SHINYPROXY_USERGROUPS = "")
  expect_error(get_user_groups())
  Sys.setenv(SHINYPROXY_USERGROUPS = "GROUP1,GROUP2")
  expect_equal(get_user_groups(), c("GROUP1", "GROUP2"))
})

test_that("env db host is provided", {
  expect_error(db_host(context = "undefined environment"))
  Sys.unsetenv("IMONGR_DB_HOST")
  expect_error(db_host())
  Sys.setenv(IMONGR_DB_HOST = "testhost")
  expect_equal(db_host(), "testhost")
  Sys.setenv(IMONGR_DB_HOST_VERIFY = "testhost_verify")
  expect_equal(db_host(context = "verify"), "testhost_verify")
  Sys.setenv(IMONGR_DB_HOST_QA = "testhost_qa")
  expect_equal(db_host(context = "qa"), "testhost_qa")
})

test_that("env db name is provided", {
  Sys.unsetenv("IMONGR_DB_NAME")
  expect_error(db_name())
  Sys.setenv(IMONGR_DB_NAME = "testdb")
  expect_equal(db_name(), "testdb")
})

test_that("env db username is provided", {
  Sys.unsetenv("IMONGR_DB_USER")
  expect_error(db_username())
  Sys.setenv(IMONGR_DB_USER = "testdbuser")
  expect_equal(db_username(), "testdbuser")
})

test_that("env db password is provided", {
  Sys.unsetenv("IMONGR_DB_PASS")
  expect_error(db_password())
  Sys.setenv(IMONGR_DB_PASS = "testdbpass")
  expect_equal(db_password(), "testdbpass")
})

test_that("env adminer url is provided", {
  Sys.unsetenv("IMONGR_ADMINER_URL")
  expect_warning(adminer_url())
  Sys.setenv(IMONGR_ADMINER_URL = "testadminerurl")
  expect_equal(adminer_url(), "testadminerurl")
})


# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
Sys.setenv(SHINYPROXY_USERNAME = env_user_name)
Sys.setenv(SHINYPROXY_USERGROUPS = env_user_groups)
