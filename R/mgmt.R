#' Functions for organization and user management in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame corresponding to database table fields
#' @param user_name String defining the username db field. Most likely its
#' content should be unique
#' @param name String defining any given name to the entity
#' @param orgnumber Integer providing a global id of the entity
#' @param valid Integer defining if the entity is valid (1) or deprecated (0).
#' Default value is 1
#' @param phone String providing the phone number of a user
#' @param email String providing the e-mail of a user
#' @return These functions are mainly fronends to DBI and the returned value is
#' mainly what will be porvided by the underlying DBI functions
#' @name mgmt
#' @aliases create_imongr_org create_imongr_user
NULL


#' @rdname mgmt
#' @export
create_imongr_org <- function(pool, df) {

  org <- get_org(pool)
  if (df$OrgNrShus %in% org$OrgNrShus) {
    return("Organization already exists. Nothing to do!")
  } else {
    msg <- insert_tab(pool, "org", df)
    return(msg)
  }
}


#' @rdname mgmt
#' @export
create_imongr_user <- function(pool, df) {

  user <- get_user(pool)
  if (df$user_name %in% user$user_name) {
    return("User already exists. Nothing to do!")
  } else {
    msg <- insert_tab(pool, "user", df)
    return(msg)
  }
}
