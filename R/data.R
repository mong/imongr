#' Organization example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{name}{String with name of organization. The person (user) adding
#'   data to qmonger must belong to an organization found in \code{org$name}}
#'   \item{orgnumber}{Numeric id of organization}
#'   \item{valid}{Numeric defining if organization is active (1) of not (0)}
#' }
"org"

#' User example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{user_name}{String user name of the person altering data, \emph{e.g.}
#'   'jesusc'}
#'   \item{user}{String full name of the person altering data, \emph{e.g.}
#'   'Jesus Christ'}
#'   \item{phone}{String phone number of the person altering data, \emph{e.g.}
#'   '+4747474747}
#'   \item{email}{String e-mail address of the person altering data,
#'   \emph{e.g.} 'jesusc\@sky.com'}
#'   \item{valid}{Numeric definig if user is active (1) or not (0)}
#'   \item{org_id}{Numeric id referencing the organization the user belings to}
#'   }
"user"

#' Delivery example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{latest}{Numeric definig if the delivery is the latest (1) or not(0)}
#'   \item{md5_checksum}{String A hash of the delivered data set used for
#'   integrity checking}
#'   \item{user_id}{Numeric id referencing the user that made the delivery}
#'   }
"delivery"

#' Example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{Aar}{Numeric 4 digit year}
#'   \item{ShNavn}{String hospital name}
#'   \item{ReshId}{String internal id of hospital}
#'   \item{OrgNrShus}{Numeric 9 digit id of hospital}
#'   \item{Variabel}{String variable name}
#'   \item{kvalIndID}{Numeric id of quality indicator}
#'   \item{delivery_id}{Numeric id referencing the delivery}
#'   }
"data"
