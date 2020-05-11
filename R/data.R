#' Registry example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{Register}{String with registry name}
#'   \item{full_name}{String with registry full name}
#' }
"registry"


#' Organization example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{OrgNrRHF}{}
#'   \item{RHF}{}
#'   \item{OrgNrHF}{}
#'   \item{HF}{}
#'   \item{Hfkortnavn}{}
#'   \item{OrgNrShus}{}
#'   \item{OrgNavnEnhetsreg}{}
#'   \item{SykehusnavnLang}{}
#'   \item{SykehusNavn}{}
#'   \item{TaMed}{}
#' }
"org"


#' Indicator example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{IndId}{}
#'   \item{Register}{}
#'   \item{IndTittel}{}
#'   \item{IndNavn}{}
#'   \item{MaalNivaaGronn}{}
#'   \item{MaalNivaaGul}{}
#'   \item{MaalRetn}{}
#'   \item{BeskrivelseKort}{}
#'   \item{BeskrivelseLang}{}
#'
#' }
"indicator"

#' User_registry example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{Register}{}
#'   \item{user_id}{}
#' }
"user_registry"

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
