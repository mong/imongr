#' Organization structure example data
#'
#' Org hierarcy
#' @format Data frame with example data
#' \describe{
#'   \item{id}{}
#'   \item{orgnr}{}
#'   \item{full_name}{}
#'   \item{short_name}{}
#' }
"nation"

#' Organization structure example data
#'
#' Org hierarcy
#' @format Data frame with example data
#' \describe{
#'   \item{id}{}
#'   \item{orgnr}{}
#'   \item{full_name}{}
#'   \item{short_name}{}
#'   \item{ref_id}{}
#' }
"rhf"

#' Organization structure example data
#'
#' Org hierarcy
#' @format Data frame with example data
#' \describe{
#'   \item{id}{}
#'   \item{orgnr}{}
#'   \item{full_name}{}
#'   \item{short_name}{}
#'   \item{ref_id}{}
#' }
"hf"

#' Organization structure example data
#'
#' Org hierarcy
#' @format Data frame with example data
#' \describe{
#'   \item{id}{}
#'   \item{orgnr}{}
#'   \item{full_name}{}
#'   \item{short_name}{}
#'   \item{ref_id}{}
#' }
"hospital"

#' Organization structure example data
#'
#' Flat org hierarcy
#' @format Data frame with example data
#' \describe{
#'   \item{hospital_id}{}
#'   \item{hospital}{}
#'   \item{orgnr_hospital}{}
#'   \item{hf}{}
#'   \item{orgnr_hf}{}
#'   \item{rhf}{}
#'   \item{orgnr_rhf}{}
#'   \item{national}{}
#'   \item{orgnr_national}{}
#' }
"org"

#' Organization structure example data
#'
#' Flat org hierarcy. To be removed...
#' @format Data frame with example data
#' \describe{
#'   \item{orgnr}{}
#'   \item{unit_level}{}
#' }
"orgnr"

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


#' Indicator example data for qmongr data store
#'
#' Meta data to allow interactions with qmong data. Only used for testing in
#' this package. Please refere to the overall data model (ref)
#' @format Data frame with made up values
#' \describe{
#'   \item{id}{}
#'   \item{title}{}
#'   \item{name}{}
#'   \item{level_green}{}
#'   \item{level_yellow}{}
#'   \item{level_direction}{}
#'   \item{short_description}{}
#'   \item{long_description}{}
#'   \item{registry_id}{}
#'
#' }
"ind"

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
