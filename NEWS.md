# Unreleased

# imongr 1.0.2

Erstatt `NA` med tom tekst for kort og lang beskrivelse som hentes fra db ([#275](https://github.com/mong/imongr/pull/275)). `NULL` i `ind`-verdier ga `NA` i indikator-fane-felt, og dermed aktiv "Oppdater tekst"-knapp.

# imongr 1.0.1

* Mulighet til å velge gult målnivå hvis grønn er `NULL` ([#272](https://github.com/mong/imongr/pull/272))

# imongr 1.0.0

## App changes

* Do not aggregate data if updating `ind` table ([#258](https://github.com/mong/imongr/pull/258))
* No longer calculate indicator level ([#254](https://github.com/mong/imongr/pull/254))
* Remove choose encoding option ([#262](https://github.com/mong/imongr/pull/262))
* Update all relevant `ind` values when publishing ([#260](https://github.com/mong/imongr/pull/260))
* Only show *Oppdater tekster* button when new text ([#264](https://github.com/mong/imongr/pull/264))
* Extended indicator editor ([#208](https://github.com/mong/imongr/pull/208))
* Added a report with brief summary of status for all registries ([#209](https://github.com/mong/imongr/pull/209))

## Dev stuff

* Modularization of shiny app ([#244](https://github.com/mong/imongr/pull/244) and [#245](https://github.com/mong/imongr/pull/245))
* Allow 120 characters per line ([#243](https://github.com/mong/imongr/pull/243))
* `docker-compose`: 
  - Expose db instances to the outside, so they can be used by API/`mongts` ([#231](https://github.com/mong/imongr/pull/231))
  - Mount local source code into docker-compose ([#239](https://github.com/mong/imongr/pull/239))
* Use Renovate instead of dependabot
* Changed name of main branch from `master` to `main`

# imongr 0.28.4
* Duplicate check incorporate indicator table data (to be extended in next feature version)
* Relaxed natural number checks, now only applied to data that are true fraction indicators

# imongr 0.28.3
* Fixed encoding related issue when downloading data ([#197](https://github.com/mong/imongr/pull/197)).
* Fixed minor issues (typos and GUI texts and default values)
* Fixed problem with update of indicator title and descriptions (without updating data) by including relevant indicator data when checking for possible duplicate upload

# imongr 0.28.2
* Fixed some issues on first time registration of users ([#195](https://github.com/mong/imongr/pull/195)).

# imongr 0.28.1
* Fixed missing fields in download of the ind table. Returned fields now based on config and for future changes in fields the corresponding function does not have to altered as long as config is kept up to date ([#193](https://github.com/mong/imongr/pull/193)).

# imongr 0.28.0
* Change of styles to ease future bootstrap transitions by use of _bslib_ ([#191](https://github.com/mong/imongr/pull/191))
* New field _sformat_ added to the indicator table to allow frontend formatting ([#192](https://github.com/mong/imongr/pull/192))
* Deprecated function _update_aggdata_delivery_time()_ set to defunct

# imongr 0.27.0
* On data upload for verification both an update and affirm date that apply for all indicator data within the selected registry can be manually provided ([#175](https://github.com/mong/imongr/pull/175)). The update date is to be understood as the latest update of indicator data while affirm is to provide a date which after indicator data must be regarded as tentative.
* As a result of the above some functions were deprecated and replaced by new ones and guide texts updated

# imongr 0.26.2
* New logout url, hopefully working along shinyproxy and aws cognito oddities
* Profile data now from prod db, only

# imongr 0.26.1
* Reconfigured logout uri
* Fixed wrong date language in terms pdf by [system locale settings in base docker image](https://github.com/mong/imongr-base-r/pull/7) 

# imongr 0.26.0
* Accepted terms text kept (in db) on each publish
* Added hyperlink to full description of terms
* Added more information on how we use and store user data
* Fixed SQL causing error in development environment
* Base R upgraded from 4.1.0 to 4.1.2 and subsequently call to shinyAlert was removed from client 

# imongr 0.25.0
* New features: upload of files only to verify instance and new publish to production instance
* All manager tools organized under same navbar menu
* App texts updated and improved based on the above changes

# imongr 0.24.1

* Fixed agg error when there are only pre-aggregated values in data set ([#146](https://github.com/mong/imongr/pull/146))

# imongr 0.24.0

* Added editing of existing indicators in GUI ([#143](https://github.com/mong/imongr/pull/143))
* Fixed bug in processing pre-calculated values with large denominator ([#141](https://github.com/mong/imongr/issues/141), [#144](https://github.com/mong/imongr/issues/144))

# imongr 0.23.0

* Upgraded base R version 4.0.3 -> 4.1.0 
* Added check for var (numerator) <= denominator ([#130](https://github.com/mong/imongr/issues/130))

# imongr 0.22.2

* Fixed error aggregating dg indicators and applying these to none dg indicators ([#128](https://github.com/mong/imongr/issues/128))
* Corrected some typos in function docs


# imongr 0.22.1

* Updated contact info and links
* Added constraints to the ind table to prevent errors on manual updates
* Added url and description fields in registry table
* Fixed misspelled context

# imongr 0.22.0

* Changed processing of target levels to avoid visual "errors" ([#122](https://github.com/mong/imongr/pull/122))
* Reactivated tests on reading csv file and aggregating real data ([#123](https://github.com/mong/imongr/pull/123))

# imongr 0.21.0

* Added new field delivery_time to agg_data table for easier access to last data update in qmongr(js) ([#121](https://github.com/mong/imongr/pull/121))

# imongr 0.20.1

* Fixed bug deleting all indicators regardless of context ([#118](https://github.com/mong/imongr/pull/118))

# imongr 0.20.0

* Added new checks for natural numbers and zero denominator ([#114](https://github.com/mong/imongr/pull/114))
* Also as a result of the above, new functions were added
* App texts added and updated

# imongr 0.19.1

* Adjustments of logo, favicon and app title ([#113](https://github.com/mong/imongr/pull/113))

# imongr 0.19.0

* GUI management of user-registry associations ([#112](https://github.com/mong/imongr/pull/112))

# imongr 0.18.0

* Added user controlled multiple data backends ([#110](https://github.com/mong/imongr/pull/110))

# imongr 0.17.0

* Added new variable 'context' to data ([#100](https://github.com/mong/imongr/pull/100))

# imongr 0.16.2

* Include all registries in GUI for managing registry and medical field relations ([PR #105](https://github.com/mong/imongr/pull/105))

# imongr 0.16.1

* Changed logout url according to new (but temporary) domain imongr.skde.org (#106)

# imongr 0.16.0

* Extended data model to cover registry medical fields (topics) #101
* Added GUI for managing registry and medical field relations

# imongr 0.15.1

* Updated `DESCRIPTION` with release version, updated `NEWS.md`, and removed `.travis.yml`

# imongr 0.15.0

* Use Github actions instead of Travis and Appveyor

# imongr 0.14.1

* Revert https://github.com/mong/imongr/commit/144ac9 introduced in release `0.13.0` (aggregating before putting raw data in db)

# imongr 0.14.0

* add (missing) endpoints for 'green' when aggregating levels 

# imongr 0.13.0

* provide new field 'min_denominator' from ind table

# imongr 0.12.0

* consolidated functions, also removing some
* increased testing
* small changes to config
* organized functions into categories in web site docs

# imongr 0.11.0

* clean-up button in app for agg_data
* configurabel time zone (default Oslo) for delivery history
* removed row names in downloaded data
* increased allowed cyclomatic complexity of functions mosty to avoid devops spam...

# imongr 0.10.0

* added spooky aggregation on names as well (should be changed in future versions)
* opt in delivery history to speed up app init
* fixed horrific bug in data aggregation

# imongr 0.9.0

* removed bling-bling from gui
* colour and fonts made more consistent with frame host
* multi registry aggregation
* all data (re-) aggration available in app
* fixed missing variable pronouns and docs

# imongr 0.8.0

* processing dg (coverage) and dependent indicators during aggregation
* better tab names and with mouseover texts
* one-click sending of upload error reports in gui
* more tables for download were added (to be adjusted in future versions)
* info in gui on valid indicators for selected registry
* on reading csv file the app will try an alternative encoding before throwing an error
* extended app config
* some errors and shortcomings were fixed

# imongr 0.7.1

* fixed missing check on valid user

# imongr 0.7.0

* minor remodelling of db
* fixed aggregation of multilevel data, but remains to tested...

# imongr 0.6.0

* first take on generic aggregation, with bugs and still missing median handling 

# imongr 0.5.0

* redesign of data model
* delivery depending on indicator(s) rather than registry
* interdependence of indicators, primarily for dg

# imongr 0.4.2

* shiny package from CRAN (and not latest from github)

# imongr 0.4.1

* fix bug in data aggregation

# imongr 0.4.0

* aggregate data on upload
* new functions interacting with db
* extended testing

# imongr 0.3.0

* increased uplad limit to 50 Mb
* improved docs on what happend to existing data at upload
* quality assessment now only accepts registry speciffic indicators
* added encoding option when data are downloaded
* some changes of texts and minor bugfixes

# imongr 0.2.0

* new field 'nevner' in data
* norwegian tabs
* profile page with upload history and reciept
* info widget in to right corner
* code logics moved to reduce complexity of server function
* new config entries
* infrastructure: ci/cd by travis

# imongr 0.1.0

* First proper release of imongr
