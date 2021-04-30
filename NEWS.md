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
