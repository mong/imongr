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
