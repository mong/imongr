# Package index

## Configuration

Handling configuration

- [`create_config()`](config.md) [`write_config()`](config.md)
  [`get_config()`](config.md) [`check_config()`](config.md) : Functions
  handling imongr R package config

## Database

Basic database interaction

- [`make_pool()`](db.md) [`drain_pool()`](db.md)
  [`insert_table()`](db.md) [`get_table()`](db.md)
  [`get_table_raw()`](db.md) : Low level database functions for imongr

## Get data

Retrieve data from database

- [`get_all_user_data()`](db_get.md) [`get_user_data()`](db_get.md)
  [`get_user_id()`](db_get.md) [`get_user_registries()`](db_get.md)
  [`get_user_registry_select()`](db_get.md)
  [`get_user_deliveries()`](db_get.md)
  [`get_registry_data()`](db_get.md)
  [`get_indicators_registry()`](db_get.md)
  [`get_registry_ind()`](db_get.md) [`get_registry_name()`](db_get.md)
  [`get_registry_short_name()`](db_get.md)
  [`get_registry_full_name()`](db_get.md) [`get_org_name()`](db_get.md)
  [`get_hospitals()`](db_get.md) [`get_hfs()`](db_get.md)
  [`get_rhfs()`](db_get.md) [`get_flat_org()`](db_get.md)
  [`get_all_orgnr()`](db_get.md) [`get_user()`](db_get.md)
  [`get_users()`](db_get.md) [`get_registry_indicators()`](db_get.md)
  [`get_dg_indicators()`](db_get.md)
  [`get_registry_medfield()`](db_get.md)
  [`get_medfield_registry()`](db_get.md)
  [`get_registry_user()`](db_get.md) [`get_user_registry()`](db_get.md)
  [`get_users_per_registry()`](db_get.md)
  [`get_aggdata_delivery()`](db_get.md) [`get_aggdata()`](db_get.md)
  [`get_review_collaborators()`](db_get.md)
  [`get_registry_projects()`](db_get.md)
  [`get_project_hospitals()`](db_get.md)
  [`get_publications()`](db_get.md) [`get_ind_agg_data()`](db_get.md)
  [`get_ind_units()`](db_get.md) [`get_ind_limits()`](db_get.md) :
  Retreiv data from imongr database

## Data operations

Specific data operations

- [`delete_indicator_data()`](ops.md) [`delete_agg_data()`](ops.md)
  [`insert_data_verify()`](ops.md) [`insert_data_prod()`](ops.md)
  [`insert_agg_data()`](ops.md) [`update_aggdata_delivery()`](ops.md)
  [`agg_all_data()`](ops.md) [`clean_agg_data()`](ops.md)
  [`create_imongr_user()`](ops.md)
  [`update_registry_medfield()`](ops.md)
  [`update_registry_user()`](ops.md)
  [`update_registry_user_role()`](ops.md) : Functions for data
  operations in imongr

## Aggregation

Prepare data for qmongr by aggregation

- [`agg()`](aggregate.md) [`agg_dg()`](aggregate.md)
  [`agg_from_level()`](aggregate.md) : Aggregate hospital data

## Environment

Provides relevant settings

- [`get_user_name()`](getenv.md) [`get_user_groups()`](getenv.md)
  [`db_host()`](getenv.md) [`db_name()`](getenv.md)
  [`db_username()`](getenv.md) [`db_password()`](getenv.md)
  [`adminer_url()`](getenv.md) : Get relevant settings either from
  config og environmental variables

## Upload data

Processing and validation of upload data

- [`check_report()`](upload.md) [`mail_check_report()`](upload.md)
  [`check_upload()`](upload.md) [`check_missing_registry()`](upload.md)
  [`check_mixing_ind()`](upload.md) [`check_missing_var()`](upload.md)
  [`check_invalid_var()`](upload.md)
  [`check_invalid_context()`](upload.md)
  [`check_invalid_org()`](upload.md) [`check_invalid_ind()`](upload.md)
  [`check_numeric_var()`](upload.md) [`check_natural_var()`](upload.md)
  [`check_overflow_var()`](upload.md)
  [`check_numeric_denominator()`](upload.md)
  [`check_natural_denominator()`](upload.md)
  [`check_zero_denominator()`](upload.md)
  [`check_duplicated_inds()`](upload.md)
  [`check_values_exists()`](upload.md)
  [`check_numeric_year()`](upload.md)
  [`check_natural_year()`](upload.md) [`csv_to_df()`](upload.md)
  [`sample_df()`](upload.md) [`indicator_is_fraction()`](upload.md)
  [`filter_fraction_indicator()`](upload.md) : Upload data to imongr

## Tools

Miscellaneous functions

- [`natural()`](misc.md) [`md5_checksum()`](misc.md)
  [`remove_empty_rows()`](misc.md) [`user_widget()`](misc.md)
  [`version_info()`](misc.md) [`no_opt_out_ok()`](misc.md)
  [`insert_sample_data()`](misc.md) [`delete_all_data()`](misc.md)
  [`invalidate_cache()`](misc.md) : Tools and whatever

## App server output

For use by the app server function

- [`profile_ui()`](mod_profile.md) [`profile_server()`](mod_profile.md)
  [`profile_app()`](mod_profile.md) : Shiny module providing GUI and
  server logic for (user) profile
- [`select_registry_ui()`](server_output.md)
  [`submit_ui()`](server_output.md)
  [`error_report_ui()`](server_output.md)
  [`warning_report_ui()`](server_output.md)
  [`upload_sample_text_ui()`](server_output.md)
  [`upload_sample_ui()`](server_output.md)
  [`var_doc_ui()`](server_output.md)
  [`medfield_summary_text_ui()`](server_output.md) : Functions that
  provide server output

## App

Application server, user interface, modules and start-up

- [`run_app()`](run_app.md) : Run the imongr Shiny Application
- [`app_server()`](app_server.md) : Server logic for the imongr app
- [`app_ui()`](app_ui.md) : Client (ui) for the imongr app
- [`indicator_ui()`](mod_indicator.md)
  [`indicator_server()`](mod_indicator.md)
  [`indicator_app()`](mod_indicator.md) : Shiny module providing GUI and
  server logic for the indicator tab
- [`publish_ui()`](mod_publish.md) [`publish_server()`](mod_publish.md)
  [`publish_app()`](mod_publish.md) : Shiny module providing GUI and
  server logic for the publish indicator tab
- [`upload_ui()`](mod_upload.md) [`upload_server()`](mod_upload.md)
  [`upload_app()`](mod_upload.md) : Shiny module providing GUI and
  server logic for the upload data tab
- [`download_ui()`](mod_download.md)
  [`download_server()`](mod_download.md)
  [`download_app()`](mod_download.md) : Shiny module providing GUI and
  server logic for the download data tab
- [`review_ui()`](mod_review.md) [`get_last_year()`](mod_review.md)
  [`update_graph_data()`](mod_review.md)
  [`toggle_button()`](mod_review.md)
  [`render_checkboxes()`](mod_review.md)
  [`on_update_form()`](mod_review.md) [`review_server()`](mod_review.md)
  [`review_app()`](mod_review.md) : Shiny module providing GUI and
  server logic for the expert group tab
- [`add_arrows()`](mod_status.md) [`status_ui()`](mod_status.md)
  [`status_server()`](mod_status.md) [`status_app()`](mod_status.md) :
  Shiny module providing UI and server functions for registry status
  overview
- [`project_ui()`](mod_project.md) [`project_server()`](mod_project.md)
  : Shiny module providing GUI and server logic for the indicator tab
- [`selected_indicators_ui()`](mod_selected_indicators.md)
  [`selected_indicators_server()`](mod_selected_indicators.md) : Shiny
  module providing GUI and server logic for the selected indicators tab
- [`publication_ui()`](mod_publication.md)
  [`publication_server()`](mod_publication.md) : Shiny module providing
  GUI and server logic for the publication tab

## Data

Package data, mosty for testing

- [`data`](data.md) : Example data for qmongr data store
- [`delivery`](delivery.md) : Delivery example data for qmongr data
  store
- [`hf`](hf.md) : Organization structure example data
- [`hospital`](hospital.md) : Organization structure example data
- [`ind`](ind.md) : Indicator example data for qmongr data store
- [`nation`](nation.md) : Organization structure example data
- [`org`](org.md) : Organization structure example data
- [`orgnr`](orgnr.md) : Organization structure example data
- [`orgnr_shortname`](orgnr_shortname.md) : Organization structure
  short_name example data
- [`registry`](registry.md) : Registry example data for qmongr data
  store
- [`rhf`](rhf.md) : Organization structure example data
- [`user`](user.md) : User example data for qmongr data store
- [`user_registry`](user_registry.md) : User_registry example data for
  qmongr data store
- [`medfield`](medfield.md) : medfield example data for qmongr data
  store
- [`registry_medfield`](registry_medfield.md) : registry_medfield
  example data for qmongr data store
- [`publish`](publish.md) : publish example data for qmongr data store

## To be added

New items yet to be structured…

- [`.registry_status_data()`](report.md)
  [`registry_status_report()`](report.md) : Functions providing reports
  to be used in imongr
- [`report_ui()`](mod_report.md) [`report_server()`](mod_report.md)
  [`report_app()`](mod_report.md) : Shiny module providing GUI and
  server logic for reports
- [`make_pool()`](db.md) [`drain_pool()`](db.md)
  [`insert_table()`](db.md) [`get_table()`](db.md)
  [`get_table_raw()`](db.md) : Low level database functions for imongr
