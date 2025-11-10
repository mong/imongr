# Shiny module providing GUI and server logic for the expert group tab

Shiny module providing GUI and server logic for the expert group tab

## Usage

``` r
review_ui(id)

get_last_year()

update_graph_data(input, pool, rv)

toggle_button(input, session, rv, event, requirements)

render_checkboxes(input, output, df_requirements, ns, id_numbers)

on_update_form(
  session,
  input,
  pool,
  n_requirements,
  fetch_previous_year = FALSE
)

review_server(id, registry_tracker, pool)

review_app(pool)
```
