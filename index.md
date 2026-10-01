# imongr

Primarily a tool to update data used by the
[mongts](https://github.com/mong/mongts/) application.

## Development

The easiest way to develop `imongr` is to fire up the
`docker-compose.yml` file:

``` sh
docker compose up
```

This file consist of seven different services: - three mariadb databases
(`prod` at port `3331`, `verify` at port `3332`, and `qa` at port
`3333`) - Adminer, a tool for database management, at port
[8888](http://localhost:8888/) - RStudio at port
[8787](http://localhost:8787/) - Code-server (vscode) at port
[8080](http://localhost:8080/) - The app, based on the
`hnskde/imongr:latest` image, at port [3838](http://localhost:3838/)

Open [localhost:8787](http://localhost:8787/) with your favorite browser
and login with `rstudio` and `password`. Go into the `imongr` folder,
open `imongr.Rproj`, and press **Yes** to *Do you want to open the
project ~/imongr?*. Start coding.

Populate the databases by using Adminer, either through `imongr`
(*Administrative verktøy* - *Adminer*) or through [port
8888](http://localhost:8888/). There are two databases: `db` and
`db-verify`. The username and password are both “imongr” for both
databases. You will need a copy of the database as a compressed sql
file. Open the imongr database and click the `import` button. Select the
file containing the database and click `Execute`. This must be done for
the production and verify databases separately.

### Getting out of some dirty states

If the environment variables have been changed an you want to change
them back to default:

``` r

readRenviron("~/.Renviron")
```

If your pool have not been closed properly, for instance if you have not
cleaned up after testing with database:

1.  Go into Adminer on port
    [8888](http://localhost:8888/?server=db&username=imongr), and click
    on `db` in the breadcrumb.
2.  Click *Refresh* next to *Database* in the database table and delete
    all unwanted databases.
3.  Look at *Process list* and kill unwanted processes.
4.  Go into *RStudio* on port [8787](http://localhost:8787/) and restart
    `R` (Ctrl-Shift-F10).

## Ethics

Please note that the ‘imongr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
