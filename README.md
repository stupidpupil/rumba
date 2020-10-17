# Rumba

Rumba is an R Shiny app for managing other R Shiny apps behind a Microsoft IIS instance. 

For each R Shiny app it handles:

1) Starting and stopping a pool of local R workers.
2) Defining and destroying an IIS *Web Farm* containing those R workers.
3) Creating and removing an IIS *Application Request Routing* rewrite rule for a site to point a path at that web farm.

## Requirements

- R and certain packages (tested with R 4.0)
- Microsoft IIS 7 or later (tested with IIS 10.0)
- IIS [Application Request Routing](https://www.iis.net/downloads/microsoft/application-request-routing) 3.0 or later

## rumba.yml - per app config

Each app directory must contain a *rumba.yml* file that, at a minimum, defines a webPath.

```yaml
# an example rumba.yml file
webPath: my_cool_app

```

| Setting     | Example     | Default | Description                                                                                                                                                                                                       |
|-------------|-------------|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| webPath     | my_cool_app |         | The URL path that will be used for the app. This example might route "https://example.com/my_cool_app" to the R app. Must be set. Can only include lowercase letters (a-z), numbers (0-9) and underscores (\_). |
| workerCount | 5           | 2       | The number of R workers to start for this app. Must be between 1 and 9.                                                                                                                                           |
| basePort    | 5001        | Dynamic | The start of local port range that the R workers will use. Must be in 5001-6999. If you don't explicitly set a port, then your application will be allocated a range in 7001-12001                                                  |


## config.yml - Rumba config

| Setting                  | Default                                                   | Description                                                                                                          |
|--------------------------|-----------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| iisApplicationHostConfig | C:/Windows/system32/inetsrv/config/applicationHost.config | The IIS configuration file that Rumba will try to define Web Farms in.                                               |
| iisSitePath              | C:/inetpub/wwwroot/                                       | The physical path that Rumba will try to create sub-directories, containing web.config files with rewrite rules, in.                         |
| appsDir                  | apps                                                      | Where Rumba will look for app directories. Each app directory must contain a valid rumba.yml file, as described above. |


## Alternatives

Assuming you're looking for some way to easily configure and run a bunch of R Shiny apps on your own webserver, with some control over scaling:

- If you're able to setup Docker and Java, then [ShinyProxy](https://www.shinyproxy.io/).

- If you have at least $15,000 then [RStudio Connect](https://rstudio.com/pricing/#connect).
