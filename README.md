# weather

Forecast for selected climbing locations near(?) Ithaca, NY. Data are pulled from the Open-meteo Free Weather API. 

# Using Shiny

The Shiny application has been deployed to [Shinyapps.io](https://baruuum.shinyapps.io). 

To open the Shiny application directly, you might use
```bash
R -e 'renv::restore(); library(shiny); runApp(".")'
```
in the cloned directory or, alternatively,
```bash
R -e 'shiny::runGitHub("weather", "baruuum")'
```
without cloning the directory. The second method requires you to have the following `R` packages installed: `shiny`, `bslib`, `data.table`, `ggplot2`, `cowplot`, `openmeteo`.


# If nothing else works...

You can build a `docker` image from the shared `Dockerfile`. This will take, however, quite some time (and, unfortunately, I've never shared the image)


Enjoy!