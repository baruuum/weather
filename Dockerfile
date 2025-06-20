# Dockerfile

# base Shiny image
FROM rocker/shiny-verse:4.5.0

# make home dir
RUN mkdir /home/shiny-app

# install Renv
RUN R -e 'install.packages("renv", repos = c(CRAN = "https://cran.rstudio.com"))'

# make app working directory
WORKDIR /home/shiny-app

# copy over files
RUN mkdir -p renv
COPY app.R app.R
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY data/coordinates.csv data/coordinates.csv

# restore R environment
RUN R -e 'renv::restore()'

# expose port
EXPOSE 8180

# run the Shiny app
CMD Rscript /home/shiny-app/app.R

### EOF ###