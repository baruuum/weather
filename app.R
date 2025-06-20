# app.R

library("shiny")
library("bslib")
library("data.table")
library("ggplot2")



## ------------------------------------------------------------------
## Setup renv and packages
## ------------------------------------------------------------------

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# if (!requireNamespace("renv", quietly = TRUE)) 
#     install.packages("renv", repos = "cran.rstudio.com")

# if (!renv::status()$synchronized)
#     renv::restore()



## ------------------------------------------------------------------
## Get coordinates and forcasts
## ------------------------------------------------------------------

# get data
dat = fread(file.path("data", "coordinates.csv"))[order(Location)]

# locations
app_locs = dat$Location
app_loc_list = as.list(seq_along(app_locs))
names(app_loc_list) = app_locs
app_loc_sel = which(app_locs %in% grep("Gunk|Itha|Sunfi|Snowy", app_locs, value = TRUE))

# stats
app_stats = c("Temperature", "Feels Like", "Dew Point", "Precip. Prob", "Precipitation", "Humidity")
app_stats_list = as.list(seq_along(app_stats))
names(app_stats_list) = app_stats
app_stats_sel = c(1, 3, 4, 5)

# create mapping of stats
app_stats_map = app_stats
names(app_stats_map) = c("temperature_2m", "apparent_temperature", "dew_point_2m", "precipitation_probability", "precipitation", "relative_humidity_2m")

# coordinates
loc = as.matrix(dat[, c("Latitude", "Longitude")])
places = dat$Location

# number of places
n = nrow(dat)

# today's date
today = as.Date(Sys.Date())

# get forecast
res = lapply(
    seq_along(places),
    function(w) {
        ping_res = openmeteo::weather_forecast(
            location = loc[w, ],
            start = today,
            end = today + 14,
            hourly = names(app_stats_map),
            timezone = "America/New_York"
        )
        ping_res$location = places[w]
        setDT(ping_res)
        return(ping_res)
    }
)

# format data
data = rbindlist(res) |> 
    melt(id.vars = c("datetime", "location")) |> 
    suppressWarnings()



## ------------------------------------------------------------------
## Define UI
## ------------------------------------------------------------------

ui = page_sidebar(
    title = tags$h1("Weather Forecast"),
    sidebar = sidebar(
        card(
            card_header("Locations"),
            checkboxGroupInput(
                "loc",
                NULL,
                choices  = app_loc_list,
                selected = app_loc_sel
            )
        ),
        card(
            card_header("Date"),
            dateRangeInput(
                "dates",
                NULL,
                start = today,
                end   = today + 7,
                min   = today,
                max   = today + 14
            )
        ),
        card(
            card_header("Statistics"),
            checkboxGroupInput(
                "stats",
                NULL,
                choices  = app_stats_list,
                selected = app_stats_sel
            )
        )
    ),
    plotOutput(outputId = "forecast"),
    tags$div(style="font-size:75%;padding:5px;text-align:right", tags$small("Forecast data are pulled from ", tags$a(href = "https://open-meteo.com/", "Open-meteo's Free Weather API."))),
    theme = bs_theme(preset = "sandstone")
)


# input = list()
# input$stats = c("1", "3", "5")
# input$loc = c("1", "4")
# input$dates = c(Sys.Date(), Sys.Date() + 5)

## ------------------------------------------------------------------
## Define server logic
## ------------------------------------------------------------------

server = function(input, output) {

    # bs_themer()
    output$forecast = renderPlot({
        
        withProgress(message = "Working on it ...", value = 0, {

            # select dates
            sel_dates = input$dates
            data[, date := as.Date(as.character(datetime))]
            data = data[date >= sel_dates[1] & date <= sel_dates[2]]
            tot_days = as.integer(sel_dates[2] - sel_dates[1] + 1)

            # select places
            sel_loc = names(app_loc_list)[as.integer(input$loc)]
            data = data[location %in% sel_loc]

            # select variables
            sel_stats = app_stats_map[as.integer(input$stats)]
            data = data[variable %in% paste0("hourly_", names(sel_stats))]

            # make data for shading weekends
            dayname = weekdays(data$datetime, abbr = TRUE)
            sat_data = data[grepl("Sat", dayname), ]
            sat_data = sat_data[, list(m = min(datetime)+1, M = max(datetime) + 24 * 60 * 62), by = "date"]

            if (weekdays(today, abbr = TRUE) == "Sun")
                sat_data = rbind(
                    data.table(
                        date = today,
                        m = as.POSIXct(today, tz = "") + 1,
                        M = as.POSIXct(today, tz = "") + 24 * 60 * 62
                    ),
                    sat_data
                )


            # factor to differentiate between temperatures
            if ("Feels Like" %in% sel_stats) {

                data[, feelslike := factor(
                    ifelse(variable == "hourly_apparent_temperature", TRUE, FALSE),
                    levels = c(FALSE, TRUE)
                    )
                ]

                data[variable == "hourly_apparent_temperature", variable := "hourly_temperature_2m"]

            }

            # factor to differentiate between temperatures and dew point
            if ("Dew Point" %in% sel_stats) {

                data[, dew_point := factor(
                    ifelse(variable == "hourly_dew_point_2m", TRUE, FALSE),
                    levels = c(FALSE, TRUE)
                    )
                ]

                data[variable == "hourly_dew_point_2m", variable := "hourly_temperature_2m"]

            }

            # create factors for plotting order
            data[
                , variable_fact := factor(
                    variable,
                    levels = c("hourly_temperature_2m", "hourly_precipitation_probability", "hourly_precipitation", "hourly_relative_humidity_2m"),
                    labels = c("Temperature (°C)", "Precip. Prob", "Precipitation (mm)", "Relative Humidity")
                )
            ]

            # factorize locations
            data[, loc_fact := factor(
                location,
                levels = sort(sel_loc),
                labels = sort(sel_loc)
            )]

            # generate limits for plots
            plims = transpose(data[, list(m = min(value), M = max(value)), by = "variable"], make.names = 1L)
            plims[, datetime := c(min(data$datetime), max(data$datetime))]

            # enforce limits on percipitation probability
            if ("Precip. Prob" %in% sel_stats)
                plims[, hourly_precipitation_probability := c(0, 100)]

            plims = melt(plims, id.vars = "datetime")
            plims[, variable_fact := factor(
                    variable,
                    levels = c("hourly_temperature_2m", "hourly_precipitation_probability", "hourly_precipitation", "hourly_relative_humidity_2m"),
                    labels = c("Temperature (°C)", "Precip. Prob", "Precipitation (mm)", "Relative Humidity")
                )
            ]

            # create benchmarking lines
            hline_dat = data.table(
                value = numeric(), variable = character()
            )

            if ("Feels Like" %in% sel_stats | "Temperature" %in% sel_stats)
                hline_dat = rbind(
                    hline_dat,
                    data.table(
                        value = 0,
                        variable = "hourly_temperature_2m"
                    )
                )

            if ("Precip. Prob" %in% sel_stats)
                hline_dat = rbind(
                    hline_dat,
                    data.table(
                        value = 50,
                        variable = "hourly_precipitation_probability"
                    )
                )

            if (NROW(hline_dat) > 0) 
                hline_dat[, variable_fact := factor(
                    variable,
                    levels = c("hourly_temperature_2m", "hourly_precipitation_probability", "hourly_precipitation", "hourly_relative_humidity_2m"),
                    labels = c("Temperature (°C)", "Precip. Prob", "Precipitation (mm)", "Relative Humidity")
                )]

            # make plots
            plot_list = lapply(
                sel_loc,
                function(l) {

                    g = ggplot(
                            data[loc_fact == l],
                            aes(x = datetime, y = value)
                        ) + 
                        geom_hline(
                            data = hline_dat,
                            aes(yintercept = value),
                            col = "darkgrey",
                            linetype = 2,
                            linewidth = .5,
                            alpha = .8
                        ) + 
                        geom_blank(data = plims) +
                        geom_rect(
                            data = sat_data,
                            aes(xmin = m, xmax = M, ymin = -Inf, ymax = Inf),
                            inherit.aes = FALSE,
                            fill = "steelblue",
                            alpha = .25
                        ) +
                        labs(x = "Date/Time", y = "") +
                        facet_wrap(
                            ~ variable_fact,
                            scales = "free_y",
                            ncol = 4
                        ) +
                        theme_bw() +
                        theme(
                            axis.text.x = element_text(size = 8),
                            strip.background.x = element_rect(fill = "transparent", color = "white"),
                            strip.background.y = element_rect(fill = "transparent"),
                            strip.text.x = element_text(hjust = 0, size = 12),
                            strip.text.y = element_text(hjust = 0, size = 15),
                            plot.margin = margin(t = 25,  r = 2, b = 0, l = 2)
                        )

                    g = if (l == sel_loc[length(sel_loc)]) {
                        
                        g + theme(
                            legend.position = "bottom",
                            legend.location = "plot",
                            legend.justification.bottom = "left",
                            legend.direction = "horizontal",
                            legend.box.spacing = unit(-5, "pt")
                        )

                    } else {

                        g + theme(legend.position = "none")

                    }

                    g = if ("Feels Like" %in% sel_stats && "Dew Point" %in% sel_stats) {

                        g + geom_line(
                                aes(
                                    alpha = interaction(feelslike, dew_point),
                                    color = interaction(feelslike, dew_point),
                                    linewidth = interaction(feelslike, dew_point)
                                ), 
                                show.legend = TRUE
                            ) +
                            scale_color_manual(name = "", labels = c("Temperature", "Feels Like", "Dew Point"), values = c("black", "darkmagenta", "#2e8c86")) +
                            scale_alpha_manual(name = "", labels = c("Temperature", "Feels Like", "Dew Point"), values = c(1, .6, .6)) + 
                            scale_linewidth_manual(name = "", labels = c("Temperature", "Feels Like", "Dew Point"), values = c(.5, .4, .4))

                    } else if ("Feels Like" %in% sel_stats) {

                        g + geom_line(aes(alpha = feelslike, color = feelslike), show.legend = TRUE) +
                            scale_color_manual(name = "", labels = c("Temperature", "Feels Like"), values = c("black", "darkmagenta")) +
                            scale_alpha_manual(name = "", labels = c("Temperature", "Feels Like"), values = c(1, .6))

                    } else if ("Dew Point" %in% sel_stats) {

                        g + geom_line(aes(alpha = dew_point, color = dew_point), show.legend = TRUE) +
                            scale_color_manual(name = "", labels = c("Temperature", "Dew Point"), values = c("black", "#2e8c86")) +
                            scale_alpha_manual(name = "", labels = c("Temperature", "Dew Point"), values = c(1, .6))

                    } else {

                        g + geom_line()

                    }

                    g = if (tot_days > 8) {

                        g + scale_x_datetime(breaks = "1 week", minor_breaks = "1 day")

                    } else {

                        g + scale_x_datetime(breaks = "2 days", minor_breaks = "1 day")

                    }

                    return(g)

                }

            )

            cowplot::plot_grid(
                plotlist = plot_list,
                nrow = length(input$loc),
                ncol = 1,
                labels = places,
                label_size = 15,
                label_fontface = "bold",
                vjust = 2,
                hjust = -.1
            )

        }) # withProgress

    }) #renderPlot

}

# Run the app ----
shinyApp(ui = ui, server = server)