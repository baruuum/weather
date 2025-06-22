#!/usr/bin/env Rscript

#### ----------------------------------------------------------------
#### GET WEATHER FORECAST FOR CLIMBING DESTINATIONS NEAR ITHACA
####
#### Author: Barum Park
####
#### ----------------------------------------------------------------

if (!renv::status()$synchronized)
    renv::restore()
    
## ------------------------------------------------------------------
## Argument parser
## ------------------------------------------------------------------

library("optparse")

option_list = list(
    make_option(
        c("-v", "--verbose"),
        action  = "store_true",
        default = TRUE,
        help    = "Print extra output [default]"
    ),
    make_option(
        c("-s", "--stats"),
        action  = "store",
        default = "tdpr",
        type    = "character",
        metavar = "STRING",
        help    = "Statistics to curls: t = temperature, f = apparent temperature, p = precipitation, r = precipitation prob., h = relative humidity, d = dew point"
    ),
    make_option(
        c("-p", "--places"),
        action = "store",
        default = "ithaca,gunks,bodine,sunfish,nine,snowy",
        type    = "character",
        metavar = "STRING",
        help    = "Places to track: names of places to track, separated by a comma. Defaults to all places in the ~/data/coordinates.csv file."
    ),
    make_option(
        c("-o", "--output"),
        action  = "store",
        default = "forecast.pdf, forecast.png",
        type    = "character",
        metavar = "STRING",
        help    = "Output file. Only *.pdf and *.png allowed. Files should be separated by a comma."
    ),
    make_option(
        c("-d", "--days"),
        action  = "store",
        default = 7,
        type    = "integer",
        metavar = "NUMBER",
        help    = "Number of days ahead to forecast"
    ),
    make_option(
        c("-t", "--time"),
        action  = "store_true",
        default = FALSE,
        help    = "Print coordinates and drive time to destination [defaults to FALSE]"
    )
)



## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("data.table")
library("ggplot2")
library("cowplot")

# helper function to trim strings
str_trim = function(x) { gsub("\\s+", "", trimws(x)) }

# get options
# opts = parse_args(OptionParser(option_list = option_list), args = c("-v", "-o", "forecast.pdf, forecast.png", "-d", 10, "-s", "tfpd", "-p", "gunks, bodine"))
opts = parse_args(OptionParser(option_list = option_list))

# verbose
verbose = opts$verbose

# days
days = as.integer(opts$days)

# statistics
stats = sapply(
    strsplit(gsub(" ", "", opts$stats), "")[[1]],
    function(x) {
        switch(
            x,
            t = "temperature_2m",
            f = "apparent_temperature",
            r = "precipitation_probability",
            p = "precipitation",
            h = "relative_humidity_2m",
            d = "dew_point_2m"
        )
    }
)

# output files
outfiles = str_trim(strsplit(opts$output, ",")[[1]])

if (!all(grepl("(\\.pdf$|\\.png$)", outfiles)))
    stop("Output must have either a pdf extension or png extension (i.e., filename.pdf or filename.png)")

# coordinates and driving time
print_time = opts$time

## ------------------------------------------------------------------
## Get data
## ------------------------------------------------------------------

# get data
coordinates = fread(file.path("data", "coordinates.csv"))

# coordinates
loc = as.matrix(coordinates[, c("Latitude", "Longitude")])

# get places
if (opts$places == "all") {
    
    places = coordinates$Location

} else {

    opts_places = pmatch(
        tolower(str_trim(strsplit(opts$places, ",")[[1]])),
        tolower(coordinates$Location)
    )

    coordinates = coordinates[opts_places, ]
    places = coordinates$Location
    loc = loc[opts_places, , drop = FALSE]

}

if (verbose)
    message("Getting forecasts for ", paste0(places, collapse = ", "), " ...")

# number of places
n = length(places)

# today's date
today = as.Date(Sys.Date())

if (verbose)
    message("Getting forecast data from Open-Meteo ...")

# ping API
res = lapply(
    seq_along(places),
    function(w) {
        ping_res = openmeteo::weather_forecast(
            location = loc[w, ],
            start = today,
            end = today + days,
            hourly = stats,
            timezone = Sys.timezone()
        )
        ping_res$location = places[w]
        setDT(ping_res)
        return(ping_res)
    }
)



## ------------------------------------------------------------------
## Prepare data for plotting
## ------------------------------------------------------------------

if (verbose)
    message("Formatting data ...")

# format data
data = rbindlist(res) |> 
    melt(id.vars = c("datetime", "location")) |> 
    suppressWarnings()

# factor to differentiate between temperatures
if ("apparent_temperature"  %in% stats) {

    data[, feelslike := factor(
        ifelse(variable == "hourly_apparent_temperature", TRUE, FALSE),
        levels = c(FALSE, TRUE)
        )
    ]

    data[variable == "hourly_apparent_temperature", variable := "hourly_temperature_2m"]

}

# factor to differentiate between temperatures and dew point
if ("dew_point_2m" %in% stats) {

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
        labels = c("Temperature (C)", "Precip. Prob", "Precipitation (mm)", "Relative Humidity")
    )
]

# make data for shading weekends
dayname = weekdays(data$datetime, abbr = TRUE)
data[, date := as.Date(as.character(datetime))]

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

# add coordinates to locations
if (print_time) {

    coords = apply(loc, 1L, function(w) paste0(format(w, digits = 4), collapse = ", "))
    pwc = paste0("Coordinates: (", coords, " ) Distance from Ithaca : ", coordinates$Drive)

}

pord = order(places)
data[, loc_fact := factor(
    location,
    levels = places[pord],
    labels = places[pord]
)]

# get limts
plims = transpose(data[, list(m = min(value), M = max(value)), by = "variable"], make.names = 1L)
plims[, datetime := c(min(data$datetime), max(data$datetime))]

# enforce limits on percipitation probability
if ("precipitation_probability" %in% stats)
    plims[, hourly_precipitation_probability := c(0, 100)]

plims = melt(plims, id.vars = "datetime")
plims[, variable_fact := factor(
        variable,
        levels = c("hourly_temperature_2m", "hourly_precipitation_probability", "hourly_precipitation", "hourly_relative_humidity_2m"),
        labels = c("Temperature (C)", "Precip. Prob", "Precipitation (mm)", "Relative Humidity")
    )
]



## ------------------------------------------------------------------
## Make plots
## ------------------------------------------------------------------

hline_dat = data.table(
    value = numeric(), variable = character()
)

if ("apparent_temperature"  %in% stats | "temperature_2m" %in% stats)
    hline_dat = rbind(
        hline_dat,
        data.table(
            value = 0,
            variable = "hourly_temperature_2m"
        )
    )

if ("precipitation_probability" %in% stats)
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
        labels = c("Temperature (C)", "Precip. Prob", "Precipitation (mm)", "Relative Humidity")
    )]

if (verbose)
    message("Generating plots ...")

# make plots
plot_list = lapply(
    places[pord],
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
                plot.margin = margin(t = 25,  r = 2, b = 0, l = 2),
                legend.position = "bottom",
                legend.location = "plot",
                legend.justification.bottom = "left",
                legend.direction = "horizontal",
                legend.box.spacing = unit(-5, "pt")
            )

        if ("apparent_temperature" %in% stats && "dew_point_2m" %in% stats) {

            g = g + 
                geom_line(
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

        } else if ("apparent_temperature" %in% stats) {

            g = g + 
                geom_line(aes(alpha = feelslike, color = feelslike), show.legend = TRUE) +
                scale_color_manual(name = "", labels = c("Temperature", "Feels Like"), values = c("black", "darkmagenta")) +
                scale_alpha_manual(name = "", labels = c("Temperature", "Feels Like"), values = c(1, .6))

        } else if ("dew_point_2m" %in% stats) {

            g = g + 
                geom_line(aes(alpha = dew_point, color = dew_point), show.legend = TRUE) +
                scale_color_manual(name = "", labels = c("Temperature", "Dew Point"), values = c("black", "#2e8c86")) +
                scale_alpha_manual(name = "", labels = c("Temperature", "Dew Point"), values = c(1, .6))

        } else {

            g = g + geom_line()

        }

        if (days > 8) {

            g = g + scale_x_datetime(breaks = "1 week", minor_breaks = "1 day")

        } else {

            g = g + scale_x_datetime(breaks = "2 days", minor_breaks = "1 day")

        }

        if (print_time) 
            g = add_sub(g, label = pwc[which(places == l)], size = 12, fontface = "italic", color = "#3b3b3b") # , x = .175, y = 1

        return(g)

    }

)

if (verbose)
    message("Saving output...")

unique_stats = length(stats)
if (sum(c("apparent_temperature", "dew_point_2m", "temperature_2m") %in% stats) > 1)
    unique_stats = unique_stats - sum(c("apparent_temperature", "dew_point_2m", "temperature_2m") %in% stats) + 1


for (o in outfiles) {

    if (grepl("\\.pdf$", o)) {
        pdf(o, width = unique_stats * 3.5 + 1, height = n * 2.5 + 2)
    } else {
        png(o, width = unique_stats * 3.5 + 1, height = n * 2.5 + 2, unit = "in", res = 200)
    }

    plot_grid(
        plotlist = plot_list,
        nrow = n,
        ncol = 1,
        labels = places[pord],
        label_size = 15,
        label_fontface = "bold",
        vjust = 2,
        hjust = -.1
    ) |> print()

    invisible(dev.off())

}

if (verbose)
    message("Done!")

### EOF ###