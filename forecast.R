#!/usr/bin/env Rscript

#### ----------------------------------------------------------------
#### GET WEATHER FORECAST FOR CLIMBING DESTINATIONS NEAR ITHACA
####
#### Author: Barum Park
####
#### ----------------------------------------------------------------

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
        default = "tfpr",
        type    = "character",
        metavar = "STRING",
        help    = "Statistics to curls: t = temperature, f = apparent temperature, p = precipitation, r = precipitation prob., h = relative humidity"
    ),
    make_option(
        c("-o", "--output"),
        action  = "store",
        default = "forecast.pdf forecast.png",
        type    = "character",
        metavar = "STRING",
        help    = "Output file. Only *.pdf and *.png allowed"
    ),
    make_option(
        c("-d", "--days"),
        action  = "store",
        default = 7,
        type    = "integer",
        metavar = "NUMBER",
        help    = "Number of days ahead to forecast"
    )
)



## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("data.table")
library("ggplot2")
library("ggtext")
library("cowplot")

# get options
# opts = parse_args(OptionParser(option_list = option_list), args = c("-v", "-o", "forecast.pdf forecast.png", "-d", 10, "-s", "tp"))
opts = parse_args(OptionParser(option_list = option_list))

verbose = opts$verbose
days = as.integer(opts$days)
stats = sapply(
    strsplit(gsub(" ", "", opts$stats), "")[[1]],
    function(x) {
        switch(
            x,
            t = "temperature_2m",
            f = "apparent_temperature",
            r = "precipitation_probability",
            p = "precipitation",
            h = "relative_humidity_2m"
        )
    }
)
outfiles = strsplit(opts$output, " ")[[1]]

if (!all(grepl("(\\.pdf$|\\.png$)", outfiles)))
    stop("Output must have either a pdf extension or png extension (i.e., filename.pdf or filename.png)")



## ------------------------------------------------------------------
## Get data
## ------------------------------------------------------------------

if (verbose)
    logger::log_info("Getting coordinates ...")

# get data
coordinates = fread(file.path("data", "coordinates.csv"))

# coordinates
loc = as.matrix(coordinates[, c("Latitude", "Longitude")])
# name of place
places = coordinates$Location
# number of places
n = length(places)
# today's date
today = as.Date(Sys.Date())


if (verbose)
    logger::log_info("Getting forecast data from Open-Meteo ...")

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
    logger::log_info("Formatting data ...")

# format data
data = rbindlist(res)
# setnames(
#     data,
#     c("datetime", "temperature", "feelslike", "pprob", "percip", "humidity", "loc")
# )
data = melt(data, id.vars = c("datetime", "location")) |> suppressWarnings()

# factor to differentiate between temperatures
if ("apparent_temperature"  %in% stats) {

    data[, feelslike := factor(
        ifelse(variable == "hourly_apparent_temperature", TRUE, FALSE),
        levels = c(FALSE, TRUE)
        )
    ]

    data[variable == "hourly_apparent_temperature", variable := "hourly_temperature_2m"]

}

# create factors for plotting order
data[, variable_fact := factor(
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
pord = order(places)
coords = apply(loc, 1L, function(w) paste0(format(w, digits = 4), collapse = ", "))
pwc = paste0("Coordinates: (", coords, " ) Distance from Ithaca : ", coordinates$Drive)

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
    logger::log_info("Generating plots ...")

# make plots
plot_list = lapply(places[pord], function(l) {

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
        scale_alpha_manual(values = c(1, .6)) +
        scale_color_manual(values = c("black", "darkmagenta")) +
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

        if ("apparent_temperature" %in% stats) {

            g = g + geom_line(aes(alpha = feelslike, color = feelslike), show.legend = FALSE)

        } else {

            g = g + geom_line()

        }

        if (days > 8) {

            g = g + scale_x_datetime(breaks = "1 week", minor_breaks = "1 day")

        } else {

            g = g + scale_x_datetime(breaks = "2 days", minor_breaks = "1 day")

        }

        g = add_sub(g, label = pwc[which(places == l)], size = 12, x = .175, y = 1, fontface = "italic", color = "#3b3b3b")

        return(g)

    }

)

if (verbose)
    logger::log_info("Saving output...")

for (o in outfiles) {

    if (grepl("\\.pdf$", o)) {
        pdf(o, width = 12, height = n * 2.5)
    } else {
        png(o, width = 12, height = n * 2.5, unit = "in", res = 150)
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
    logger::log_info("Done!")

### EOF ###