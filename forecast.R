# get_forecast.R

library("data.table")
library("ggplot2")
library("ggtext")
library("cowplot")

# get data
logger::log_info("Getting coordinates ...")
coordinates = fread(file.path("data", "coordinates.csv"))

# coordinates
loc = as.matrix(coordinates[, c("Latitude", "Longitude")])
# name of place
places = coordinates$Location
# number of places
n = length(places)
# today's date
today = as.Date(Sys.Date())

# ping API
logger::log_info("Getting Forecast data from Open-Meteo ...")
res = lapply(
    seq_along(places),
    function(w) {
        ping_res = openmeteo::weather_forecast(
            location = loc[w, ],
            start = today,
            end = today + 15,
            hourly = c(
                "temperature_2m",
                "apparent_temperature",
                "precipitation_probability",
                "precipitation",
                "relative_humidity_2m"
            ),
            timezone = Sys.timezone()
        )
        ping_res$location = places[w]
        setDT(ping_res)
        return(ping_res)
    }
)

logger::log_info("Formatting Data ...")

# format data
data = rbindlist(res)
setnames(
    data,
    c("datetime", "temperature", "feelslike", "pprob", "percip", "humidity", "loc")
)
data = melt(data, id.vars = c("datetime", "loc")) |> suppressWarnings()

# factor to differentiate between temperatures
data[, feelslike := factor(
    ifelse(variable == "feelslike", TRUE, FALSE),
    levels = c(FALSE, TRUE)
    )
]
data[variable == "feelslike", variable := "temperature"]

# create factors for plotting order
data[, variable_fact := factor(
        variable,
        levels = c("temperature", "pprob", "percip", "humidity"),
        labels = c("Temperature (C)", "Percip. Prob", "Percipitation (mm)", "Relative Humidity")
    )
]

# make data for shading weekends
dayname = weekdays(data$datetime, abbr = TRUE)
data[, date := as.Date(as.character(datetime))]

sat_data = data[grepl("Sat", dayname), ] 
sat_data = sat_data[, list(m = min(datetime)+1, M = max(datetime) + 24 * 60 * 60), by = "date"]

if (weekdays(today, abbr = TRUE) == "Sun")
    sat_data = rbind(
        data.table(
            date = today,
            m = as.POSIXct(today, tz = "") + 1,
            M = as.POSIXct(today, tz = "") + 24 * 60 * 60
        ),
        sat_data
    )

# add coordinates to locations
pord = order(places)
coords = apply(loc, 1L, function(w) paste0(format(w, digits = 4), collapse = ", "))
# pwc = paste0(places, "<br><span style='font-size:11pt'>(", coords, ")</span>")

pwc = paste0("Coordinates: (", coords, " ) Distance from Ithaca : ", coordinates$Drive)

data[, loc_fact := factor(
    loc,
    levels = places[pord],
    labels = places[pord]
)]

# get limts
plims = transpose(data[, list(m = min(value), M = max(value)), by = "variable"], make.names = 1L)
plims[, datetime := c(min(data$datetime), max(data$datetime))]

# enforce limits on percipitation probability
plims[, pprob := c(0, 100)]
plims = melt(plims, id.vars = "datetime")
plims[, variable_fact := factor(
        variable,
        levels = c("temperature", "pprob", "percip", "humidity"),
        labels = c("Temperature (C)", "Percip. Prob", "Percipitation (mm)", "Relative Humidity")
    )
]

logger::log_info("Generating Plots ...")

# make plots
plot_list = lapply(places[pord], function(l) {
    g = ggplot(
        data[loc_fact == l],
        aes(x = datetime, y = value)
    ) +
        geom_blank(data = plims) + 
        geom_hline(
            data = data.frame(
                loc_fact = rep(l, 2),
                value = rep(c(0, 50), each = n), 
                variable_fact = factor(
                    rep(c("temperature", "pprob"), each = n),
                    levels = c("temperature", "pprob"),
                    labels = c("Temperature (C)", "Percip. Prob")
                ),
                feelslike = FALSE
            ),
            aes(yintercept = value),
            col = "darkgrey",
            linetype = 2,
            linewidth = .5,
            alpha = .8
        ) +
        geom_rect(
            data = sat_data,
            aes(xmin = m, xmax = M, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "steelblue",
            alpha = .25
        ) +
        geom_line(aes(alpha = feelslike, color = feelslike), show.legend = FALSE) +
        scale_x_datetime(breaks = "1 week", minor_breaks = "1 day") +
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

        g = add_sub(g, label = pwc[which(places == l)], size = 12, x = .175, y = 1, fontface = "italic", color = "#3b3b3b")
        g
    }
)

# combine and save
pdf("forecast.pdf", width = 12, height = n * 2.5)
plot_grid(
    plotlist = plot_list,
    nrow = n,
    ncol = 1,
    labels = places[pord],
    label_size = 15,
    label_fontface = "bold",
    vjust = 2,
    hjust = -.1
)
invisible(dev.off())

logger::log_info("Done!")

### EOF ###