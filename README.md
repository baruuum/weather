# weather

Forecast for selected climbing locations near(?) Ithaca, NY. Data are pulled from the Open-meteo Free Weather API. 

# Example

All available options can be found by running
```bash
> Rscript forecast.R --help
Usage: forecast.R [options]


Options:
        -v, --verbose
                Print extra output [default]

        -s STRING, --stats=STRING
                Statistics to curls: t = temperature, f = apparent temperature, p = precipitation, r = precipitation prob., h = relative humidity, d = dew point

        -p STRING, --places=STRING
                Places to track: names of places to track, separated by a comma. Defaults to Bodine, Gunks, Ithaca, Nine Corners, Snowy Mountains, Sunfish Pond.

        -o STRING, --output=STRING
                Output file. Only *.pdf and *.png allowed. Files should be separated by a comma.

        -d NUMBER, --days=NUMBER
                Number of days ahead to forecast

        -t, --time
                Print coordinates and drive time to destination [defaults to FALSE]

        -h, --help
                Show this help message and exit
```
For example, running
```bash
Rscript forecast.R -d 2 -s tp -o foo.pdf
```
will give you the forecast for the temperature and precipitation for the next two days and save it into the file `foo.pdf`.

- All forecasts are shown with black lines. The only exceptions are:
        - the apparent("feels like") temperature, displayed in purple 
        - the dew point, displayed in teal
- Weekends are shaded in blue
- Temperature and dew point are measured in Celsius degrees and precipitation in mm/hr.
- Locations can be customized by changing the `./data/coordinates.csv` file, which has four columns
    - Location (Name)
    - Latitude
    - Longitude
    - Drive (hrs drive from Ithaca)

An example plot is shown below

<p align="center">
<img src="images/forecast.png" width="700" />
</p>

Enjoy!