## Charlottesville 2W daily weather history bulk download

This file (`data/GHCN_USC00441593.csv`) contains daily weather reports from McCormick Observatory (GHCND:USC00441593). The start date is Jan 1, 1893.

The data fields are:

* PRCP - precipitation (inches)
* SNOW - snowfall (inches)
* SNWD - snow depth (inches)
* TMAX - maximum temperature (degrees Fahrenheit)
* TMIN - minimum temperature

Detailed field definitions are in [this NOAA documentation file](ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt).

This file is created by running `R/Retrieve_GHCN_USC00441593.R`. That script downloads the latest data, unzips it, filters for the desired statistics, converts those values to the desired units, formats date columns, and converts the data from long to wide format.
