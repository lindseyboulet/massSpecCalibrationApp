# Mass Spectrometer Calibration Application 

This app accepts data (.lvm) from a matlab module that pulls raw data from a Hiden mass spectrometer with a MIMS probe attached.  The app allows you to refresh data so live data can be streamed into the app in real time. It allows you to then select data points to average and produces 3 calibration plots on the second tab (O2, CO2, N2). Finally it allows the user to generate a calibration report in markdown format.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

These r libraries are required

```
c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown", "lubridate", "chron", "ggthemes", "knitr")
```

### Installing

Install the libraries above and load data into \data folder

## Authors

* **Lindsey Boulet** 

## License

This project is licensed under the MIT License - see the [license.md](license.md) file for details

## Acknowledgments

* CPLEAP

