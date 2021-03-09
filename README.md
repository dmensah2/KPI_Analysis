# KPI_Analysis
R shiny apps integrated with leaflet package to visualize KPIs of cannabis landscape.

### This shiny app was designed to leverage internal metrics to visualize KPI of cannabis landscape to optimize revenue potential. ######

The scripts are divied into 2 separate pieces - data processing and app. </br>

## Data Processing </br>
This script preprocesses a large shapefile that will eventually be consumed by your shiny app. This is done this way to reduce load times on the shiny app.

## App</br>
This is the app.r file that shinyapps.io will use to visualize and host your web app. It's important to minimize the extra unnecessary data preprocessing to reduce load times.
