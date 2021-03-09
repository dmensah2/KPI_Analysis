#Objective: Create interactive web map of revenue KPI by county

#read in packages
library(sp);library(sf);library(ggplot2)
library(classInt);library(leaflet);library(usethis)
library(ggpubr);library(devtools);library(spatialEco)
library(shinyWidgets);library(shinythemes)
library(ggpubr);library(shiny);library(scales)


#------DATA PREPARATION------#

#read in csv
revenue_csv <- readr::read_csv("Revenue.csv")

#read in county shapefile
county_sf <- sf::st_read("usa_county.shp") %>%
  #transform crs of shapefile
  sf::st_transform(4326)

#test for missing values in lat and long columns
any(is.na(revenue_csv$lat))
any(is.na(revenue_csv$long))

#filter out na values and convert to sf object
revenue_sf <- revenue_csv %>% 
  tidyr::drop_na() %>% 
  st_as_sf(coords=c("long", "lat")) 

#set crs
st_crs(revenue_sf) <- 4326

#convert date column to month
revenue_sf$Month_Yr <- format(as.Date(revenue_sf$cohort), "%B-%Y")

#spatial join of listings to county file
listings_by_county <- county_sf %>%
  #spatial join
  sf::st_join(revenue_sf) %>%
  #aggregate on county column
  dplyr::group_by(NAME, Month_Yr, category2__c, cohort) %>%
  #sum revenue and listings by county
  dplyr::summarise(total_revenue = sum(total), listing_count = dplyr::n()) %>%
  #split name column to county and state
  tidyr::separate(NAME, c("County", "State"), sep=", ")

#aggregate category column
listings_by_county_all <- listings_by_county %>%
  dplyr::group_by(County, State, Month_Yr, cohort) %>%
  dplyr::summarise(total_revenue = sum(total_revenue), listing_count = sum(listing_count))

#add aggregate column
listings_by_county_all$category2__c <- "All"

#format revenue to currency
listings_by_county$total_revenue_text <- paste("$", formatC(listings_by_county$total_revenue, big.mark = ',', digits= 0, format = "f"))
listings_by_county_all$total_revenue_text <- paste("$", formatC(listings_by_county_all$total_revenue, big.mark = ',', digits= 0, format = "f"))

#convert to sp object so it can work with everything (sp is more recent than sf)
listings_by_county <- sf::as_Spatial(listings_by_county, cast = TRUE)
listings_by_county <- listings_by_county[!is.na(listings_by_county$cohort),]
listings_by_county_all <- sf::as_Spatial(listings_by_county_all, cast = TRUE)
listings_by_county_all <- listings_by_county_all[!is.na(listings_by_county_all$cohort),]

#bind the dataframe unaggregated by category and aggregated by category into single dataframe
listings_by_county <- base::rbind(listings_by_county, listings_by_county_all)

#random number generator to add a buffer
randNum <- runif(22421)/1000
listings_by_county$total_revenue <- listings_by_county$total_revenue + randNum

#write out to shapefile
writeOGR(obj=listings_by_county, dsn=".", layer="listings_by_county", driver="ESRI Shapefile")
