
library(shiny)
library(tidyverse)
library(magrittr)
library(bslib)
library(DT)
library(scales)
library(thematic)
library(broom)
library(tidycensus)
library(keyring)
library(sf) 
library(tigris) 
options(tigris_use_cache = TRUE)

library(viridis) 


#Business Logic

AirQuality_data <- readRDS("./data-raw/Air_Quality_History.rds")


AirQuality_data|>
  mutate(SITE_NAME = case_match(SITE_NUM, 41 ~ "River_Terrace_NE", 43 ~ "McMillan_NW", 50 ~ "Takoma_Recreation_NW",
                                51 ~ "Anacostia_Freeway_NE", 53 ~ "Greenleaf_Recreation_SW", 42 ~ "Hains_Point_SW")) ->
  AirQuality_data

AirQuality_data|>
  mutate(Long = case_match(SITE_NUM, 53 ~ 38.87495, 41 ~ 38.8956675, 50 ~ 38.9753882, 43 ~ 38.92501, 51 ~ 38.866924)) ->
  AirQuality_data

AirQuality_data|>
  mutate(Lat = case_match(SITE_NUM, 53 ~ -77.01333, 41 ~ -76.9574747, 50 ~ -77.0227545, 43 ~ -77.01665, 51 ~ -76.994545))|>
  select(-c(LONGITUDE, LATITUDE, UNCERTAINTY)) ->
  AirQuality_data


AirQuality_data|>
  filter(SITE_NUM != 51) ->
  AirQuality_data
  

desired_vars <- c(Median_Income = "B19013_001E", Total_Pop = "B02001_001E", White = "B02001_002E",
                  Black = "B02001_003E", American_Indian_Alaskan_Native = "B02001_004E", Asian = "B02001_005E",
                  Native_Hawiian_Pacific_Islander = "B02001_006E", Other = "B02001_007E", Two_or_More = "B02001_008E",
                  Pop_25_andOver = "B15003_001E", High_School_grad = "B15003_017E", Some_College = "B15003_018E",
                  Bachelors = "B15003_022E", Associates = "B15003_021E", Masters = "B15003_023E", Professional = "B15003_024E",
                  Doctorate = "B15003_025E", Labor_Force = "B23025_001E", Unemployed = "B23025_005E")




get_acs(geography = "tract", state = "DC", county = "District of Columbia", variables = desired_vars, key = key_get("API_KEY_CENSUS"),
        geometry = TRUE, cache_table = TRUE, output = "wide") -> dc_data




fastDummies::dummy_cols(.data=AirQuality_data, select_columns = c("PARAMETER_NAME", "SITE_NAME", "METHOD_TYPE")) ->
  AirQuality_data

#In order to be able to use these columns for multiple linear regression they had to be one-hot encoded

AirQuality_data_sf <- st_as_sf(AirQuality_data, coords = c("Long", "Lat"), crs = 4326)

crs_air_quality <- st_crs(AirQuality_data_sf)

crs_dc_data <- st_crs(dc_data)




if (crs_air_quality != crs_dc_data) {
  AirQuality_data_sf <- st_transform(AirQuality_data_sf, crs_dc_data)
}


AirQuality_data_sf_buffered <- st_buffer(AirQuality_data_sf, dist = 200)

combined_data <- st_join(AirQuality_data_sf_buffered, dc_data)


ui <- fluidPage(

    theme = bslib::bs_theme(version = 5, bootswatch = "solar"),
    titlePanel("DC Air Quality Data"),
    
    tabsetPanel(
      
    )
    
   


)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
