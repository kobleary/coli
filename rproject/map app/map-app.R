
library(shiny)
library(ggplot2)
library(mapview)
library(sf)
library(dplyr)
library(stringr)
library(mapview)
library(reshape2)
library(magrittr)
library(leaflet)

# COLI Map Layer
### Bring in Urban Area geometries 
uas <- st_read("/Users/ryankobler/Desktop/Tristan Research/shapefiles/tl_2017_us_uac10/tl_2017_us_uac10.shp")

#coli_input <- paste(coli$URBAN_AREA_NAME, collapse = "|")
# the convention is a space. 

# Use the space naming scheme, as it is easier to back out than adding the comma
# because some city names are more than one word.

ua_input <- paste(uas$NAME10, collapse = "|")
ua_alias <- str_replace_all(string = ua_input, pattern = ", ", replacement = " ")
ua_alias <- strsplit(ua_alias, split = "\\|")
ua_alias <- unlist(ua_alias)

#coli_cities <- coli$URBAN_AREA_NAME
#ua_overlap <- unique(coli_cities[coli_cities %in% ua_alias])
#notInUA <- coli_cities[!coli_cities %in% ua_alias]

# Roughly half of our COLI cities are not mappable onto the UA data set
#length(unique(ua_overlap))
#length(unique(notInUA))

# Save cleaned city names in new columns
uas$city <- ua_alias


library(readxl)
# COLI 60 consistent cities
yrcounts <- read_excel("/Users/ryankobler/Desktop/Tristan Research/data sets/COLI_CITIES_beta.xlsx")
yrcounts %<>%
    rename(city = URBAN_AREA_NAME)

# Find the year indexes, 1990, 1991, etc. The quarterly values lie between each of these years
year_i <- seq(3, ncol(yrcounts), by = 5)
atleastonce <- yrcounts[, year_i] 
atleastonce$city <- yrcounts$city

# bool is set to T if a city was non-missing for at least one quarter of
# a specified year
bool <- atleastonce > 0 # note: this is a matrix
bool <- as.data.frame(bool*1) # transform to data frame of 0 and 1
bool$years <- rowSums(bool) # count the 1s by city (aka by row)
bool$city <- yrcounts$city

# Get cities observed at least once every year--since there are 30 years in the data set, sufficies
# to call all those cities whose rowSums are at least 30.
colicities <- bool %>% 
    filter(years > 30) %>%
    pull(city)

#coli_city_table <- bool %>% filter(years > 30) %>% select(city, years)


# Grab each quarter's coli index value: reshape long->wide
coli$time <- paste0(coli$YEAR, coli$QUARTER)
coli <- coli %>% 
    filter(time != "NANA")

# Create long df of geometries + most popular citynames
coli_long <- st_as_sf(left_join(coli, uas, by = "city")) 
coli_long %<>% filter(city %in% colicities)
# -------------------------------------------------

# CPI Map Layer
### Load and clean CBSA shapefile
# CBSAS
#cbsas <- st_read("/Users/ryankobler/Desktop/Tristan Research/shapefiles/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")

#extractCity <- function(string){
#    unlist(str_split(string, ", "))[1]
#}
#shortnames <- sapply(cbsas$NAMELSAD, extractCity)
#cbsas$city <- shortnames

### Load the CPI and fix spelling errors
# Read the cpi data
#cpi <- "/Users/ryankobler/Desktop/Tristan Research/data sets/CPI Data_Final.xlsx"
#cpi <- read_excel(cpi)
#cpi %<>% 
#    rename(city = City)
#cpi$city <- plyr::revalue(cpi$city, 
#                          c("Cleveland-Akron" = "Cleveland", 
#                            "Dallas-Forth Worth-Arlington" = "Dallas-Fort Worth-Arlington", 
#                            #"Los Angeles-Riverside-Orange County" = "Los Angeles-Long Beach-Anaheim",
#                            "Cincinnati-Hamilton" = "Cincinnati",
#                            "Pheonix-Mesa-Scottsdale" = "Phoenix-Mesa-Scottsdale",
#                            "San Diego-Carsbad" = "San Diego-Carlsbad",
#                            "Urban Hawaii" = "Urban Honolulu",
#                            "Portland-Salem" = "Portland-Vancouver-Hillsboro"))

### Construct map data frame
# Grab the geometries from CBSAs that match to CPI index 
#cpimap2 <- cbsas %>% 
#    filter(city %in% unique(cpi$city))

# Arrange in alphabetical order
#cpimap2 %<>%
#    arrange(NAME)

# Note: Cleveland TN and Cleveland MS are both city names in cpimap2
# remove the micro area, MS with GEOID 17380
#cpimap2 %<>% filter(GEOID != 17380)

# --------- RESHAPE CPI DATA LONG TO WIDE -------------------
# this is to add the CPIu values from 1915 to 2019 to the map 
#c <- cpi %>% 
#    filter(city != "N/A")

# Use DCast to reshape
#cpi_wide <- dcast(c, city ~ Year,
 #                 value.var = "CPIu", 
 #                 drop = FALSE) 

#gh_cpi <- cpi_wide %>%
#    filter(city %in% cpimap2$city)

# Join the CPI yearly data to the sf data frame, cpimap2
#for(n in colnames(gh_cpi)){
#    cpimap2[n] <- gh_cpi[n]
#}

createYearDF_coli <- function(year){
    coli_long %>%
        dplyr::filter(YEAR == year) %>%
        dplyr::group_by(city) %>%
        dplyr::summarise(index = mean(COMPOSITE_INDEX)) %>%
        select(index, city)
}

cpi_long <- st_as_sf(left_join(cpi, cbsas, by = "city"))

createYearDF_cpi <- function(year){
    cpi_long %>%
        dplyr::filter(Year == year) %>%
        select(CPIu, city)
}


ui <- fluidPage(
    titlePanel("Cost of Living and CPI Map"),
    
    fluidRow(
        
        column(4,
               radioButtons("legendInput", "Legend:",
                            choices = c("False", "True"))
        ),
        column(4,
               selectInput("yearInput", "Year:",
                           choices = as.character(unique(coli_long$YEAR)))
        )
       ),
    
    
    #sidebarLayout(
    #    sidebarPanel(
    #        selectInput("yearInput", "Year:",
    #                     choices = as.character(unique(coli_long$YEAR))),
    #        
    #        radioButtons("legendInput", "Legend:",
    #                     choices = c("False", "True"))
    #    ),
    #    mainPanel(
            mapviewOutput("coolmap")
 #       )
#    )
)

server <- function(input, output) {
    
    coli_react <- reactive({
        createYearDF_coli(as.numeric(input$yearInput))
    })
    cpi_react <- reactive({
        createYearDF_cpi(as.numeric(input$yearInput))
    })
    
    leg_react <- reactive({
        as.logical(input$legendInput)
    })
    
    # this reactive may not be necessary
    year <- reactive({
        as.character(input$yearInput)
    })
    
    output$coolmap <- renderLeaflet({
        
        coli_df <- coli_react()
        cpi_df <- cpi_react()
        LEGEND <- leg_react()
        
        bigmap <- mapview(coli_df,
                layer.name = "COLI",
                zcol = "index",
                legend = LEGEND) + 
            mapview(cpi_df, 
                    layer.name = "CPI", 
                    zcol = "CPIu",
                    legend = LEGEND)
        
        # call the map attribute
        bigmap@map
        
        
        })
    
        
}

shinyApp(ui = ui, server = server)