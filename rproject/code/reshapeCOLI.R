library(readxl)
library(dplyr)
library(xlsx)

# IMPORT DATA FROM EXCEL FILES ON SHARED DROP BOX
library(readxl)
coli <- read_excel("data sets/COLI Historical Data - 1990 Q1 - 2019 Q1.xlsx")
path <- "data sets/COLI Historical Data - 1990 Q1 - 2019 Q1.xlsx"
coli <- read_xlsx(path, sheetName = "HistoricalIndexData")
years <- levels(as.factor(coli$YEAR)) 

#-----------------------------------------------------------------
# EXPLORATORY COMPARISONS

# Compare the cities Lulu and Sienna already found in coli_cities to the cities
# that exist in the data set currently
coli_cities <- "/Users/ryankobler/Downloads/COL_CITIES.xlsx"
coli_cities <- read_xlsx(coli_cities)


# Convert string urban area name to factor
coli$URBAN_AREA_NAME <- as.factor(coli$URBAN_AREA_NAME)
levels(coli$URBAN_AREA_NAME)

coli_cities$name <- as.factor(coli_cities$`City Name`)
setdiff(coli_cities$name, coli$URBAN_AREA_NAME)
setdiff(coli$URBAN_AREA_NAME, coli_cities$name)

length(levels(coli_cities$name))
length(levels(coli$URBAN_AREA_NAME))

all <- coli$URBAN_AREA_NAME
tf <- coli_cities$name

# we can see that the number of cities in the entire data (679) set do not match up
# the number of cities listed in the transformed spreadsheet (510). 

# Let's figure out if this has to do with year to year name differences, or if 
# we have left some of the cities in the universe data set out of the transformed

# END OF COMPARISONS
#-------------------------------------------------------------

# Add variable to coli that counts # obsv for each year/quarter/name grouping
# We expect n = 1 whenever a city is present in the data set (if not, y/q/name does not uniquely id observations)
coli_n <- coli %>%
group_by(YEAR, QUARTER, URBAN_AREA_NAME) %>%  
  add_count() %>% 
  select(YEAR, QUARTER, URBAN_AREA_NAME, n) %>% 
  mutate(n)


# RESHAPE FROM LONG TO WIDE BY YEAR-QUARTER TO GET THE MISSING VALUES

# there are a few observations for which city name appears more than once a quarter
remove <- which(coli_n$n > 1)
coli_n <- coli_n[-c(remove),] # ***need to find out which observations i'm removing here***

# glom year & quarter into 1 "time" variable
coli_n$time <- paste0(coli_n$YEAR, coli_n$QUARTER)

library(reshape2)

# reshape from long to wide, keeping only time, city name, and n 
wide <- dcast(coli_n, URBAN_AREA_NAME ~ time, value.var = "n", drop = FALSE) 

# Change "NA"s to 0s
yearnames <- colnames(wide)[-c(1)] # removes the CITY_NAME column
makeZero <- function(colname){
  wide[colname]<- ifelse(is.na(wide[colname]), 0, 1)
  wide
}
wide <- makeZero(yearnames)

# CREATE "SUM" COLUMNS OVER ALL QUARTERS BY YEAR----------------------------------------

# The sumQuarters function takes as input a list of all years the data spans and a data frame.
# It uses the string recognition function `grep` to output the columns that have
# a particular year in their name. For instance, grep takes the string "1990" then outputs the 
# column numbers corresponding to "1990Q1", "1990Q2", "1990Q3", "1990Q4" which is x_1 = c(2,3,4,5).
# Then, lapply loops grep over each year and saves the vector x_i as the ith entry in the list `qtrs``

 sumQuarters <- function(years, df = wide){ # the df object must be in wide form
  qtrs <- lapply(years, grep, x = colnames(df)) # list of lists containing column numbers from wide associate w each year
  names(qtrs) <- years
  for(i in 1:length(qtrs)){
    thisYear <- unlist(qtrs[i])
    df[years[i]] <- rowSums(wide[thisYear])
  }
  df
}

wide <- sumQuarters(years, wide) # Note that we can apply sumQuarters no more than once after wide is created
# because grep will start to include the yearly total column "1990" in the sum
wide$total <- rowSums(wide[years])

# re-order the columns so that 1990, 1990Q1, 1990Q2, etc. are next to each other for easy viewing:
cols <- colnames(wide)
cols <- sort(setdiff(cols, "URBAN_AREA_NAME"))
wide <- wide %>% select(URBAN_AREA_NAME, cols)

  
# WRITE wide DF TO AN EXCEL FILE
write.xlsx(wide, "/Users/ryankobler/Desktop/fun projects/COLI_CITIES_beta.xlsx")










