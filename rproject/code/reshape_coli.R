library(readxl)
library(dplyr)

# IMPORT DATA FROM EXCEL FILES FROM SHARED DROP BOX
coli <- "/Users/ryankobler/Downloads/COLI Historical Data - 1990 Q1 - 2019 Q1.xlsx"
coli <- read_xlsx(coli)

# Grab a list of all the years we have
years <- levels(as.factor(coli$YEAR))


# Convert string urban area name to factor
coli$URBAN_AREA_NAME <- as.factor(coli$URBAN_AREA_NAME)

library(dplyr)
coli_n <- coli %>%
  group_by(YEAR, QUARTER, URBAN_AREA_NAME) %>%  # add variable that counts # obsv given a year/quarter/name grouping
  add_count() %>% 
  select(YEAR, QUARTER, URBAN_AREA_NAME, n) %>% mutate(n)

#-------------------------------------------------------------
# RESHAPE FROM LONG TO WIDE BY YEAR TO GET THE MISSING TIMES

# there are a few observations for which city name appears more than once a quarter (not sure why yet...)
remove <- which(coli_n$n > 1)
coli_n <- coli_n[-c(remove),] # ***need to find out which observations are removed here***
coli_n$time <- paste0(coli_n$YEAR, coli_n$QUARTER)

library(reshape2)
# reshape long to wide, keeping only time, name of city and n 
wide <- dcast(coli_n, URBAN_AREA_NAME ~ time, value.var = "n", drop = FALSE) 

# Change "NA"s to 0s with function makeZero()
yearnames <- colnames(wide)[-c(1)] # removes the CITY_NAME column
makeZero <- function(col){ # takes as input a column name
  wide[col]<- ifelse(is.na(wide[col]), 0, 1)
  wide
}

wide <- makeZero(yearnames)
View(wide)

# WRITE wide df TO EXCEL FILE:
library(xlsx)
write.xlsx(wide, "/Users/ryankobler/Desktop/fun projects/COLI_CITIES_R.xlsx")


# Check if all values in a row are missing:
all.NA <- function(id, df = coli){
  row <- df[c(id),]
  n_missings <- sum(is.na(row))
  n_missings==length(row)
}
lapply(remove, all.NA)
  sum(is.na(coli[remove[1],]))


