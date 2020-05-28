#-----------------------------------------------
#1. WHICH CITIES OBSERVED AT LEAST ONCE A YEAR? 

wide$city <- wide$URBAN_AREA_NAME
wide$allyears <- NA

# Gives vector of TRUE/FALSE wherever there is 0/non-zero
isZero <- function(year){
  wide[year] == 0
}

list <- !isZero(years)
allyears <- apply(!list, 1, all)
wide$allyears <- allyears
sum(allyears)
# There are 91 cities consistently observed in the sample :(

# Gives which 91 cities these are:
wide %>% 
  filter(allyears == T) %>% 
  select(city, allyears, years)

#-----------------------------------------------
# 2. WHICH CITIES ARE OBSERVED IN 90%, 75%, 50% OF THE YEARS?

# This function takes a boolean vector and sums the true components
# and divides by the total number of years to get percentage of occurences
percentTrue <- function(row, pct = 0.5){
  sum(row) / length(row) >= pct
}

wide$ninety <- apply(list, 1, percentTrue, pct = 0.9)
wide$fifty <- apply(list, 1, percentTrue, pct = 0.5)
wide$seventyfive <- apply(list, 1, percentTrue, pct = 0.75)



#-----------------------------------------------
# 3. WHICH CITIES ARE OBSERVED AT LEAST ONCE EVERY OTHER YEAR?

# In this case, we are considering only cities for which we
# observed everybody in all years or saw every other year. 
# This means a row must be true in all even years
# or true in all odd years.

noConsecFalse <- function(row){
  falses <- which(row==F)
  i <- 1
  while(i <= length(falses) - 1){
    if(falses[i+1] - falses[i] == 1){
      return(FALSE)
    }
    i <- i + 1
  }
  TRUE
}

wide$everyOther <- apply(list, 1, noConsecFalse)

# Every other year adds 56 new cities to the group in which all cities must
# have been observed at least once a year.
wide %>%
  filter(everyOther==T & allyears == F) %>% 
  select(city, years)

#-----------------------------------------------
# 4. WHICH CITIES ARE OBSERVED AT LEAST ONCE EVERY OTHER YEAR?





