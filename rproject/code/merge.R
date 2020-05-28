#----------------------------------------------------
# MERGING CPI DATA: Which variable(s) should we try to join on?

# How many cities in the CPI data overlap with the COLI stuff?

library(readxl)
cpi <- "/Users/ryankobler/Downloads/CPI Data_Final.xlsx"
cpi <- read_xlsx(cpi)
cpi$city <- as.factor(cpi$City)

library(reshape2)
cpi_n <- cpi %>%
  group_by(Year, city) %>%  
  add_count() %>% 
  select(Year, city, n) %>% 
  filter(city != "N/A") %>%
  mutate(n)
# reshape from long to wide, keeping only time, city name, and n 
wide_cpi <- dcast(cpi_n, city ~ Year, value.var = "n", drop = FALSE) 

yearnames <- colnames(wide_cpi)[-c(1)] # removes the city column
makeZero <- function(colname){
  wide_cpi[colname]<- ifelse(is.na(wide_cpi[colname]), 0, 1)
  wide_cpi
}

wide_cpi <- makeZero(yearnames)
wide_cpi$total <- rowSums(wide_cpi[yearnames])

wide_cpi %>% 
  select(total, city)


