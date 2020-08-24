library(readr)
library(tidyverse)
library(tidyverse)
library(dplyr)
library(lubridate)
library(maps)
data <- read_csv("Covid_dataset2020.csv")


View(data)
# Dates
data$reporting_date <- parse_date_time(data$reporting_date, orders = c("mdy", "dmy"))
data$symptom_onset <- parse_date_time(data$symptom_onset, orders = c("mdy", "dmy"))
sum(is.na(data$symptom_onset))

data$visit_date_hosp <- parse_date_time(data$visit_date_hosp, orders = c("mdy", "dmy"))
sum(is.na(data$visit_date_hosp))

data$exposure_startdate <- parse_date_time(data$exposure_startdate, orders = c("mdy", "dmy"))
sum(is.na(data$exposure_startdate))

data$exposure_enddate <- parse_date_time(data$exposure_enddate, orders = c("mdy", "dmy"))
sum(is.na(data$exposure_enddate))

data$recovered <- parse_date_time(data$recovered, orders = c("mdy", "dmy"))
sum(is.na(data$recovered))

data$death <- parse_date_time(data$death, orders = c("mdy", "dmy"))

# International Traveller
tempIntl <- data$intl_traveler
tempIntl[4] <- NA
data$intl_traveler <- tempIntl

# Domestic Traveller
# should be dropped

# Visiting Wuhan
data$visiting_Wuhan[data$visiting_Wuhan == "No"] = 0
data$visiting_Wuhan[data$visiting_Wuhan == "Yes"] = 1

# Lives in Wuhan
data$lives_in_Wuhan[data$lives_in_Wuhan == "No"] = 0
data$lives_in_Wuhan[data$lives_in_Wuhan == "Yes"] = 1
View(data)
write_csv(data, "tidydataset_2017CSC1018.csv")


#importing
data <- read_csv("tidydataset_2017CSC1018.csv")

#Unique research problem

# this question statement is two fold, and will demonstrate how self protection is the way forwards
# a) - we calculate wether older people in each and every region have a high risk of getting corona.
# b) - can we draw gender conclusions - ie what gender is more susuptible to this, and is this region specific.

# first of all, we plot a bar graph to see the difference in all these countries.

countryData <- data %>% group_by(country) %>% summarise(case_in_country = last(case_in_country))

ggplot(countryData, aes(x = country, y =case_in_country)) +
  geom_bar(stat = "identity",aes(fill=case_in_country) ) + 
  scale_fill_viridis_c(option = "C") +
  labs(title = "Country wise Distribution of Cases",
       y="Number of Cases",x="Countries")

# To get a better look, here they are varying geographically

world_map <- map_data("world")
world_map$region[which(world_map$subregion == "Hong Kong")] <- "Hong Kong"
world_map$region[which(world_map$subregion == "Finland")] <- "Finland"
world_map$region[which(world_map$subregion == "Phillipines")] <- "Phillipines"


countryData <- countryData %>% rename(region = country)

dest_map <- left_join(countryData, world_map)

ggplot(dest_map, aes(long, lat, group = group), xlim(0,100))+
  geom_polygon(aes(fill = case_in_country), color = "white")+
  scale_fill_viridis_c(option = "D") 

# The USA is very very far ,very large with healthcare, yet there are many cases there to be seen.

# Plotting a bargrpah for genders and NA
  genderData <- data %>% group_by(gender) %>% summarise(total_affected=n())

  ggplot(genderData, aes(gender, total_affected)) +
  geom_bar(stat = "identity", aes(fill = total_affected)) + 
  scale_fill_viridis_c(option = "D") +
  labs(title = "Bar Graph depicting total cases for both Genders",
       y="Number of Cases",x="Gender")
  
# If the virus started in China, why does USA have the most number of cases.
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
# Ungendered ...............
  
modeData <- select(data, country, age) %>% filter(!is.na(age)) %>% group_by(country) %>% summarise(mode_age = getmode(age))

  ggplot(modeData, aes(x = mode_age)) + 
    geom_density(color="orange1", fill="#2ADBCF") + 
    geom_vline(aes(xintercept = mean(mode_age)), linetype = "dashed")
  
    boxplot(modeData$mode_age)
  
ungenderedMean <- mean(modeData$mode_age)

# Gendered ...............
  

gendered_mode_data <- select(data, country, age, gender) %>% filter(!is.na(age), !is.na(gender)) %>% group_by(country, gender) %>% summarise(mode_age = getmode(age))
  
ggplot(gendered_mode_data, aes(x = mode_age)) + 
  geom_density(color="orange1", fill="#2ADBCF") + 
  geom_vline(aes(xintercept = mean(mode_age)), linetype = "dashed")

  boxplot(gendered_mode_data$mode_age)


# female .................
female_mode_data <- select(gendered_mode_data, country, mode_age, gender) %>% filter(gender == "female")
ggplot(female_mode_data, aes(x = mode_age)) + 
  geom_density(color="orange1", fill="#2ADBCF") + 
  geom_vline(aes(xintercept = mean(mode_age)), linetype = "dashed")

femaleMean <- mean(female_mode_data$mode_age)

boxplot(female_mode_data$mode_age)

# male .....................

male_mode_data <- select(gendered_mode_data, country, mode_age, gender) %>% filter(gender == "male")

ggplot(male_mode_data, aes(x = mode_age)) + 
  geom_density(color="orange1", fill="#2ADBCF") + 
  geom_vline(aes(xintercept = mean(mode_age)), linetype = "dashed")

maleMean <- mean(male_mode_data$mode_age)

boxplot(male_mode_data$mode_age)

  




