# This is the solution set for Question 1
library(dplyr)
library(maps)
library(ggplot2)
library(hms)
library(readr)


# Question 1
data <- read_csv("Disaster.csv")

# Question 2 
data <- read_csv("Disaster.csv", header = TRUE)

# Question 3
nrow(data)

# Question 4 
View(data)

# Question 5, Question 6, Question 7 on hold
group_by(data, ORGANIZATION) %>% summarise(TOTAL_ACRES = mean(TOTAL_ACRES),
                                           `DESTRUCTION (in Thousand Dollars)` = mean( `DESTRUCTION (in Thousand Dollars)`))
                                          

# Question 8
#facet plot for ACRE
x <- group_by(data, LOCALITY) %>% summarise(ACRE = sum(TOTAL_ACRES))
y <- x$ACRE

x <- data.frame(c("arizona", "california", "colorado", "idaho", "montana", "nevada", "new mexico",
                  "oregon", "utah", "washington", "wyoming"), y)
colnames(x) <- c("region", "ACRE")
us_map = map_data("state")

dest.map <- left_join(x, us_map)

ggplot(dest.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = ACRE), color = "white")+
  scale_fill_viridis_c(option = "D") + 
  facet_wrap(~region
             , scales = "free"
             , ncol=3)

# Question 9
range(data$YEAR_)
#
decadeOne = filter(data, data$YEAR_ >=1980 & data$YEAR_ < 1990) %>% select(YEAR_)
decadeTwo = filter(data, data$YEAR_ >=1990 & data$YEAR_ < 2000) %>% select(YEAR_)
decadeThree = filter(data, data$YEAR_ >=2000 & data$YEAR_ < 2010) %>% select(YEAR_)

decadeDisaster = c(nrow(decadeOne), nrow(decadeTwo), nrow(decadeThree))
decadeLabel = c("1980-1990", "1991-2000", "2001-2010")
barplot(decadeDisaster,
        names.arg = decadeLabel,
        xlab = "Disasters",
        ylab = "Mean Math Score", legend = c("Lowest", "Highest", "Medium"), 
        args.legend = list(title = "Color Scheme", x = "topleft", cex = .7),
        col = c("#72DE6D", "#6DDEC9", "#6D96DE"))

# Question 10
# After seeing the barplot of question 9, we can clearly observe that most disasters happened in 
# the decade of 2001-2010, followed by 1991-200 and then 1980-1990.
# It is interesting to note that the greatest value of YEAR_ in our dataset is 2006, therefore
# The data for the last decade is incomplete.

# From this we infere that the disasters that occurred in the 6 years of 2000's were already higher 
# than the last 2 decades, therefore, there is a good chance, that the total disasters in this decade
# are much higher.
print("Total disasters between 1980-1990")
print(nrow(decadeOne))

print("Total disasters between 1991-2000")
print(nrow(decadeTwo))

print("Total disasters between 2001-2006")
print(nrow(decadeThree))


# Question 11
startDate <- strptime(data$START_DATE, "%m/%d/%Y %H:%M")
endDate <- strptime(data$END_DATE, "%m/%d/%Y %H:%M")
timeDifference <- difftime(endDate, startDate)
totalSeconds <- mean(timeDifference, na.rm = TRUE)
hm <- as_hms(totalSeconds)
print("The average time taken to overcome a disaster is : ")
print(hm)

# Question 12
month <- startDate$mon + 1
data["Month_of_the_year"] <- month

# Question 13
group_by(data, Month_of_the_year) %>% 
  summarize(Acres = mean(TOTAL_ACRES,na.rm=TRUE),
            Average_Destruction_cost = mean(`DESTRUCTION (in Thousand Dollars)`,na.rm = TRUE),
            Average_Units = mean(UNIT,na.rm = TRUE),
            Most_frequent_cause = CAUSE[sort(table(CAUSE),decreasing = TRUE)[1]] ,
            Most_frequent_locality = LOCALITY[sort(table(LOCALITY),decreasing = TRUE)[1]])

# Question 14 
mydata2=read.table(file="Disaster.csv", sep= ",",header=TRUE, fill=TRUE)
mydata2
class(mydata2)

# Question 15
# Below are both the functions
# The read.csv() function is present in the R base package
sample1 <- read.csv("Disaster.csv")

# The read_csv() function is present in the readr package
sample2 <- read_csv("Disaster.csv")

# The read_csv package came later but read files way faster than read.csv().
# It also has the added functionality of progress meters.
# The read.csv() function might miss some characters ( like '(' and ')' ), but the
# read_csv() does not.

# to illustrate this, run the code writtern bellow and observe the 10th column
View(sample1)

# Question 16
# A good way to do this would be to print the session info
print(sessionInfo())

# Here, however alot of useless information is also provided.
# For a more brief viewing we can do this
session <- sessionInfo()
print("The base packages are")
print(session[6])
print("The imported third party packages in detail are")
print(session[7])
# All the 3rd party packages are loaded in the first 6 lines.

# Question 17
summary(data)
ggpairs(data=data, columns = c(1,3,6,10,11), title="Disaster Data")

# Question 18
z.test(data$Acres, sigma.x = sd(data$Acres))
z.test(data$Destruction, sigma.x = sd(data$Destruction))
z.test(data$YEAR_, sigma.x = sd(data$YEAR_))
z.test(data$UNIT, sigma.x = sd(data$UNIT))

mydata <- data

# Question 19
shapiro.test(mydata$Acres)
shapiro.test(mydata$Destruction)
shapiro.test(mydata$UNIT)
shapiro.test(mydata$YEAR_)


# Question 20
summary(mydata[,c(1,3,6,10,11)])
scaled_data=as.data.frame(scale(mydata[,c(1,3,6,10,11)]))
summary(scaled_data)




