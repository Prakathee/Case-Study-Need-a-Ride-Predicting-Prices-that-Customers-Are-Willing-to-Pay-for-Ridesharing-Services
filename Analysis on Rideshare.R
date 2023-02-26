#importing data
### Load the packages (all must have been installed)
library(tidyverse)
library(stargazer)
library(openintro)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
#install.packages("plotrix")
library(plotrix)

#importing data
data<-read_excel("Rideshare.xlsx")

#summary
summary(data)

str(data)

#checking for null values
colSums(is.na(data))

####
#General analysis
####

table(data$weekday)
table(data$source)
table(data$destination)
table(data$rideCategory)
table(data$rideshare)
table(data[data$rideshare=="Uber",]$rideCategory)
table(data[data$rideshare=="Lyft",]$rideCategory)
names(data)

class(data)


#####
#Analyzing source and destination
#####

#source 
mytable <- table(data$source)
lbls <- paste(names(mytable), "\n", mytable, sep="")
cols <- rainbow(nrow(mytable))
pie3D(mytable, labels = lbls, labelcex = 0.7,
      main="Sources",
      col = cols,
      border = "black",
      start = 45)


#destination 
mytable <- table(data$destination)
lbls <- paste(names(mytable), "\n", mytable, sep="")
cols <- rainbow(nrow(mytable))
pie3D(mytable, labels = lbls, labelcex = 0.7,
      main="Destination",
      col = cols,
      border = "black",
      start = 45)

#creating a table for source and destinations 
sou_des_data<-data %>% dplyr::group_by(source, destination) %>% 
  dplyr::summarise(count=n(), price_sum=sum(price), price_min = min(price), price_max = max(price),
                   sou_des = paste(source, destination, sep = ","), distance = min(distance)) 



ggplot(sou_des_data, aes(x=source, y=destination)) + geom_point(col="blue") + 
  geom_text(aes(label=distance) , hjust=0.7, vjust=1) +
  labs(title = "Source Destination Distance", y = "Destination", x = "Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

high_sou_des_subset = sou_des_data[sou_des_data$count > 300,] 

ggplot(high_sou_des_subset, aes(y = sou_des)) +
  geom_bar(fill = "violet") + 
  geom_text(aes(x =..count.., label =..count..), stat = "count", vjust = 0.2)+
  labs(title = "Highest frequency source and destinations", x = "Frequency",
       y = "Source and destination") + scale_fill_brewer(palette = "Purples")
+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

low_sou_des_subset = sou_des_data[sou_des_data$count < 200,] 

ggplot(low_sou_des_subset, aes(y = sou_des)) +
  geom_bar(fill = "red") + 
  geom_text(aes(x =..count.., label =..count..), stat = "count", vjust = 0.2)+
  labs(title = "Highest frequency source and destinations", x = "Frequency",
       y = "Frequency") + scale_fill_brewer(palette = "Purples") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
#Analyzing time
#####

#obtaining dates to check frequency of rides
data$Session_Start_Time <- as.POSIXct(data$dateTime, format="%Y-%m-%d %H:%M:%S")
data$date <- as.Date(data$dateTime)


table(data$month, data$rideshare)
#Ride freq for months
ggplot(data, aes(x=month, fill=as.factor(rideshare))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Months", y="Count", fill="Uber vs Lyft", title="Uber and Lyft usage across many months")

#plot to check the frequency of rides across the given months
ggplot(data, aes(x=date, color=as.factor(rideshare))) +
  geom_line(aes(y=..count..), stat="count") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Time duration", y="Count", color="Uber vs Lyft", title = "Uber and Lyft usage across different months by date")


#The frequency of rides throughout the day
ggplot(data, aes(hour)) +
  geom_bar(fill = "blue") + 
  geom_text(aes(y =..count.., label =..count..), stat = "count", vjust = -0.5)+
  labs(title = "Hourly ride data", x = "Hour of the day", y = "Frequency", 
       title_size = 14) +
  scale_fill_brewer(palette = "Purples")
  

#The frequency of rides throughout the day for Uber and Lyft
ggplot(data, aes(x=hour, fill=as.factor(rideshare))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Hours of the day", y="Count", fill="Type of Order", title = "Uber and Lyft usage throughout the day")

#line plot
ggplot(data, aes(x=hour, color=as.factor(rideshare))) +
  geom_line(aes(y=..count..), stat="count") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="hour of the day", y="Count", color="Type of rideshare") +
  ggtitle("Number of rides by hour of the day")

#The frequency of rides throughout the week for Uber and Lyft
ggplot(data, aes(x=weekday, fill=as.factor(rideshare))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Hours of the day", y="Count", fill="Type of Order", title = "Uber and Lyft usage throughout the day")
  


#Number of Uber rides by hour of the day and ride category
ggplot(data[data$rideshare == 'Uber',], aes(x=hour, fill=as.factor(rideCategory))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="hour of the day", y="Count", fill="Type of Uber")+
  ggtitle("Number of Uber rides by hour of the day and ride category")


#Number of Lyft rides by hour of the day and ride category
ggplot(data[data$rideshare == 'Lyft',], aes(x=hour, fill=as.factor(rideCategory))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="hour of the day", y="Count", fill="Type of Lyft")+
  ggtitle("Number of Uber rides by hour of the day and ride category")

#line plot
ggplot(data[data$rideshare == 'Lyft',], aes(x=hour, color=as.factor(rideCategory))) +
  geom_line(aes(y=..count..), stat="count") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="hour of the day", y="Count", color="Type of Lyft") +
  ggtitle("Number of Lyft rides by hour of the day and ride category")
#write possible expln for Lyft


#pie chart for rides
data_pie_rideshare <- data %>%
  group_by(rideshare) %>%
  summarize(count = n())

data_pie_rideshare$percentage <- 100 * data_pie_rideshare$count / sum(data_pie_rideshare$count)

ggplot(data_pie_rideshare, aes(x = 1, y = percentage, fill = rideshare)) +
  geom_col(width = 1, show.legend = TRUE) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Distribution of rideshare types") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  guides(fill=guide_legend(title=NULL, nrow=2, byrow=TRUE, reverse=TRUE)) +
  theme(legend.direction = "vertical", legend.position = "right")


#pie chart for type of Uber rides by their percentage
data_pie <- data[data$rideshare == 'Uber',] %>%
  group_by(rideCategory) %>%
  summarize(count = n())

data_pie$percentage <- 100 * data_pie$count / sum(data_pie$count)

ggplot(data_pie, aes(x = 1, y = percentage, fill = rideCategory)) +
  geom_col(width = 1, show.legend = TRUE) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Distribution of Uber rides") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  guides(fill=guide_legend(title=NULL, nrow=2, byrow=TRUE, reverse=TRUE)) +
  theme(legend.direction = "vertical", legend.position = "right")


#pie chart for type of Lyft rides by their percentage
data_pie2 <- data[data$rideshare == 'Lyft',] %>%
  group_by(rideCategory) %>%
  summarize(count = n())

data_pie2$percentage <- 100 * data_pie2$count / sum(data_pie2$count)

ggplot(data_pie2, aes(x = 1, y = percentage, fill = rideCategory)) +
  geom_col(width = 1, show.legend = TRUE) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Distribution of Lyft rides") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  guides(fill=guide_legend(title=NULL, nrow=2, byrow=TRUE, reverse=TRUE)) +
  theme(legend.direction = "vertical", legend.position = "right")



#####
#Analyzing weather data
#convert to line
ggplot(data, aes(x=weather, fill=as.factor(rideshare))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Weather conditions", y="Count", fill="Weather", title = "Rides during different weather conditions")





#####
#Uber and Lyft ride segments
#####

table(data$rideshare, data$rideCategory)

data$ride_categorized <- ifelse(data$rideCategory %in% c("Black", "Black SUV"), "Black",
                                ifelse(data$rideCategory %in% c("Lux", "Lux Black", "Lux Black XL"), "Luxury",
                                       ifelse(data$rideCategory %in% c("UberX", "Lyft"), "Basic",
                                              ifelse(data$rideCategory %in% c("UberXL", "Lyft XL"), "Basic XL", 
                                                     data$rideCategory))))



#Basic segment
ggplot(data[data$ride_categorized == 'Basic',], aes(x=factor(weekday, 
                                                              levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                                     fill=as.factor(rideshare)))+
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Weekdays", y="Count", fill="Uber vs Lyft", title = "Lyft vs UberX")

#Basic segment
ggplot(data, aes(x=factor(weekday, 
                                                             levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                                    fill=as.factor(rideshare)))+
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Weekdays", y="Count", fill="Uber vs Lyft", title = "Lyft vs Uber")

#Xl segment
ggplot(data[data$ride_categorized == 'Basic XL',], aes(x=factor(weekday, 
                                                             levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                                    fill=as.factor(rideshare)))+
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Traffic Source", y="Count", fill="Uber vs Lyft", title = "Lyft XL vs Uber XL")

#Luxury segment
ggplot(data[data$ride_categorized == 'Luxury',], aes(x=factor(weekday, 
                                                             levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                                    fill=as.factor(rideshare)))+
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Days", y="Count", fill="Lyft", title = "Luxury segment usage for Lyft")

#disability friendly
ggplot(data[data$ride_categorized == 'WAV',], aes(x=factor(weekday, 
                                                              levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                                     fill=as.factor(rideshare)))+
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Days", y="Count", fill="Uber and Lyft", title = "Disability friendly Uber services")





#####
#Analyzing Pricing
#####

#Average pricing for each segment
result <- data %>%   group_by(ride_categorized , rideshare ) %>%  summarise(avg_price = mean(price))
result

#Constructing a plot of Price vs Distance
ggplot(data, aes(y=log(price), x=(distance))) + geom_point(col="blue") + 
  labs(title = "price vs distance", x = "distance", y = "price") +
  stat_smooth(method=lm, col = "red", se=FALSE)

#SurgeMultiplier
table(data$rideshare, data$surgeMultiplier)
#Only Lyft charges at a surge rate

table(data$weather, data$surgeMultiplier)

# #extra charges for weather
# ggplot(data[data$surgeMultiplier != 1,], aes(x=weather, fill=as.factor(weather))) +
#   geom_bar(aes(y=..count..), position="dodge") +
#   geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
#   labs(x="Weather", y="Count", fill="Weather")


#Surge prices for different wewather
ggplot(data[data$surgeMultiplier != 1,], aes(x=weather, fill=as.factor(surgeMultiplier))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Weather", y="Count", fill="Surge multiplier for Lyft", title = "SurgeMultiplier across different weather conditions")

#Surge prices during the day
ggplot(data[data$surgeMultiplier != 1,], aes(x=hour, fill=as.factor(surgeMultiplier))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Days", y="Count", fill="Surge multiplier for Lyft")

#Surge prices during the day
ggplot(data, aes(x=month, fill=as.factor(surgeMultiplier))) +
  geom_bar(aes(y=..count..), position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge2(width = 1), vjust = -0.25) +
  labs(x="Days", y="Count", fill="Surge multiplier for Lyft")


#Constructing a histogram of price to see the price distribution
ggplot(data) + geom_histogram(aes(x=price), col="blue", binwidth = 3) +
  labs(title = "Histogram of price", x="Price", y = "Frequency")




#####
#Building models
#####

######
#Model 1 - Multiple linear regression
######
install.packages("fastDummies")
install.packages("recipes")
library(fastDummies)
library(recipes)
library(Metrics)

#MAKE DUMMIES
data <- dummy_cols(data, select_columns = 'rideCategory')
names(data)

data<-data %>% rename ("rideCategory_Black_SUV" = "rideCategory_Black SUV",
                       "rideCategory_Lux_Black" = "rideCategory_Lux Black",
                       "rideCategory_Lux_Black_XL" = "rideCategory_Lux Black XL",
                       "rideCategory_Lyft_XL" = "rideCategory_Lyft XL")
#simple log-linear reg
lr1<- lm(log(price)~distance, data=data)
summary(lr1)

#dummies of ridecategory in regression
dvlm<-lm(log(price)~distance+rideCategory_Black+ rideCategory_Black_SUV+
           rideCategory_Lux+ rideCategory_Lux_Black + rideCategory_Lux_Black_XL+
           rideCategory_Lyft + rideCategory_Lyft_XL + 
           rideCategory_Shared+ rideCategory_Taxi+ rideCategory_UberPool +
           rideCategory_UberX , data=data)
summary(dvlm)

#Creating dummies of rideshare and month
data <- dummy_cols(data, select_columns = 'rideshare')
data <- dummy_cols(data, select_columns = 'month')
names(data)

dvlm<-lm(log(price)~distance+rideCategory_Black+ rideCategory_Black_SUV+
           rideCategory_Lux+ rideCategory_Lux_Black + rideCategory_Lux_Black_XL+
           rideCategory_Lyft + rideCategory_Lyft_XL + 
           rideCategory_Shared+ rideCategory_Taxi+ rideCategory_UberPool +
           rideCategory_UberX + month_8 + month_9 + month_10 
         + month_11 + rideshare_Lyft, data=data)
summary(dvlm)

predicted<-dvlm$fitted.values
mape(log(data$price),predicted)
mae(log(data$price),predicted)
mse(log(data$price),predicted)
AIC(dvlm)
BIC(dvlm)

# removing statistically insignificant variables to calculate AIC and BIC
dvlm1<-lm(log(price)~distance+
            rideCategory_Lux+ rideCategory_Lux_Black + rideCategory_Lux_Black_XL+
            rideCategory_Lyft_XL + 
            rideCategory_Shared+ month_10 
          + month_11 + rideshare_Lyft, data=data)
summary(dvlm1)

predicted<-dvlm1$fitted.values
mape(log(data$price),predicted)
mae(log(data$price),predicted)
mse(log(data$price),predicted)
AIC(dvlm1)
BIC(dvlm1)

#######
#Model 2 - Predictive logit model
######
set.seed(123)
# Generate random indices
indices <- sample(nrow(logit_data), size = floor(0.7 * nrow(data)), replace = FALSE)

# Create the training set
train_data <- logit_data[indices, ]

# Create the test set
test_data <- logit_data[-indices, ]

model <- glm(U_L ~ price + distance, data = train_data, family = binomial(link = "logit"))
summary(model)

# Make predictions
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predictions to binary values
predictions <- ifelse(predictions > 0.5, 1, 0)
table(predictions, test_data$U_L)


#Accuracy
accuracy <- mean(predictions == test_data$U_L)
accuracy

library(caret)

confusion_matrix <- confusionMatrix(as.factor(test_data$U_L), as.factor(predictions))
print(confusion_matrix)


print(confusion_matrix$table)

tp <- confusion_matrix$table[1,1] # true positive
fp <- confusion_matrix$table[1,2] # false positive
tn <- confusion_matrix$table[2,2] # true negative
fn <- confusion_matrix$table[2,1] # false negative

# Calculate precision and recall
precision <- tp / (tp + fp)
print(precision)

#recall
recall <- tp / (tp + fn)
print(recall)

f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

