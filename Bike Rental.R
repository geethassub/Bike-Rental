library(rpart)
library(rattle)#better visualisation for decision tree models
library(rpart.plot)
library(RColorBrewer)
library(randomForest)


setwd("....Kaggle/Bike Rental")

train <- read.csv(file.path(getwd(),"train.csv" ))
test <- read.csv(file.path(getwd(),"test.csv" ))

str(train)
str(test)

#to combine both test and train

test$casual = 0
test$registered = 0
test$count = 0 

data = rbind(train,test)

str(data)
table(is.na(data))# find any missing values

par(mfrow = c(4,2))
par(mar = rep(2,4))
hist(data$season)
hist(data$holiday)
hist(data$workingday)
hist(data$weather)
hist(data$temp)
hist(data$atemp)
hist(data$humidity)
hist(data$windspeed)

#convert discrete variables to factor

data$season <- as.factor(data$season)
data$holiday <- as.factor(data$holiday)
data$workingday <- as.factor(data$workingday)
data$weather <- as.factor(data$weather)

#Find hourly trend

data$hour <- substr(data$datetime,12,13)
data$hour <- as.factor(data$hour)

head(test$datetime)

train <- data[as.integer(substr(data$datetime,9,10))<20,]
test <- data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$count~train$hour, xlab = "Hours", ylab ="Users")

#Find daily trend

date <- substr(data$datetime, 1,10)
days <- weekdays(as.Date(date))
data$day <- days

data$day <- as.factor(data$day)

boxplot(data$registered~data$day, xlab = "day", ylab = "registered")
boxplot(data$casual~data$day, xlab = "day", ylab = "casual")


#find correlation between temperature, windspeed, humidity

sub <- data.frame(train$registered,train$casual,train$count,train$temp,train$humidity,train$atemp,train$windspeed)
cor(sub)

#Find trend over the year

data$year <- substr(data$datetime, 1,4)
data$year <- as.factor(data$year)

train <- data[as.integer(substr(data$datetime,9,10))<20,]
test <- data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year,xlab="year", ylab="count")


#Feature engineering - adding additional variables to make better prediction

#hour bins

train$hour <- as.integer(train$hour)
test$hour <- as.integer(test$hour)

d <- rpart(registered~hour, data = train)
prp(d)
fancyRpartPlot(d)

   #based on the above tree different bins(groups) are created
data=rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>=22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==19 | data$hour==18]=7

d_c <- rpart(casual~hour, data = train)
prp(d_c)

data$dc_cas = 0

data$dc_cas[data$hour<10 & data$hour <8] = 1
data$dc_cas[data$hour<10 & data$hour >8] = 2
data$dc_cas[data$hour>10 & data$hour <20] = 3
data$dc_cas[data$hour>10 & data$hour >20] = 4

#Temp bins

train$temp <- as.integer(train$temp)
test$temp <- as.integer(test$temp)

t <- rpart(registered~temp , data = train)
fancyRpartPlot(t)
prp(t)

data$temp_reg = 0
data$temp_reg[data$temp<20 & data$temp <12] = 1
data$temp_reg[data$temp<20 & data$temp >12] = 2
data$temp_reg[data$temp>20 & data$temp <28] = 3
data$temp_reg[data$temp>20 & data$temp >28] = 4

table(data$temp_reg)

t_c <- rpart(casual~temp , data = train)
fancyRpartPlot(t_c)
prp(t_c)

data$temp_cas = 0
data$temp_cas[data$temp<22 & data$temp <14] = 1
data$temp_cas[data$temp<22 & data$temp >14] = 2
data$temp_cas[data$temp>22 & data$temp <30] = 3
data$temp_cas[data$temp>22 & data$temp >30] = 4

table(data$temp_cas)

#year bins - split the years on quarterly basis

data$year_part = 0
data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

#day type bin

data$day_type = 0
data$day_type[data$holiday ==1] = "holiday"
data$day_type[data$workingday ==1 & data$holiday == 0] = "working day"
data$day_type[data$workingday == 0] = "weekend"
table(data$day_type)

data$weekend = 0
data$weekend[data$day == "Sunday" | data$day == "Saturday"] = 1

#Model Building

#convert all discrete variable to factor

data$hour <- as.factor(data$hour)
data$day_type <- as.factor(data$day_type)
train <- data[as.integer(substr(data$datetime,9,10))<20,]
test <- data[as.integer(substr(data$datetime,9,10))>19,]

set.seed(415)
fit1 <- randomForest(registered~hour +workingday+day+holiday+ day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred1 <- predict(fit1, test)
test$registered <- pred1

set.seed(415)
fit2 <- randomForest(casual~hour +workingday+day+holiday+ day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred2 <- predict(fit2, test)
test$casual <- pred2

test$count = test$registered+test$casual

final <- data.frame(data = test$datetime, count = test$count)

write.csv(final, file = "submit.csv", row.names = FALSE)
