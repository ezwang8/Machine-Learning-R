bike <- read.csv('bikeshare.csv')
head(bike)

# Create a scatter plot of count vs temp
library(ggplot2)
ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()

# Plot count vs datatime scatterplot
bike$datetime <- as.POSIXct(bike$datetime)
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

# What is the correlation between temp and count?
cor(bike[,c('temp','count')])

# Create a boxplot, with the y axis indicating count 
# and the x axis begin a box for each season.
ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()

# Create an "hour" column that takes the hour from the datetime column.
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})
head(bike)

# Create a scatterplot of count versus hour, with color scale based on temp.
library(dplyr)
pl <- ggplot(filter(bike,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

# Create the same plot for non working days
pl <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

# Use lm() to build a model that predicts count based solely on the temp feature
temp.model <- lm(count~temp,bike)
summary(temp.model)

# How many bike rentals would we predict if the temperature was 25 degrees Celsius?
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

# Use sapply() and as.numeric to change the hour column to a column of numeric values.
bike$hour <- sapply(bike$hour,as.numeric)

# Build a model that attempts to predict count based off of the following features.
# season holiday workingday weather temp humidity windspeed hour (factor)
model <- lm(count ~ . -casual - registered -datetime -atemp,bike )

# Get the summary of the model
summary(model)

