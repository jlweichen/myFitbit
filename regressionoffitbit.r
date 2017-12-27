library(ggplot2)
library(leaps)
# Constructing the myData frame!
myData <-  read.csv(#path to my data from the Fitbit API, saves as csv#)
myData <- cbind(myData[,(2:10)])
names(myData) <- c('date', 'calories', 'steps', 'fairlyActive', 'veryActive', 'heartRate', 'sleep', 'temp', 'onCampus')
# Now we have a lot of nice data to play with!
#We will omit the line that has a missing heart rate value
myData <- na.omit(myData)
#splitting data into training and building
predict <- data.frame()
testing <- data.frame()
set.seed(38)
predictIndex <- c(sample.int(92, 70))
for(i in (1:length(predictIndex))){
   predict <- rbind(predict, myData[predictIndex[i],] )
}
names(predict) <- names(myData)
testingIndex <- c(1:92)
for(i in (1:92)){
  for(j in (1:70))
      {
      if(i == predictIndex[[j]]){
        testingIndex[i]<-0
    }
  }
}
for(i in (1:92)){
  if(testingIndex[i]>0){
    testing <- rbind(testing, myData[testingIndex[i],] )
  }
}
# To get a feel, I first plot some pairs of variables
# Plotting heart rate versus calories in ggplot2
heartplot <- ggplot(data=predict, aes(x=(heartRate), y = calories, color="calories")) +geom_point(size=3)
heartplot <- heartplot + stat_smooth(method="lm", color="blue")
heartplot <- heartplot + coord_cartesian(xlim=c(68,82), ylim=c(1400, 2800))
heartplot
# Plotting steps versus calories in ggplot2
stepplot <- ggplot(data=predict, aes(x=steps, y = calories, color="calories")) +geom_point(size=3)
stepplot <- stepplot + stat_smooth(method="lm", color="blue")
stepplot <- stepplot + coord_cartesian(xlim=c(70,36600), ylim=c(1400, 2800))
stepplot
#Let's try a combined linear model
# one with everything thrown in!
fullModel <- (lm(predict$calories ~ predict$steps + predict$fairlyActive + predict$veryActive + predict$heartRate + predict$sleep + predict$temp + predict$onCampus))
summary(fullModel)

plot(fullModel)
# heteroskedasticity not an issue in residuals vs fitted
# Q-Q plot nice

# the full model shows the strongest input is the number of steps

# intuition tells me maybe sleep is inversely related to calories burnt - I'm buring less when I'm asleep than awake right?
# maybe a model with a transformed sleep is in order
inverseSleep <- c(1/predict$sleep)

summary(lm(predict$calories ~ inverseSleep))
# looks pretty insignificant on its own - what if thrown in with some other variables?

summary(lm(predict$calories ~ inverseSleep + predict$steps + predict$heartRate +predict$fairlyActive + predict$veryActive + predict$temp + predict$onCampus))
# inverse sleep has a bigger p-value than untransformed sleep!
# will keep sleep as is

#use of forward stepwise procedure on the full model
subs <- regsubsets(predict$calories ~ predict$steps + predict$fairlyActive + predict$veryActive + predict$heartRate + predict$sleep + predict$temp + predict$onCampus, method = "forward",  data=myData , nbest=1)
plot(subs, scale="adjr2")
sumsubs <- summary(subs)
sumsubs
# steps is the most important variable to keep, then hfairlyActive, then temperature
red1 <- (lm(predict$calories ~ predict$steps + predict$heartRate + predict$sleep))
summary(red1)
plot(red1)
# this model shows only steps to be significant


# making things simpler and just using steps as a predictor
summary(lm(predict$calories ~ predict$steps))
plot(lm(predict$calories ~ predict$steps))

#confidence inerval for three variable model
confint(red1)

# now to use that training data to see if it works any!

calories <- predict$calories
steps <- predict$steps
heartRate <- predict$heartRate
sleep <- predict$sleep

summary(predict(lm(calories ~ steps + heartRate + sleep), newdata=testing))
predictions <- data.frame(predict(lm(calories ~ steps + heartRate + sleep), newdata=testing))
squareSum <- sum((testing$calories-predictions)^2)
meanSquareSum <- (squareSum/length(testing$calories))
anova(red1)
anova(fullModel)
# mean square errors of full model is larger than reduced model
