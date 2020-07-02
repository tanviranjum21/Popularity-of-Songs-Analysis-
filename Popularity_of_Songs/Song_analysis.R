#load all the necessary libraries 
library(tidyverse)
library(ggplot2)
library(caTools)

#load the dataset
songdata <- read.csv("Dataset.csv")
table(songdata$year)

#understanding the data 
str(songdata)

songs.2010 = subset(songdata, year == 1998)

nrow(songs.2010)

Britney_Spears = subset(songdata, artistname == "Britney Spears")

nrow(Britney_Spears)

Britney_spears.10 = subset(songdata, artistname == "Britney Spears" & Top10 == 1) 
levels(factor(Britney_spears.10$songtitle))

nrow(Britney_spears.10)

#timesignature variable: a musical score 
table(songdata$timesignature)

#Songs with the highest tempo 
levels(factor(songdata$songtitle[which.max(songdata$tempo)]))

#splitting Data 

set.seed(123)
split = sample.split(songdata,SplitRatio = 0.8)
train = subset(songdata, split == "TRUE")
test = subset(songdata, split == "FALSE")

#now for the analysis year, songtitle, artistname, songid, artist id is not important for analysis
#so we will remove them from the training and testing dataset for efficient calculation 
nonessential_variables <- c("year", "songtitle", "artistname", "songID", "artistID")
train = train[, !(names(train) %in% nonessential_variables)]
str(train)
test = test[, !(names(test) %in% nonessential_variables)]
str(test)

#creating model 
model <- glm(Top10 ~ ., data = train, family = "binomial")
summary(model)

#to find out the optimized values 
step(model,direction = "both")

#final model 
newmodel <- glm(formula = Top10 ~ timesignature_confidence + loudness + tempo_confidence + 
      key + key_confidence + energy + pitch + timbre_0_min + timbre_0_max + 
      timbre_1_min + timbre_3_max + timbre_4_min + timbre_4_max + 
      timbre_5_min + timbre_6_min + timbre_6_max + timbre_7_min + 
      timbre_7_max + timbre_10_max + timbre_11_min + timbre_11_max, 
    family = "binomial", data = train)
summary(newmodel)

#making prediction on training dataset 
#let us call it predictTrain and use the predict function to make predictions using the model of new model of songs dataset. We will
#also use an argument called type = "response" which gives us the probabilities. We should always preduct on the unseen 
#observations but here want to get the value of the threshold, hence the predictions on the train set.

predictTrain <- predict(newmodel, type = "response")
summary(predictTrain)

tapply(predictTrain, train$Top10, mean)

#we find that for all of the true top 10 in billboard cases , we predict an average probability of about 0.313.
#and for true not in top 10 cases, it is 0.12 

#baseline model 
table(songdata$Top10)
nrow(songdata)
accuracy_for_in_top_10 <- 1119/nrow(songdata)
accuracy_for_in_top_10 #so baseline accuracy is 0.15

#setting a threshold value 

#confusion matrix for threshold 0.45 
confmat <- table(train$Top10, predictTrain > 0.45)
confmat

#Accuracy (TP+TN/Total)
(confmat[2,2] + confmat[1,1])/sum(confmat)

#sensitivity 
(confmat[2,2]/(confmat[2,2] + confmat[2,1]))

#specificity
(confmat[1,1]/(confmat[1,1]+confmat[1,2]))

#confusion matrix for threshold 0.7 
confmat <- table(train$Top10, predictTrain > 0.7)
confmat

#Accuracy (TP+TN/Total)
(confmat[2,2] + confmat[1,1])/sum(confmat)

#sensitivity 
(confmat[2,2]/(confmat[2,2] + confmat[2,1]))

#specificity
(confmat[1,1]/(confmat[1,1]+confmat[1,2]))


#confusion matrix for threshold 0.3 
confmat <- table(train$Top10, predictTrain > 0.3)
confmat

#Accuracy (TP+TN/Total)
(confmat[2,2] + confmat[1,1])/sum(confmat)

#sensitivity 
(confmat[2,2]/(confmat[2,2] + confmat[2,1]))

#specificity
(confmat[1,1]/(confmat[1,1]+confmat[1,2]))


#checking ROC Curve(Reciever Operator Characteristic Curve) for optimum threshold value 
install.packages("ROCR")
library(ROCR)

#calling predicttrain 
ROCpred <- prediction(predictTrain, train$Top10)
#performance function
ROCRpref <- performance(ROCpred, "tpr", "fpr")

#plotting the ROC curve 
plot(ROCRpref)

plot(ROCRpref, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#so according to ROC we will take 0.3 as threshold as we are looking for low
#false positive rate 

#confusion matrix for threshold 0.3 
confmat <- table(train$Top10, predictTrain >= 0.3)
confmat

#Accuracy (TP+TN/Total)
(confmat[2,2] + confmat[1,1])/sum(confmat)

#sensitivity 
(confmat[2,2]/(confmat[2,2] + confmat[2,1]))

#specificity
(confmat[1,1]/(confmat[1,1]+confmat[1,2]))

#prediction on test data set 
predictTest <- predict(newmodel, type = "response", newdata = test)

confustion_matrix <- table(test$Top10, predictTest >= 0.3)

#accuracy test 
(confustion_matrix[2,2] + confustion_matrix[1,1])/sum(confustion_matrix)

#calculating the McFadden's Pseudo R^2, we will pull the log-likelihood of the null model of
#the newmodel variable buy getting the value for the null deviance and dividing by -2

ll.null <- newmodel$null.deviance/-2

#log-likelihood for the newmodel 

ll.proposed <- newmodel$deviance/-2

(ll.null- ll.proposed)/ll.null

#calculating p value for the r-squared using chi-square distribution 
1- pchisq(2*(ll.proposed- ll.null), df=(length(newmodel$coefficients)-1))


#drawing graph 
#to draw the graph, we start by creating new data.frame that contains probabilities of having 
#in top 10 along with the actually in top 10 in train dataset 
predicted.data <- data.frame(
  probability.of.top10 = newmodel$fitted.values,
  top10 = train$Top10
)

#sort the data.frame from low probabilities to high probabilities 
predicted.data <- predicted.data[
  order(predicted.data$probability.of.top10, decreasing = FALSE),
]

#add a new column to the data.frame that has the rank of each sample from low probability to
#high probability 
predicted.data$rank <- 1:nrow(predicted.data)

library(ggplot2)
library(cowplot)

ggplot(data = predicted.data, aes(x =rank, y = probability.of.top10)) +
  geom_point(aes(color = top10), alpha = 1, shape = 4, stroke = 2 ) +
  xlab("Index") +
  ylab("Predicted probability of getting into top 10 in the billboard")
  
  
ggsave("getting_into_top_1p.pdf")
  
#gains chart
library(gains)
gains(test$Top10, predict(newmodel, type = "response",newdata = test), groups = 10)
  
  
  