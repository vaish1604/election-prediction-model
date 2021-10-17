#we will first load required packages 
library(dplyr)
library(caTools)
#loading the data sets
primarydata=read.csv(file=
                       "https://raw.githubusercontent.com/peterkoebel/ProjectElectionResults/master/primary_winner.csv",stringsAsFactors=T)
countydata=read.csv(file=
                      "https://raw.githubusercontent.com/peterkoebel/ProjectElectionResults/master/county_data.csv",stringsAsFactors=T)
#now we will merge our data sets by using inner join
finaldata=primarydata %>% inner_join(countydata, by = 'fips')
print(finaldata)
#now we will count the counties won by each candidate 
primarydata %>% group_by(winner) %>% summarise(numberofwins = n())
#now we will divide our data set. one for "training" and one for "testing:
set.seed(486)
split = sample.split(finaldata$winner, SplitRatio = 0.8)
#set.seed is used to generate a sequence of random numbers starting from the argument in set.seed()
#our training data set will have 80% data and testing set will have 20% data
trainingdata = finaldata[split,]
testingdata = finaldata[!split,]
#now we check the levle i.e. to see which party is at what level, 0 or 1.
print('The level for data is:')
print(levels(finaldata$winner))
#now we will build a logistic model for our prediction

logisticmodel = glm(winner ~ age_over_65 + female_perc + foreign_born_perc + 
                      bachelors_perc + household_income,
                    data = finaldata,
                    family = binomial)
print(summary(logisticmodel))
#since household income is not significant, we can remove it form our model
logisticmodel = glm(winner ~ age_over_65 + female_perc + foreign_born_perc + 
                      bachelors_perc,
                    data = finaldata,
                    family = binomial)
#now we will predict the winner from our training set
trainingdata$winnerprediction = predict(logisticmodel, trainingdata, type = 'response')
#now before testing we need to find the threshold
for(i in c(0.4, 0.5, 0.6, 0.7)){
  trainingdata = trainingdata %>% 
    mutate(winnerprediction1 = ifelse(trainingdata$winnerprediction > i, 'Hillary Clinton', 'Bernie Sanders'))
  print(sum(as.character(trainingdata$winner) == trainingdata$winnerprediction1)/
          nrow(trainingdata))
}
#mutate function used to create new variables
#we can see that 0.5 gives best answer so we will use 0.5 as out threshold
#now we will make a confusion matrix to check our predictions
trainingdata = trainingdata %>% 
  mutate(winnerprediction1 = ifelse(winnerprediction > 0.5, 'Hillary Clinton', 'Bernie Sanders'))
train_confusion=table(trainingdata$winner, trainingdata$winnerprediction1)
print('The confusion matrix for training data:')
print(train_confusion)

testingdata$winnerprediction = predict(logisticmodel, testingdata, type = 'response')
testingdata = testingdata %>% 
  mutate(winnerprediction1 = ifelse(winnerprediction > 0.5, 'Hillary Clinton', 'Bernie Sanders'))
test_confusion = table(testingdata$winner, testingdata$winnerprediction1)
print('The confusion matrix for testing data:')
print(test_confusion)
#seeing the matrix we can see the correct instances of prediction
#now we calculate the corrected prediction percentage
print('The prediction rate is:')
print(sum(diag(test_confusion))/sum(test_confusion))
#diag() is used to extract or replace elements in the diagonal of a matrix