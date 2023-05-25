library(tidyverse)        #read modeling data
songs = read.csv('C:/Users/10791/Desktop/5200/analysisData.csv')
scoringData = read.csv('C:/Users/10791/Downloads/scoringData.csv')

library(stringr)
songs$genre=gsub("\\[|\\]", "",songs$genre) # add a new column after removing "[]"
scoringData$genre= gsub("\\[|\\]","",scoringData$genre) 

library(qdapTools)
genre <- mtabulate(strsplit(as.character(songs$genre),", ")) # update new column after removing 
songs <- cbind(songs,genre)
songs <- songs[,-4]
genre1 <-mtabulate(strsplit(as.character(scoringData$genre),", "))
scoringData <- cbind(scoringData,genre1)
scoringData <- scoringData[,-4]

library(janitor)
songs <- clean_names(songs)          #remove duplicate category
scoringData <- clean_names(scoringData)
names(songs)<- gsub(" ","_", names(songs))
names(scoringData)<- gsub(" ","_",names(scoringData))

songs[is.na(songs)]<-0
scoringData[is.na(scoringData)]<-0

view(songs)

write.csv(x = songs,file = "C:/Users/10791/Desktop/data.csv")

# following code will read data and construct a ranger model
#split data
library(caret)
set.seed(123)
split = createDataPartition(y=songs$rating,p = 0.8,list = F,groups = 100)
train = songs[split,]
test = songs[-split,]

#ranger model
library(ranger)
forest_ranger = ranger(rating~time_signature+tempo+energy+song+track_duration+adult_standards
                       +album_rock+urban_contemporary+trap+southern_soul+soul+soft_rock
                       +dance_pop+brill_building_pop+classic_rock+bubblegum_pop+country+classic_soul+contemporary_country
                       +country_road+country_rock+folk_rock+hip_hop+funk+disco+folk+mellow_gold+rock+pop+motown
                       +pop_rap+rap+quiet_storm+rock_and_roll+post_teen_pop+r_b+new_wave_pop+pop_rock, data=train, num.trees = 2000)

pred_train_ranger = predict(forest_ranger, data = train, num.trees = 2000)
rmse_train_forest_ranger = sqrt(mean((pred_train_ranger$predictions - train$rating)^2)); rmse_train_forest_ranger

pred_test_ranger = predict(forest_ranger, data = test, num.trees = 2000)
rmse_test_forest_ranger = sqrt(mean((pred_train_ranger$predictions - test$rating)^2)); rmse_test_forest_ranger



pred4=predict(forest_ranger, scoringData)$predictions
pred4
submissionFile = data.frame(id = scoringData$id, rating = pred4)
write.csv(submissionFile, 'C:/Users/10791/Desktop/3_submission.csv',row.names = F)

