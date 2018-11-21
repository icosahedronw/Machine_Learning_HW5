library(dplyr)
library(randomForest)
library(ROCR)
library(tidyr)
library(stats)
library(doParallel)
library(foreach)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)

library(memisc)
library(foreign)
library(Hmisc)
library(data.table)
library(ggmap)
library(caret)
library(maptools) 
library(gpclib)
library(sp)
library(rgdal)
library(readr)

setwd("C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/HW/HW5/")
all_data<-read_csv(file = "DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
View(all_data) #375012 obs
str(all_data)

#A
#a
#Drop additional following columns
all_data<-all_data %>% select(-BUILDING, -STREET, -PHONE, -DBA,-ZIPCODE, -12, -16, -17)
#Make INSPECTION DATE a date object
all_data$`INSPECTION DATE`<-as.Date(all_data$`INSPECTION DATE`, "%m/%d/%Y")
#extract the year as inspection_year
all_data$inspection_year<-year(all_data$`INSPECTION DATE`)

#b
#Rename the appropriate columns
all_data <- dplyr::rename(all_data, id = CAMIS, borough = BORO, cuisine = `CUISINE DESCRIPTION`,inspection_date=`INSPECTION DATE`,action=`ACTION`,code=`VIOLATION CODE`,critical=`CRITICAL FLAG`,score=`SCORE`,grade=`GRADE`,inspection_type=`INSPECTION TYPE`) 
#Deal with ‘Missing’ borough information
unique(all_data$borough)
sum(all_data$borough=="Missing") #64 missing boroughs
ind0<-which(all_data$borough=="Missing")
all.data<-read_csv(file = "DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
missing_borough<-all.data[ind0,]
View(missing_borough)
#Add borough information according to location
unique(missing_borough$CAMIS)
all_data$borough<-ifelse(all_data$id==41669951,"QUEENS",all_data$borough)
all_data$borough<-ifelse(all_data$id==50049804,"BROOKLYN",all_data$borough)
all_data$borough<-ifelse(all_data$id==50056175,"BROOKLYN",all_data$borough)
#Other missing_borough contain missing informaiton for the whole observation, so just delete these obs
ind00<-which(all_data$id==50080387|all_data$id==50076237|all_data$id==50080702|all_data$id==50075690|all_data$id==50081016|all_data$id==50075691)
all_data<-all_data[-ind00,] #375006 obs
#Make sure all missing boroughs have been dealt with
sum(all_data$borough=="Missing")
rm(all.data)
#remove restaurants that haven’t been inspected
all_data<-all_data[which(all_data$inspection_date!= "1900-1-1"),] #373819 obs
sum(all_data$inspection_date== "1900-1-1")
#remove rows without a score or with a negative 1 score
all_data<-all_data[which(!is.na(all_data$score)&all_data$score>=0),] #355370 obs
#remove six inspection types
unique(all_data$inspection_type)
all_data<-all_data[which(all_data$inspection_type!="Calorie Posting / Re-inspection"&all_data$inspection_type!="Inter-Agency Task Force / Re-inspection"&all_data$inspection_type!="Smoke-Free Air Act / Re-inspection"&all_data$inspection_type!="Administrative Miscellaneous / Re-inspection"&all_data$inspection_type!="Trans Fat / Re-inspection"&all_data$inspection_type!= "Inter-Agency Task Force / Re-inspection"),] #355359 obs
!#replace all scores for any inspection for a given restaurant on a given day by the maximum score
all_data<-all_data%>%arrange(desc(id)) 
#Locate same inspections with different scores
all_2<-all_data%>%select(id, borough, cuisine, inspection_date, code, score)
View(all_2)
ind000<-which(duplicated(all_2[,c(1,2,3,4,6)]))
unique_all<-unique(all_2$id) #25464 unqiue restaurants
#for (i in 1:10){
#  if (all_2$id==unique_all[i]){
#    ind0000<-which(duplicated(all_2[,c(1,2,3,4,6)]))
#  }
#}
########
###########
#B
#Restrict to 2015, 2016, or 2017
restaurant.data<-all_data[which(all_data$inspection_year==2015|all_data$inspection_year==2016|all_data$inspection_year==2017),] #255467 obs
View(restaurant.data)
#Restrict to  "Cycle Inspection / Initial Inspection" 
unique(restaurant.data$inspection_type)
ind1<-grep("Cycle Inspection / Initial Inspection",restaurant.data$inspection_type)
restaurant.data<-restaurant.data[ind1,] #155781 obs
#Restrict to certain features
restaurant_data1<-restaurant.data%>% select(id,borough,cuisine,inspection_year,inspection_date,score)
View(restaurant_data1)
#Delete duplicated obs(duplication due to several different violations in one inspection)
restaurant_data1<-restaurant_data1[!duplicated(restaurant_data1),] #53025 obs
#Locate same inspections with different scores
ind2<-which(duplicated(restaurant_data1[,c(1,2,3,4,5)])) #3 obs
#Order by scores in decreasing order
restaurant_data1<-as.data.frame(restaurant_data1)
restaurant_data1<-restaurant_data1[order(restaurant_data1[,1],restaurant_data1[,6],decreasing = T),]
View(restaurant_data1)
#Deal with the 3 obs with different scores for the same inspection
restaurant_data2<-restaurant_data1[!duplicated(restaurant_data1[,c(1,2,3,4,5)],fromLast=TRUE),]
View(restaurant_data2) #53022 unduplicated obs
unique(restaurant_data2$id) #20068 different restaurants
#Create a binary outcome variable 
restaurant_data2$outcome<-ifelse(restaurant_data2$score>=28,1,0)
#Just keep borough, cuisine, outcome, and inspection_year
restaurant_data<-restaurant_data2%>% select(-id,-score,-inspection_date)
View(restaurant_data)

#C
#Add month and weekday to restaurant_data
restaurant_data2$month<-month(restaurant_data2$inspection_date)
restaurant_data2$weekday<-weekdays(restaurant_data2$inspection_date)
View(restaurant_data2)
#Add four features constructed from historical inspection records
all_data_2<-all_data%>% select(id,score,action,inspection_date)
all_data_2$inspection_year<-year(all_data_2$inspection_date)
View(all_data_2)
all_data_2<-all_data_2%>%filter(all_data_2$inspection_year!=2018) #258326 obs
# unique(all_data_2$id)
# unique(restaurant_data2$id)
# unique(all_data_2$inspection_year)
all_data_2 <- dplyr::rename(all_data_2,inspection_date.y = inspection_date) 
all_data_2 <-all_data_2[order(all_data_2$id,all_data_2$inspection_date.y,decreasing = T),]
joined_restaurant<-merge(restaurant_data2,all_data_2,by.x="id",by.y="id",all.x=T,allow.cartesian=T)
View(joined_restaurant) #
joined_restaurant<-joined_restaurant[order(joined_restaurant[,1],decreasing = T),]
joined_restaurant<-joined_restaurant[!duplicated(joined_restaurant),] #269199 unduplicated obs
joined_restaurant<-joined_restaurant%>%filter(!is.na(joined_restaurant$inspection_date)) #no NAs
#Filter historical investigations
joined_restaurant<-joined_restaurant%>%filter(inspection_date.y <inspection_date) #108082 obs
#group by id and inspection_date to construct features
#compute number of previous low inspections
num_previous_low_inspections<-joined_restaurant%>%group_by(id,inspection_date)%>%filter(score.y<14)%>%summarise(num_previous_low_inspections = n()) 
View(num_previous_low_inspections)
#compute number of previous median inspections
num_previous_med_inspections<-joined_restaurant%>%group_by(id,inspection_date)%>%filter(score.y>=14&score.y<28)%>%summarise(num_previous_med_inspections = n()) 
View(num_previous_med_inspections)
#compute number of previous high inspections
num_previous_high_inspections<-joined_restaurant%>%group_by(id,inspection_date)%>%filter(score.y>=28)%>%summarise(num_previous_high_inspections = n()) 
View(num_previous_high_inspections)
#compute number of previous closings
unique(joined_restaurant$action)
num_previous_closings<-joined_restaurant%>%group_by(id,inspection_date)%>%filter(action=="Establishment Closed by DOHMH.  Violations were cited in the following area(s) and those requiring immediate action were addressed."|action=="Establishment re-closed by DOHMH")%>%summarise(num_previous_closings= n()) 
View(num_previous_closings)
#generate features based on historical inspections
joined_restaurant_2<-merge(joined_restaurant,num_previous_low_inspections,by=c("id","inspection_date"),all.x=T)
joined_restaurant_2<-merge(joined_restaurant_2,num_previous_med_inspections,by=c("id","inspection_date"),all.x=T)
joined_restaurant_2<-merge(joined_restaurant_2,num_previous_high_inspections,by=c("id","inspection_date"),all.x=T)
joined_restaurant_2<-merge(joined_restaurant_2,num_previous_closings,by=c("id","inspection_date"),all.x=T)
View(joined_restaurant_2)
rm(joined_restaurant)
rm(num_previous_low_inspections)
rm(num_previous_med_inspections)
rm(num_previous_high_inspections)
rm(num_previous_closings)
# changing NA to 0
joined_restaurant_2 <- joined_restaurant_2 %>% replace(list = is.na(joined_restaurant_2), values = 0)
sum(is.na(joined_restaurant_2)) #all NAs have been changed
#Drop additional features
joined_restaurant_2 <- joined_restaurant_2 %>% select(id,outcome,borough,cuisine,inspection_date,inspection_year.x,month,weekday,num_previous_low_inspections,num_previous_med_inspections,num_previous_high_inspections,num_previous_closings)
#Drop duplicated obs
joined_restaurant_2<-joined_restaurant_2[!duplicated(joined_restaurant_2),] #39142 unduplicated obs
#Restrict joined_restaurant_2 to only the top 50 most common cuisines
unique_cuisine<-unique(joined_restaurant_2$cuisine)
top50 <- joined_restaurant_2 %>% 
  count(cuisine) %>% 
  arrange(desc(n)) %>% 
  slice(1:50) 
restaurant_data<-joined_restaurant_2%>%filter(joined_restaurant_2$cuisine%in%top50$cuisine) #108082 obs
unique(restaurant_data$cuisine) #38363 obs
restaurant_data<- dplyr::rename(restaurant_data, inspection_year = inspection_year.x) 
restaurant_data <-restaurant_data %>% select(outcome,borough,cuisine,inspection_year,month,weekday,num_previous_low_inspections,num_previous_med_inspections,num_previous_high_inspections,num_previous_closings)
write.csv(restaurant_data,file="restaurant_data.csv")
rm(joined_restaurant_2)

#D

View(restaurant_data)
#Create training set
train<-restaurant_data%>%filter(inspection_year==2015|inspection_year==2016)
#Create testing set
test<-restaurant_data%>%filter(inspection_year==2017)
#Fit a standard logistic regression model on the training set, predicting outcome as a function of only cuisine, borough, month, and weekday.
fit.logit <- glm(outcome ~cuisine+borough+month+weekday, 
           data = train, family = binomial(link = "logit"))
#Compute the AUC of this model on the test dataset.
test.prob <- predict(fit.logit, newdata = test, type = "response")
test.pred <- prediction(test.prob, test$outcome)
test.perf <- performance(test.pred,"auc")
auc<-100*test.perf@y.values[[1]]
auc
#60.16192

#E
# Fit a random forest model on train , predicting outcome as a function of cuisine,
# borough, month, weekday, and the four historical features created in Step C. Use 1000
# trees, but other settings can have default values.4 Compute the AUC of this model on the
# test dataset. How does the AUC of the random forest compare with the AUC of the
# logistic regression model?
str(restaurant_data)
#Transfer to factors 
for (i in 1:10){
  restaurant_data[,i] <- as.factor(restaurant_data[,i])
}
#Create training set
train<-restaurant_data%>%filter(inspection_year==2015|inspection_year==2016)
#Create testing set
test<-restaurant_data%>%filter(inspection_year==2017)
# Fit random forest model
fit.rf <- randomForest(outcome~.-inspection_year,data=train,ntree=1000)
#Compute the AUC of this model on the test dataset.
rf.prob <- predict(fit.rf, newdata = test, type = "prob")
threshold <- 0.1
test <- test %>% mutate(prediction = case_when(
  rf.prob[,2] < threshold ~ 0,
  rf.prob[,2] >= threshold ~ 1
))
table(test$prediction, test$outcome)
#sum(rf.prob[,2]>0.5)
#sum(test$outcome==1)
#rf.prob
rf.pred <- prediction(as.numeric(test$prediction), as.numeric(test$outcome))
rf.perf <- performance(rf.pred,"auc")
auc2<-100*rf.perf@y.values[[1]]
auc2
#57.57792