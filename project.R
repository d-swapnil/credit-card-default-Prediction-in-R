#PROJECT - CREDIT CARD DEFAULT PREDICTION
#Class: BUAN 6356.003
#Swapnil Deshpande - SXD220134

#To clear the console :
rm(list=ls())
cat("\014")

#Set desired working directory
setwd()


#                       PART I : LOAD PACKAGES AND DATA 

# Installing the required packages:
# To plot missing values, we need naniar.
install.packages("naniar")
library("naniar")
# To quickly create dummy variables and rows , we need fastDummies.
install.packages("fastDummies")
library("fastDummies")
# For classification and regression ,we need randomForest.
install.packages("randomForest")
library("randomForest")
# For machine learning, we need caret.
install.packages("caret")
library("caret")
# For data visualization, we need ggplot 
install.packages("ggplot2")
library("ggplot2")
# For performing logistic regression
install.packages("caTools") 
library(caTools)
#To create decision tree, we need rpart
install.packages("rpart")    
library(rpart)
#To plot decision tree, we need rpart.plot
install.packages("rpart.plot")    
library(rpart.plot)

#Reading the csv files and merging them :
application_record=read.csv("application_record.csv")
credit_record=read.csv("credit_record.csv")
creditcard = merge(x = credit_record, y = application_record, by = "ID",all.x=TRUE)
# Checking the number of observations in the merged dataset :
nrow(creditcard)

#Checking the balance of data :
balance_of_records = table(creditcard$STATUS)
prop.table(balance_of_records)
#We can conclude that it is a highly imbalanced dataset.

#                     PART II : EXPLORATORY DATA ANALYSIS

#We will start by checking for missing values and plotting them out.
vis_miss(creditcard,warn_large_data=F)

# We can see that apart from ID, MONTHS_BALANCE and STATUS, for all the other variables there is 26% of data redundancy, that means almost quarter part of the dataset contains missing values.

# Now, we have to replace the missing values with the median of each column.
creditcard$CODE_GENDER[is.na(creditcard$CODE_GENDER)] <- median(creditcard$CODE_GENDER, na.rm = T)
creditcard$FLAG_OWN_CAR[is.na(creditcard$FLAG_OWN_CAR)] <- median(creditcard$FLAG_OWN_CAR, na.rm = T) 
creditcard$FLAG_OWN_REALTY[is.na(creditcard$FLAG_OWN_REALTY)] <- median(creditcard$FLAG_OWN_REALTY, na.rm = T)
creditcard$CNT_CHILDREN[is.na(creditcard$CNT_CHILDREN)] <- median(creditcard$CNT_CHILDREN, na.rm = T)
creditcard$AMT_INCOME_TOTAL[is.na(creditcard$AMT_INCOME_TOTAL)] <- median(creditcard$AMT_INCOME_TOTAL, na.rm = T)
creditcard$NAME_INCOME_TYPE[is.na(creditcard$NAME_INCOME_TYPE)] <- median(creditcard$NAME_INCOME_TYPE, na.rm = T)
creditcard$NAME_EDUCATION_TYPE[is.na(creditcard$NAME_EDUCATION_TYPE)] <- median(creditcard$NAME_EDUCATION_TYPE, na.rm = T)
creditcard$NAME_FAMILY_STATUS[is.na(creditcard$NAME_FAMILY_STATUS)] <- median(creditcard$NAME_FAMILY_STATUS, na.rm = T)
creditcard$NAME_HOUSING_TYPE[is.na(creditcard$NAME_HOUSING_TYPE)] <- median(creditcard$NAME_HOUSING_TYPE, na.rm = T)
creditcard$DAYS_BIRTH[is.na(creditcard$DAYS_BIRTH)] <- median(creditcard$DAYS_BIRTH, na.rm = T)
creditcard$DAYS_EMPLOYED[is.na(creditcard$DAYS_EMPLOYED)] <- median(creditcard$DAYS_EMPLOYED, na.rm = T)
creditcard$FLAG_MOBIL[is.na(creditcard$FLAG_MOBIL)] <- median(creditcard$FLAG_MOBIL, na.rm = T)
creditcard$FLAG_WORK_PHONE[is.na(creditcard$FLAG_WORK_PHONE)] <- median(creditcard$FLAG_WORK_PHONE, na.rm = T)
creditcard$FLAG_PHONE[is.na(creditcard$FLAG_PHONE)] <- median(creditcard$FLAG_PHONE, na.rm = T)
creditcard$FLAG_EMAIL[is.na(creditcard$FLAG_EMAIL)] <- median(creditcard$FLAG_EMAIL, na.rm = T)
creditcard$OCCUPATION_TYPE[is.na(creditcard$OCCUPATION_TYPE)] <- median(creditcard$OCCUPATION_TYPE, na.rm = T)
creditcard$CNT_FAM_MEMBERS[is.na(creditcard$CNT_FAM_MEMBERS)] <- median(creditcard$CNT_FAM_MEMBERS, na.rm = T)

#We want to assign 1 to C,X,0 and 0 to 1,2,3,4,5. We can achieve this as follows:
# We first replace the values of C,X,0, with "z"
creditcard$STATUS[creditcard$STATUS == "C"] <- "z"
creditcard$STATUS[creditcard$STATUS == "X"] <- "z"
creditcard$STATUS[creditcard$STATUS == "0"] <- "z"
# Replacing 1,2,3,4,5 with 0.
creditcard$STATUS[creditcard$STATUS == "1"] <- 0
creditcard$STATUS[creditcard$STATUS == "2"] <- 0
creditcard$STATUS[creditcard$STATUS == "3"] <- 0
creditcard$STATUS[creditcard$STATUS == "4"] <- 0
creditcard$STATUS[creditcard$STATUS == "5"] <- 0
#Now, we reset the z values back to '1':
creditcard$STATUS[creditcard$STATUS == "z"] <- 1
#Now, we convert the status column to numeric form:
creditcard$STATUS=as.numeric(creditcard$STATUS)


# One hot encoding for CODE_GENDER (M as 1 AND F as 0) 
creditcard$CODE_GENDER[creditcard$CODE_GENDER == "M"] <-1
creditcard$CODE_GENDER[creditcard$CODE_GENDER == "F"] <-0
creditcard$CODE_GENDER=as.numeric(creditcard$CODE_GENDER)
# One hot encoding for FLAG_OWN_CAR (Y as 1 AND N as 0)
creditcard$FLAG_OWN_CAR[creditcard$FLAG_OWN_CAR == "Y"] <-1
creditcard$FLAG_OWN_CAR[creditcard$FLAG_OWN_CAR == "N"] <-0
creditcard$FLAG_OWN_CAR=as.numeric(creditcard$FLAG_OWN_CAR)
# One hot encoding for FLAG_OWN_REALTY (Y as 1 AND N as 0)
creditcard$FLAG_OWN_REALTY[creditcard$FLAG_OWN_REALTY == "Y"] <-1
creditcard$FLAG_OWN_REALTY[creditcard$FLAG_OWN_REALTY == "N"] <-0
creditcard$FLAG_OWN_REALTY=as.numeric(creditcard$FLAG_OWN_REALTY)


creditcard$STATUS <- as.factor(creditcard$STATUS)

#Now , for the. variables that have more than 2 categories, we use the fastdummies package to quickly create dummies:
creditcard=dummy_cols(creditcard,select=c("NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE","OCCUPATION_TYPE"))

colnames(creditcard)
# We will now rename the column names having spaces in their names

colnames(creditcard)[21]="NAME_INCOME_TYPE_Commercial_associate"
colnames(creditcard)[23]="NAME_INCOME_TYPE_State_servant"
colnames(creditcard)[26]="NAME_EDUCATION_TYPE_Academic_degree"
colnames(creditcard)[27]="NAME_EDUCATION_TYPE_Higher_education"
colnames(creditcard)[28]="NAME_EDUCATION_TYPE_Incomplete_higher"
colnames(creditcard)[29]="NAME_EDUCATION_TYPE_Lower_secondary"
colnames(creditcard)[30]="NAME_EDUCATION_TYPE_Secondary_secondary_special"
colnames(creditcard)[31]="NAME_FAMILY_STATUS_Civil_marriage"
colnames(creditcard)[26]="NAME_EDUCATION_TYPE_Academic_degree"
colnames(creditcard)[34]="NAME_FAMILY_STATUS_Single_not_married"
colnames(creditcard)[36]="NAME_HOUSING_TYPE_Co_op_apartment"
colnames(creditcard)[37]="NAME_HOUSING_TYPE_House_apartment"
colnames(creditcard)[38]="NAME_HOUSING_TYPE_Municipal_apartment"
colnames(creditcard)[39]="NAME_HOUSING_TYPE_Office_apartment"
colnames(creditcard)[40]="NAME_HOUSING_TYPE_Rented_apartment"
colnames(creditcard)[41]="NAME_HOUSING_TYPE_With_parents"
colnames(creditcard)[44]="OCCUPATION_TYPE_Cleaning_staff"
colnames(creditcard)[45]="OCCUPATION_TYPE_Cooking_staff"
colnames(creditcard)[46]="OCCUPATION_TYPE_Core_staff"
colnames(creditcard)[48]="OCCUPATION_TYPE_High_skill_tech_staff"
colnames(creditcard)[49]="OCCUPATION_TYPE_HR_staff"
colnames(creditcard)[50]="OCCUPATION_TYPE_IT_staff"
colnames(creditcard)[52]="OCCUPATION_TYPE_Low_skill_Laborers"
colnames(creditcard)[54]="OCCUPATION_TYPE_Medicine_staff"
colnames(creditcard)[55]="OCCUPATION_TYPE_Private_service_staff"
colnames(creditcard)[56]="OCCUPATION_TYPE_Realty_agents"
colnames(creditcard)[57]="OCCUPATION_TYPE_Sales_staff"
colnames(creditcard)[59]="OCCUPATION_TYPE_Security_staff"
colnames(creditcard)[60]="OCCUPATION_TYPE_Waiters_barmen_staff"


#                      PART III - DATA VISUALIZATION


#Bar chart for approval rate vis a vis no.of children
h <- ggplot(creditcard) + 
  stat_count(aes(x=CNT_CHILDREN, y=..prop.., group=STATUS,fill=STATUS),alpha=0.65) + 
  facet_grid(.~STATUS) +
  scale_fill_manual(values=c("red", "darkgreen"), guide=FALSE)+
  geom_text(aes(x=CNT_CHILDREN, y=..prop.., 
                group=STATUS, label=paste0(round(..prop..*100, 1), "%")), 
            stat="count", vjust=-1) + theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
h

#Bar chart for approval rate vis a vis education type
i <- ggplot(creditcard) + 
  stat_count(aes(x=NAME_EDUCATION_TYPE, y=..prop.., group=STATUS,fill=STATUS),alpha=0.65) + 
  facet_grid(.~STATUS) +
  scale_fill_manual(values=c("red", "darkgreen"), guide=FALSE)+
  geom_text(aes(x=NAME_EDUCATION_TYPE, y=..prop.., 
                group=STATUS, label=paste0(round(..prop..*100, 1), "%")), 
            stat="count", vjust=-1) + theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
i

#Bar chart for approval rate vis a vis family status
j <- ggplot(creditcard) + 
  stat_count(aes(x=NAME_FAMILY_STATUS, y=..prop.., group=STATUS,fill=STATUS),alpha=0.65) + 
  facet_grid(.~STATUS) +
  scale_fill_manual(values=c("red", "darkgreen"), guide=FALSE)+
  geom_text(aes(x=NAME_FAMILY_STATUS, y=..prop.., 
                group=STATUS, label=paste0(round(..prop..*100, 1), "%")), 
            stat="count", vjust=-1) + theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
j


#Bar chart for approval rate vis a vis income type
k <- ggplot(creditcard) + 
  stat_count(aes(x=NAME_INCOME_TYPE, y=..prop.., group=STATUS,fill=STATUS),alpha=0.65) + 
  facet_grid(.~STATUS) +
  scale_fill_manual(values=c("red", "darkgreen"), guide=FALSE)+
  geom_text(aes(x=NAME_INCOME_TYPE, y=..prop.., 
                group=STATUS, label=paste0(round(..prop..*100, 1), "%")), 
            stat="count", vjust=-1) + theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
k

#Bar chart for approval rate vis a vis months balance
l <- ggplot(creditcard) + 
  stat_count(aes(x=MONTHS_BALANCE, y=..prop.., group=STATUS,fill=STATUS),alpha=0.65) + 
  facet_grid(.~STATUS) +
  scale_fill_manual(values=c("red", "darkgreen"), guide=FALSE)+
  geom_text(aes(x=MONTHS_BALANCE, y=..prop.., 
                group=STATUS, label=paste0(round(..prop..*100, 1), "%")), 
            stat="count", vjust=-1) + theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
l

#                       PART IV - MODELING TECHNIQUES

#Now, we can drop the original categorical columns :
drop=c("NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE","OCCUPATION_TYPE","ID")
creditcard=creditcard[,!names(creditcard) %in% drop]

#Select a random sample of 100000 samples from the dataset
x=creditcard[c(700000:800000), ]


######splitting the data into training and testing samples
index=sample(nrow(x),0.70*nrow(x))
trainingData=x[index,]
validationData=x[-index,]

######Assigning class weights to correct the imbalanced data set
class_weights <- ifelse(trainingData$STATUS == 1, 0.015, 0.985)


#.  MODEL 1 - LOGISTIC REGRESSION
set.seed(123)
logmodel=glm(STATUS ~ . ,data=trainingData,family="binomial", weights = class_weights)

#########making predictions
predictions=predict(logmodel,validationData,type="response")
predictions=ifelse(predictions>0.5,0,1)
confusionMatrix(as.factor(predictions),as.factor(validationData$STATUS))



#.  MODEL 2 - DECISION TREE
set.seed(123)
tree_model <- rpart(STATUS ~ ., data = trainingData, control=rpart.control(xval=0, minsplit=500), parms=list(split="gini"), weights = class_weights)
rpart.plot(tree_model, type=1, main = "Credit Card Fraud Prediction")
#making predictions
predictions=predict(tree_model,validationData,type="class")
confusionMatrix(as.factor(predictions),as.factor(validationData$STATUS))

#.  MODEL 3 - RANDOM FOREST CLASSIFIER 
set.seed(123)
rf <- randomForest(STATUS ~ ., data = trainingData, ntree = 800,
                   mtry = 8, nodesize = 30, importance = TRUE, weights = class_weights)
#Plotting the significant predictors :
varImpPlot(rf,type=1)

#Making predictions
predictions=predict(rf,validationData)
#predictions=ifelse(predictions>0.50,1,0)
confusionMatrix(as.factor(predictions),as.factor(validationData$STATUS))

#--------END-------------#






