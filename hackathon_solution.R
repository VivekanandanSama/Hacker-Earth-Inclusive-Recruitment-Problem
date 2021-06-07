setwd("F:/hackathon/hackerearth_amazon/")
train_data <- read.csv("Train.csv", stringsAsFactors = F)

summary(train_data)

for(i in 3:length(train_data))
{
  if(class(train_data[,i]) == "character")
  {
    print(paste(names(train_data)[i],"- unique values = ", length(unique(train_data[,i]))))
    print(table(train_data[,i]))
  
    }
  
}

model_data <- train_data
model_data$BiasInfluentialFactor[model_data$BiasInfluentialFactor == ""] <- "No_bias"

sum(is.na(model_data))
sum(model_data == "")

library(lubridate)
library(data.table)
library(dplyr)

########## Language as factor
#model_data$LanguageOfCommunication <- as.factor(model_data$LanguageOfCommunication)

######### Gender as factor
#model_data$Gender <- as.factor(model_data$Gender)

######### Job profile exploration
table(model_data$JobProfileIDApplyingFor)
model_data$JobProfileIDApplyingFor <- as.numeric(gsub("JR","",model_data$JobProfileIDApplyingFor))
model_data$JobProfileIDApplyingFor <- round(model_data$JobProfileIDApplyingFor/4000,2)  ##### scaling to the same scale as age and experience 

######## treating education column
table(model_data$HighestDegree)
model_data$degree_type <- ifelse(model_data$HighestDegree %like% "Tech" | model_data$HighestDegree %in% c("MS", "PhD"), "tech",
                                 ifelse(model_data$HighestDegree %in% c("BCA", "MCA"), "non_tech", "mgmt"))

model_data$edu_qual <- ifelse(model_data$HighestDegree == "PhD", "doctorate",
                              ifelse(model_data$HighestDegree %like% "M", "masters", "bachelors"))

#model_data$edu_qual <- as.factor(model_data$edu_qual)
model_data$edu_qual_num <- ifelse(model_data$edu_qual == "bachelors", 4,
                                  ifelse(model_data$edu_qual == "masters", 6 , 10))

######### Degree Branch exploration
table(model_data$DegreeBranch)
model_data$DegreeBranch[model_data$DegreeBranch %like% "Elec"] <- "Electrical Electronics" 
model_data$DegreeBranch <- gsub("[[:lower:]]|\\s{1,}","", model_data$DegreeBranch)  ## converting into abbreviation
#model_data$DegreeBranch <- as.factor(model_data$DegreeBranch)

######### Graduating institute into numeric
model_data$GraduatingInstitute <- as.numeric(gsub("Tier ","",model_data$GraduatingInstitute))

######## checking for break years using graduation year
model_data$years_since_grad <- year(Sys.Date()) - model_data$GraduationYear
model_data$possible_break <- ifelse(model_data$YearsOfExperince < model_data$years_since_grad, "yes", "no")
table(model_data$possible_break)
model_data$possible_break <- NULL
model_data$years_since_grad <- NULL

######### Exploring current designation and company
table(model_data$CurrentDesignation)
table(model_data$CurrentCompanyType)

model_data$current_company_scale <- ifelse(model_data$CurrentCompanyType == "Startup", 1,
                                           ifelse(model_data$CurrentCompanyType == "MidSized",2,3))


######### Exploring department
table(model_data$DepartmentInCompany)

cat_columns <- c()
num_columns <- c()

for(i in 3:length(model_data))
{
  if( class(model_data[,i]) == "character")
  {
    cat_columns <- c(cat_columns,i)
  }
  if( (class(model_data[,i]) == "integer") | (class(model_data[,i]) == "numeric"))
  {
    num_columns <- c(num_columns,i)
  }
}

cat_data <- model_data[,cat_columns]
cat_data$BiasInfluentialFactor <- NULL

num_data <- model_data[,num_columns]
num_data$FitmentPercent <- NULL

for(i in 1:length(cat_data))
{
  cat_data[,i] <- as.factor(cat_data[,i])
}

for(i in 1:length(num_data))
{
  num_data[,i] <- as.numeric(num_data[,i])
}

library("corrplot")
table(cat_data[,1],cat_data[,2])
a <- chisq.test(cat_data[,1],cat_data[,2])[["p.value"]]
cor(num_data[,1],num_data[,2])

pairwise_chisq_test <- as.data.frame(matrix(nrow = length(cat_data),ncol = length(cat_data)))
names(pairwise_chisq_test) <- names(cat_data)
row.names(pairwise_chisq_test) <- names(cat_data)

for(i in 1:length(cat_data))
{
  for(j in 1:length(cat_data))
  {
    if(i<=j)
    {
    pairwise_chisq_test[i,j] <- round(chisq.test(cat_data[,i],cat_data[,j])[["p.value"]],2)
    } else {
      pairwise_chisq_test[i,j] <- NA
    }
  }
}


########## current company type and highest degree are highly dependent on each other. 
########## So, drop highest degree and use edu qual and degree type instead. 
########## edu qual / degree type are derived from highest degree anyway. 
########## There also seems to be a dependency between gender and current company type, gender and department

corrplot(cor(num_data), method = "circle", type = "upper")

########## age, experience, graduation year are correlated, others, not so much
########## expected ctc and current ctc are also correlated. Maybe combine both into one variable like hike%?

model_data$hike_percent <- round((model_data$ExpectedCTC - model_data$CurrentCTC)/model_data$CurrentCTC * 100,2)

model_data$GraduationYear <- NULL
model_data$Age <- NULL

cor(model_data$CurrentCTC, model_data$hike_percent)
model_data$CurrentCTC <- NULL

######## re-establishing cat and num columns

cat_columns <- c()
num_columns <- c()

for(i in 3:length(model_data))
{
  if( class(model_data[,i]) == "character")
  {
    cat_columns <- c(cat_columns,i)
  }
  if( (class(model_data[,i]) == "integer") | (class(model_data[,i]) == "numeric"))
  {
    num_columns <- c(num_columns,i)
  }
}

cat_data <- model_data[,cat_columns]
cat_data$BiasInfluentialFactor <- NULL

num_data <- model_data[,num_columns]
num_data$FitmentPercent <- NULL

for(i in 1:length(cat_data))
{
  cat_data[,i] <- as.factor(cat_data[,i])
}

for(i in 1:length(num_data))
{
  num_data[,i] <- as.numeric(num_data[,i])
}

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

anova_table <- data.frame(variables = names(cat_data),
                          y_var_anova = rep(NA,length(cat_data)))

#summary(aov(model_data$FitmentPercent ~ cat_data[,1]))[[1]][5]

for(i in 1:length(cat_data))
{
  anova_table[i,2] <- round(summary(aov(model_data$FitmentPercent ~ cat_data[,i]))[[1]][1,5],3)
}

######## from this table, we can see that language of communication, 
######## degree branch, current designation, department aren't that significant

cor_table <- data.frame(variables = names(num_data),
                        correlation = rep(NA,length(num_data)))

#summary(aov(model_data$FitmentPercent ~ cat_data[,1]))[[1]][5]

for(i in 1:length(num_data))
{
  cor_table[i,2] <- round(cor(model_data$FitmentPercent, num_data[,i]),3)
}

######## not much correlation between any numerical variable and fitment percentage
######## maybe fitment percent is a sigmoid function. 
######## we'll create an alternate score with inverse sigmoid function and try

summary(aov(model_data$FitmentPercent ~ as.factor(model_data$GraduatingInstitute)))
## maybe graduating institute works better as categorical variable

model_data$new_fitment_score <- round(-(log(1/(model_data$FitmentPercent/100)-1)),3)

###################### check correlation and anova with new scores

anova_table$new_score_anova <- NA
cor_table$new_score_correlation <- NA

for(i in 1:length(cat_data))
{
  anova_table[i,3] <- round(summary(aov(model_data$new_fitment_score ~ cat_data[,i]))[[1]][1,5],3)
}

for(i in 1:length(num_data))
{
  cor_table[i,3] <- round(cor(model_data$new_fitment_score, num_data[,i]),3)
}

######## while there isn't much difference in the correlation table, 
######## anova has a significant difference. So let's model for the new scores and 
######## then convert it into old scores. 

summary(aov(model_data$new_fitment_score ~ as.factor(model_data$GraduatingInstitute)))
######## Yeah, graduating institute is better as a factor variable
######## And, current company type factor is better than numeric company scale
######## similarly edu_qual more than edu_qual_num

######## plot individually to check if there is any obvious  non linear dependence 
for(i in 1:length(num_data))
{
  plot(model_data$new_fitment_score, num_data[,i], 
       ylab = names(num_data)[i], xlab = "new fitment score")
}

unique(model_data$BiasInfluentialFactor)

model_data1 <- cbind(EmpID = model_data$EmpID, cat_data, num_data, 
                     new_fitment_score = model_data$new_fitment_score)


model_data1$current_company_scale <- NULL
model_data1$edu_qual_num <- NULL
model_data1$GraduatingInstitute <- paste0("Tier_",model_data1$GraduatingInstitute)
model_data1$GraduatingInstitute <- factor(model_data1$GraduatingInstitute,
                                          levels = c("Tier_1", "Tier_2", "Tier_3"),
                                          labels = c("Tier_1", "Tier_2", "Tier_3"))
names(model_data)[!names(model_data) %in% names(model_data1)]

row.names(model_data1) <- model_data1$EmpID  
model_data1$EmpID <- NULL

fit1 <- lm(new_fitment_score~
             Gender +
             # HighestDegree +
             #DegreeBranch +
             MartialStatus +
             CurrentCompanyType +
             # DepartmentInCompany +
             #degree_type +
             edu_qual +
             # GraduatingInstitute +
             # LatestDegreeCGPA +
             # YearsOfExperince +
             ExpectedCTC +
             EmpScore +
             # TotalLeavesTaken +
             hike_percent,
           data = model_data1)

summary(fit1)

############ So these appear to be significant variables

library(xgboost)
library(caret)
require(Matrix)

############## Throw in all variables for xgboost
set.seed(1729)
train_indices <- createDataPartition(model_data1$new_fitment_score, p = 0.8, list = F)
model_train1 <- model_data1[train_indices,]
model_test1 <- model_data1[-train_indices,]

model_train_x1 <- model_train1[,-which(names(model_train1) == "new_fitment_score")]
model_train_y1 <- model_train1$new_fitment_score

model_test_x1 <- model_test1[,-which(names(model_test1) == "new_fitment_score")]
model_test_y1 <- model_test1$new_fitment_score

train_encoded1 <- sparse.model.matrix(~., data = model_train_x1)[,-1]
test_encoded1 <- sparse.model.matrix(~., data = model_test_x1)[,-1]

xgb_train1 <- xgb.DMatrix(data = train_encoded1, label = model_train_y1)
xgb_test1 <- xgb.DMatrix(data = test_encoded1, label = model_test_y1)

xgb_model1 <- xgboost(data = xgb_train1, max.depth = 10, nrounds = 1000,
                     subsample = 1, colsample_bytree = 1,
                    print_every_n = 50, early_stopping_rounds = 5)

pred_y1 <-  predict(xgb_model1, xgb_test1)

mse1 <- mean((model_test_y1 - pred_y1)^2)
mse1
mae1 <- caret::MAE(model_test_y1, pred_y1)
mae1
rmse1 <- caret::RMSE(model_test_y1, pred_y1)
rmse1


############## Throw in only significant variables from linear regression
set.seed(1729)
train_indices <- createDataPartition(model_data1$new_fitment_score, p = 0.8, list = F)
model_data2 <- model_data1[,all.vars(formula(fit1))]

model_train2 <- model_data2[train_indices,]
model_test2 <- model_data2[-train_indices,]

model_train_x2 <- model_train2[,-which(names(model_train2) == "new_fitment_score")]
model_train_y2 <- model_train2$new_fitment_score

model_test_x2 <- model_test2[,-which(names(model_test2) == "new_fitment_score")]
model_test_y2 <- model_test2$new_fitment_score

train_encoded2 <- sparse.model.matrix(~., data = model_train_x2)[,-1]
test_encoded2 <- sparse.model.matrix(~., data = model_test_x2)[,-1]

xgb_train2 <- xgb.DMatrix(data = train_encoded2, label = model_train_y2)
xgb_test2 <- xgb.DMatrix(data = test_encoded2, label = model_test_y2)

xgb_model2 = xgboost(data = xgb_train2, max.depth = 12, nrounds = 500,
                     subsample = 1, colsample_bytree = 1,
                     print_every_n = 50, early_stopping_rounds = 5)

pred_y2 <-  predict(xgb_model2, xgb_test2)

mse2 <- mean((model_test_y2 - pred_y2)^2)
mse2
mae2 <- caret::MAE(model_test_y2, pred_y2)
mae2
rmse2 <- caret::RMSE(model_test_y2, pred_y2)
rmse2

########### xgb_model1 performs better. 

############## Throw in only variables from Bias Influential factors

set.seed(1729)
train_indices <- createDataPartition(model_data1$new_fitment_score, p = 0.8, list = F)
model_data3 <- model_data1[,names(model_data1) %in% c("LanguageOfCommunication", unique(model_data$BiasInfluentialFactor), "new_fitment_score")]

model_train3 <- model_data3[train_indices,]
model_test3 <- model_data3[-train_indices,]

model_train_x3 <- model_train3[,-which(names(model_train3) == "new_fitment_score")]
model_train_y3 <- model_train3$new_fitment_score

model_test_x3 <- model_test3[,-which(names(model_test3) == "new_fitment_score")]
model_test_y3 <- model_test3$new_fitment_score

train_encoded3 <- sparse.model.matrix(~., data = model_train_x3)[,-1]
test_encoded3 <- sparse.model.matrix(~., data = model_test_x3)[,-1]

xgb_train3 <- xgb.DMatrix(data = train_encoded3, label = model_train_y3)
xgb_test3 <- xgb.DMatrix(data = test_encoded3, label = model_test_y3)

xgb_model3 = xgboost(data = xgb_train3, max.depth = 12, nrounds = 1000,
                     subsample = 1, colsample_bytree = 1,
                     print_every_n = 50, early_stopping_rounds = 5)

pred_y3 <-  predict(xgb_model3, xgb_test3)

mse3 <- mean((model_test_y3 - pred_y3)^2)
mse3
mae3 <- caret::MAE(model_test_y3, pred_y3)
mae3
rmse3 <- caret::RMSE(model_test_y3, pred_y3)
rmse3

########### xgb_model1 > xgb2 > xgb3

###################################################################################
########### In order to predict the bias influential factor,
########### treat the problem as a multi class classification
########### using the same predictor variables. 

chi_sq_table <- data.frame(variables = names(cat_data),
                           bias_chi_sq = rep(NA,length(cat_data)))

anova_table2 <- data.frame(variables = names(num_data),
                            bias_anova = rep(NA,length(num_data)))

model_data$BiasInfluentialFactor <- as.factor(model_data$BiasInfluentialFactor)

for(i in 1:length(cat_data))
{
  chi_sq_table[i,2] <- round(chisq.test(model_data$BiasInfluentialFactor,cat_data[,i])[["p.value"]],2)
}

########### Wow, a lot of related variables

for(i in 1:length(num_data))
{
  anova_table2[i,2] <- round(summary(aov(num_data[,i]~model_data$BiasInfluentialFactor))[[1]][1,5],3)
}

########### A lot of significant variables


model_data4 <- cbind(EmpID = model_data$EmpID, cat_data, num_data, 
                     BiasInfluentialFactor = model_data$BiasInfluentialFactor)
model_data4$BiasInfluentialFactor <- as.factor(model_data4$BiasInfluentialFactor)

model_data4$current_company_scale <- NULL
model_data4$edu_qual_num <- NULL
model_data4$GraduatingInstitute <- paste0("Tier_",model_data4$GraduatingInstitute)
model_data4$GraduatingInstitute <- factor(model_data4$GraduatingInstitute,
                                          levels = c("Tier_1", "Tier_2", "Tier_3"),
                                          labels = c("Tier_1", "Tier_2", "Tier_3"))
names(model_data)[!names(model_data) %in% names(model_data4)]

row.names(model_data4) <- model_data4$EmpID  
model_data4$EmpID <- NULL

############## Throw in all variables for xgboost

set.seed(1729)
train_indices <- createDataPartition(model_data4$BiasInfluentialFactor, p = 0.8, list = F)
model_train4 <- model_data4[train_indices,]
model_test4 <- model_data4[-train_indices,]

model_train_x4 <- model_train4[,-which(names(model_train4) == "BiasInfluentialFactor")]
model_train_y4 <- as.numeric(model_train4$BiasInfluentialFactor)-1

model_test_x4 <- model_test4[,-which(names(model_test4) == "BiasInfluentialFactor")]
model_test_y4 <- as.numeric(model_test4$BiasInfluentialFactor)-1

train_encoded4 <- sparse.model.matrix(~., data = model_train_x4)[,-1]
test_encoded4 <- sparse.model.matrix(~., data = model_test_x4)[,-1]

xgb_train4 <- xgb.DMatrix(data = as.matrix(train_encoded4), label = model_train_y4)
xgb_test4 <- xgb.DMatrix(data = as.matrix(test_encoded4), label = model_test_y4)

no_of_classes = length(unique(model_data$BiasInfluentialFactor))

xgb_model4 = xgboost(data = xgb_train4, max.depth = 10, nrounds = 1000,
                     eval_metric = "merror", objective = "multi:softprob",
                     subsample = 1, colsample_bytreee = 1, 
                     num_class = no_of_classes, gamma = 0.5,
                     print_every_n = 10, early_stopping_rounds = 10)

pred_y4 <- as.data.frame(round(predict(xgb_model4, newdata = xgb_test4, reshape = T),3))
colnames(pred_y4) = levels(model_data4$BiasInfluentialFactor)

pred_y4$predicted_label <- as.vector(apply(pred_y4, MARGIN = 1, 
                                       FUN = function(x)
                                         {
                                         names(pred_y4)[which.max(x)]
                                         }))
confusion_matrix4 <- table(predicted = pred_y4$predicted_label, actual = as.character(model_test4$BiasInfluentialFactor))
accuracy4 <- round(sum(diag(confusion_matrix4))/sum(confusion_matrix4)*100,2)
accuracy4
############## Throw in significant variables for xgboost
model_data5 <- model_data4[,names(model_data4)%in% c(as.character(anova_table2$variables[anova_table2$bias_anova < 0.1]), as.character(chi_sq_table$variables[chi_sq_table$bias_chi_sq<0.1]),"BiasInfluentialFactor")]

set.seed(1729)
train_indices <- createDataPartition(model_data5$BiasInfluentialFactor, p = 0.8, list = F)
model_train5 <- model_data5[train_indices,]
model_test5 <- model_data5[-train_indices,]

model_train_x5 <- model_train5[,-which(names(model_train5) == "BiasInfluentialFactor")]
model_train_y5 <- as.numeric(model_train5$BiasInfluentialFactor)-1

model_test_x5 <- model_test5[,-which(names(model_test5) == "BiasInfluentialFactor")]
model_test_y5 <- as.numeric(model_test5$BiasInfluentialFactor)-1

train_encoded5 <- sparse.model.matrix(~., data = model_train_x5)[,-1]
test_encoded5 <- sparse.model.matrix(~., data = model_test_x5)[,-1]

xgb_train5 <- xgb.DMatrix(data = as.matrix(train_encoded5), label = model_train_y5)
xgb_test5 <- xgb.DMatrix(data = as.matrix(test_encoded5), label = model_test_y5)

no_of_classes = length(unique(model_data$BiasInfluentialFactor))

xgb_model5 = xgboost(data = xgb_train5, max.depth = 10, nrounds = 1000,
                     eval_metric = "merror", objective = "multi:softprob",
                     subsample = 1, colsample_bytreee = 1, 
                     num_class = no_of_classes, gamma = 0.5,
                     print_every_n = 10, early_stopping_rounds = 10)

pred_y5 <- as.data.frame(round(predict(xgb_model5, newdata = xgb_test5, reshape = T),3))
colnames(pred_y5) = levels(model_data5$BiasInfluentialFactor)

pred_y5$predicted_label <- as.vector(apply(pred_y5, MARGIN = 1, 
                                           FUN = function(x)
                                           {
                                             names(pred_y5)[which.max(x)]
                                           }))
confusion_matrix5 <- table(predicted = pred_y5$predicted_label, actual = as.character(model_test5$BiasInfluentialFactor))
accuracy5 <- round(sum(diag(confusion_matrix5))/sum(confusion_matrix5)*100,2)
accuracy5

##### xgb4 > xgb5 in accuracy

###################################################################################################################
######### Applying on real test data
real_test <- read.csv("Test.csv", stringsAsFactors = F)
row.names(real_test) <- real_test$EmpID
real_test$EmpID <- NULL
real_test$EmpName <- NULL
######### Converting the test data into proper format
real_test$LanguageOfCommunication <- as.factor(real_test$LanguageOfCommunication)
real_test$Gender <- as.factor(real_test$Gender)
real_test$JobProfileIDApplyingFor <- round(as.numeric(gsub("JR","",real_test$JobProfileIDApplyingFor))/4000,2)
real_test$HighestDegree <- as.factor(real_test$HighestDegree)
real_test$degree_type <- as.factor(ifelse(real_test$HighestDegree %like% "Tech" | real_test$HighestDegree %in% c("MS", "PhD"), "tech",
                                 ifelse(real_test$HighestDegree %in% c("BCA", "MCA"), "non_tech", "mgmt")))

real_test$edu_qual <- as.factor(ifelse(real_test$HighestDegree == "PhD", "doctorate",
                              ifelse(real_test$HighestDegree %like% "M", "masters", "bachelors")))

#model_data$edu_qual <- as.factor(model_data$edu_qual)
real_test$edu_qual_num <- ifelse(real_test$edu_qual == "bachelors", 4,
                                  ifelse(real_test$edu_qual == "masters", 6 , 10))

real_test$DegreeBranch[real_test$DegreeBranch %like% "Elec"] <- "Electrical Electronics" 
real_test$DegreeBranch <- as.factor(gsub("[[:lower:]]|\\s{1,}","", real_test$DegreeBranch))  ## converting into abbreviation

real_test$GraduatingInstitute <- as.numeric(gsub("Tier ","",real_test$GraduatingInstitute))
real_test$GraduatingInstitute <- paste0("Tier_",real_test$GraduatingInstitute)
real_test$GraduatingInstitute <- factor(real_test$GraduatingInstitute,
                                          levels = c("Tier_1", "Tier_2", "Tier_3"),
                                          labels = c("Tier_1", "Tier_2", "Tier_3"))

real_test$hike_percent <- round((real_test$ExpectedCTC - real_test$CurrentCTC)/real_test$CurrentCTC * 100,2)

real_test$MartialStatus <- as.factor(real_test$MartialStatus)
real_test$CurrentDesignation <- as.factor(real_test$CurrentDesignation)
real_test$CurrentCompanyType <- as.factor(real_test$CurrentCompanyType)
real_test$DepartmentInCompany <- as.factor(real_test$DepartmentInCompany)

real_test$Age <- NULL
real_test$GraduationYear <- NULL
real_test$CurrentCTC <- NULL
real_test$edu_qual_num <- NULL
real_test <- real_test[,names(model_data1)[1:(length(model_data1)-1)]]

real_test_encoded <- sparse.model.matrix(~., data = real_test)[,-1]
real_test_dense <- xgb.DMatrix(data = (real_test_encoded))
#colnames(real_test_dense)[(colnames(real_test_dense) != colnames(xgb_train1))]

real_test$real_y <-  predict(xgb_model1, real_test_dense)
real_test$FitmentPercent <-  round((1/(1+exp(-real_test$real_y)))*100,2)

pred_real <- as.data.frame(round(predict(xgb_model4, newdata = real_test_dense, reshape = T),3))
colnames(pred_real) = levels(model_data4$BiasInfluentialFactor)

pred_real$BiasInfluentialFactor <- as.vector(apply(pred_real, MARGIN = 1, 
                                           FUN = function(x)
                                           {
                                             names(pred_real)[which.max(x)]
                                           }))

real_test$EmpID <- row.names(real_test)
pred_real$EmpID <- real_test$EmpID

final_prediction <- merge(pred_real[,c("EmpID", "BiasInfluentialFactor")], real_test[,c("EmpID", "FitmentPercent")], by = "EmpID")
write.csv(final_prediction, file = "final_submission.csv", row.names = F)
