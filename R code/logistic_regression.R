
#### read data ####
train <- read.csv("train-1.csv")
test <- read.csv("test.csv")
library("ggplot2")
library("reshape2")
library("pscl")
library("car")
library("DMwR")
source("performance.R")
source("cutoff.R")
source("analysis.R")
source("outlier.R")

#### create new predictors ####
train$avgordamt <- train$totsale/train$totord
train$purrate <- train$totord/train$tof
train$breadth <- rowSums(train[,4:10])
train$freq.buyer <- ifelse(train$purrate > 0.1, 1, 0)

test$avgordamt <- test$totsale/test$totord
test$purrate <- test$totord/test$tof
test$breadth <- rowSums(test[,4:10])
test$freq.buyer <- ifelse(test$purrate > 0.1, 1, 0)

length(which(train$targamnt<0))
train <- train[which(train$targamnt>=0),]

train$sale <- ifelse(train$targamnt>0, 1, 0)
train <- within(train, rm(custno, targamnt))

test$sale <- ifelse(test$targamnt>0, 1, 0)

#### remove outliers ####
index0 <- outlier.index(train$totsale)
index1 <- outlier.index(train$salcls1)
index2 <- outlier.index(train$salcls2)
index3 <- outlier.index(train$salcls3)
index4 <- outlier.index(train$salcls4)
index5 <- outlier.index(train$salcls5)
index6 <- outlier.index(train$salcls6)
index7 <- outlier.index(train$salcls7)
index <- Reduce(union, list(index0, index1, index2, index3, index4, index5, index6, index7))
train <- train[-index,]
train <- train[which(train$totsale>0),]
boxplot(train$recmon)
boxplot(train$tof)
library("outliers")
outlier(train$tof)
grubbs.test(train$tof)
boxplot(train$totord)
grubbs.test(train$totord)
hist(train$totord)
summary(train$totord[which(train$totord>100)])
length(train$totord[which(train$totord==123)])
train <- train[which(train$totord < 123),]
boxplot(train$totsale)
outlier(train$totsale)
grubbs.test(train$totsale)
hist(train$totsale)
summary(train$totsale[which(train$totsale>5000)])

#### smote ####
table(train$sale)
train$sale <- as.factor(train$sale)
set.seed(571)
train_smote <- SMOTE(sale ~., data = train, perc.over = 1800, perc.under = 0)
train_smote <- rbind(subset(train, sale == "0"), train_smote)
smote_index <- sample(1:nrow(train_smote), nrow(train_smote), replace = F)
train_smote <- train_smote[smote_index,]
rm(smote_index)
rm(index)

# round
train_smote$recmon <- round(train_smote$recmon)
train_smote$tof <- round(train_smote$tof, digits = 3)
train_smote$ordcls1 <- round(train_smote$ordcls1)
train_smote$ordcls2 <- round(train_smote$ordcls2)
train_smote$ordcls3 <- round(train_smote$ordcls3)
train_smote$ordcls4 <- round(train_smote$ordcls4)
train_smote$ordcls5 <- round(train_smote$ordcls5)
train_smote$ordcls6 <- round(train_smote$ordcls6)
train_smote$ordcls7 <- round(train_smote$ordcls7)
train_smote$ord185 <- ceiling(train_smote$ord185)
train_smote$ord285 <- ceiling(train_smote$ord285)
train_smote$ord385 <- ceiling(train_smote$ord385)
train_smote$ord485 <- ceiling(train_smote$ord485)
train_smote$totord <- round(train_smote$totord)
train_smote$breadth <- round(train_smote$breadth)

#### full model ####
index <- sample(1:nrow(train), nrow(train), replace = F)
train <- train[index,]
full <- glm(sale ~ ., data = train, family = binomial)
full <- glm(sale ~ ., data = train_smote, family = binomial)
full <- glm(sale ~ ord185 + ord285 + ord385 + ord485 + freq.buyer, data = train, family = binomial)
summary(full)
vif(full)

full_analysis <- analysis(full, train$sale, test)
full_analysis$Cutoff
full_analysis$PseudoR2
full_analysis$Threshold

full_perf <- perf(full_analysis$Cutoff, test$sale, full_analysis$Prediction)
full_perf

full_step <- step(full)
step <- glm(sale ~ recmon + ordcls3 + 
              salcls3 + ord185 + ord285 + 
              ord385 + ord485,
            data = train, family = binomial)
summary(step)


step_analysis <- analysis(step, train$sale, test)
step_analysis$Cutoff
step_analysis$PseudoR2
step_analysis$Threshold

step_perf <- perf(step_analysis$Cutoff, test$sale, step_analysis$Prediction)
step_perf

#### drop some variables ####
vif(step)
which(vif(step)>3)
train_smote2 <- within(train_smote, rm(tof, ordcls3, ordcls5, ordcls6, ordcls7,
                                       salcls3, salcls5, salcls6, salcls7, totord, totsale, breadth))
full2 <- glm(sale ~ ., data = train_smote2, family = binomial)
summary(full2)
vif(full2)

train_smote2 <- within(train_smote2, rm(ordcls1, ordcls2))
full2 <- glm(sale ~ ., data = train_smote2, family = binomial)
summary(full2)
vif(full2)

train_smote2 <- within(train_smote2, rm(salcls2))
full2 <- glm(sale ~ ., data = train_smote2, family = binomial)
summary(full2)
vif(full2)

full_analysis2 <- analysis(full2, train_smote2$sale, test)
full_analysis2$Cutoff
full_analysis2$PseudoR2
full_analysis2$Threshold

full_perf2 <- perf(full_analysis2$Cutoff, test$sale, full_analysis2$Prediction)
full_perf2

inter <- glm(sale ~ recmon + ord185 + ord285 + ord385 + ord485 + avgordamt + purrate + recmon:. + avgordamt:.,
             data = train_smote3, family = binomial)
summary(inter)
vif(inter)
step_inter <- step(inter)
step2 <- glm(sale ~ recmon + ord185 + ord285 + ord385 + ord485 + avgordamt + 
               purrate + recmon:ord185 + recmon:ord285 + recmon:ord385 + 
               recmon:ord485 + ord185:avgordamt + ord285:avgordamt + ord385:avgordamt + 
               ord485:avgordamt + avgordamt:purrate, data = train_smote3, family = binomial)
summary(step2)
vif(step2)

step22 <- glm(sale ~ recmon + ord185 + ord285 + ord385 + ord485 + 
                recmon:ord385 + recmon:ord485 + ord185:avgordamt + avgordamt:purrate,
              data = train_smote3, family = binomial)
summary(step22)
vif(step22)

step_analysis2 <- analysis(step22, train_smote3$sale, test)
step_analysis2$Cutoff
step_analysis2$PseudoR2
step_analysis2$Threshold

step_perf2 <- perf(step_analysis2$Cutoff, test$sale, step_analysis2$Prediction)
step_perf2

train_smote3 <- subset(train_smote, select = c(recmon,ord185,ord285,ord385,ord485,avgordamt,purrate,sale))
full3 <- glm(sale ~ ., data = train_smote3, family = binomial)
summary(full3)
vif(full3)

full_analysis3 <- analysis(full3, train_smote3$sale, test)
full_analysis3$Cutoff
full_analysis3$PseudoR2
full_analysis3$Threshold

full_perf3 <- perf(full_analysis3$Cutoff, test$sale, full_analysis3$Prediction)
full_perf3

train_smote4 <- subset(train_smote, select = c(recmon,ord185,ord285,ord385,ord485,avgordamt,sale))
full4 <- glm(sale ~ ., data = train_smote4, family = binomial)
summary(full4)
vif(full4)

full_analysis4 <- analysis(full4, train_smote4$sale, test)
full_analysis4$Cutoff
full_analysis4$PseudoR2
full_analysis4$Threshold

full_perf4 <- perf(full_analysis4$Cutoff, test$sale, full_analysis4$Prediction)
full_perf4

train_smote5 <- subset(train_smote, select = c(recmon,ordcls1,ordcls3,ordcls4,ordcls5,
                                               ordcls6,ordcls7,ord185,ord285,ord385,ord485,
                                               avgordamt,sale))
full5 <- glm(sale ~ ., data = train_smote5, family = binomial)
summary(full5)
vif(full5)

full_analysis5 <- analysis(full5, train_smote5$sale, test)
full_analysis5$Cutoff
full_analysis5$PseudoR2
full_analysis5$Threshold

full_perf5 <- perf(full_analysis5$Cutoff, test$sale, full_analysis5$Prediction)
full_perf5
