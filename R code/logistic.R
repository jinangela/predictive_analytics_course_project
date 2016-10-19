summary(train$recmon)
hist(train$recmon)
hist(sqrt(train$recmon))

data=train
data$recmon = sqrt(data$recmon)
hist(data$recmon)

library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
data$recmon <- scale(data$recmon)
mean(data$recmon)
sd(data$recmon)

# correlated
cor(train$ordcls1,train$salcls1)
cor(train$ordcls2,train$salcls2)
cor(train$ordcls3,train$salcls3)
cor(train$ordcls4,train$salcls4)
cor(train$ordcls5,train$salcls5)
cor(train$ordcls6,train$salcls6)
cor(train$ordcls7,train$salcls7)

summary(train$salcls1)
hist(train$salcls1)
length(which(train$salcls1<0)) # 9
summary(train$salcls2)
hist(train$salcls2)
length(which(train$salcls2<0)) # 85
summary(train$salcls3)
hist(train$salcls3)
length(which(train$salcls3<0)) # 7
summary(train$salcls4)
hist(train$salcls4)
length(which(train$salcls4<0)) # 2
summary(train$salcls5)
hist(train$salcls5)
length(which(train$salcls5<0)) # 0
summary(train$salcls6)
hist(train$salcls6)
length(which(train$salcls6<0)) # 25
summary(train$salcls7)
hist(train$salcls7)
length(which(train$salcls7<0)) # 5
length(which(train$salcls1<0|train$salcls2<0|train$salcls3<0
             |train$salcls4<0|train$salcls5<0|train$salcls6<0
             |train$salcls7<0))
drop <- which(train$salcls1<0|train$salcls2<0|train$salcls3<0
              |train$salcls4<0|train$salcls5<0|train$salcls6<0
              |train$salcls7<0)

cor(train[,18:21])
cor(data.frame(train$recmon,train$tof,train$totord,train$totsale))
summary(train$targamnt)
length(which(train$targamnt==0))/length(train$targamnt)
length(which(train$targamnt<0))
hist(train$targamnt)

logitdata <- subset(train,select = c(targamnt,recmon,tof,totord,
                                     ord185,ord285,ord385,ord485))
logitdata <- logitdata[which(logitdata$targamnt>=0),]
hist(log(logitdata$targamnt+1))
logitdata$Y <- ifelse(logitdata$targamnt>0,1,0)
logitdata <- logitdata[,2:9]
table(logitdata$Y)

library("DMwR")
logitdata$Y <- as.factor(logitdata$Y)
smote_logitdata <- SMOTE(Y ~ ., data = logitdata, perc.over = 1600, perc.under = 0)
smote_logitdata <- rbind(subset(logitdata, Y==0), smote_logitdata)

# regression
library("bestglm")
index <- sample(length(smote_logitdata$Y),floor(length(smote_logitdata$Y)*0.8))
smote_logittrain <- smote_logitdata[index,]
smote_logitvalid <- smote_logitdata[-index,]
train_glm <- within(smote_logittrain,
                    {y <- Y
                     Y <- NULL})
best_glm <- bestglm(train_glm, family = binomial, IC = "AIC", TopModels = 3)



fit1 <- glm(Y ~ ., data = logitdata, family = binomial)
summary(fit1)
library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
vif(fit1)
library("glmulti", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
sub1 <- glmulti(fit1, level = 1, confsetsize = 1, plotty = F, report = T, family = binomial)
best1 <- glm(Y~1+recmon+ord185+ord285+ord385+ord485, data = logitdata, family = binomial)
summary(best1)

# validation
index <- sample(length(logitdata$Y),floor(length(logitdata$Y)*0.8))
logittrain <- logitdata[index,]
logitvalid <- logitdata[-index,]
fit1 <- glm(Y ~ ., data = logittrain, family = binomial)
summary(fit1)
sub1 <- glmulti(fit1, level = 1, confsetsize = 1, plotty = F, report = T, family = binomial)
best1 <- glm(Y~1+recmon+ord185+ord285+ord385+ord485, data = logittrain, family = binomial)
summary(best1)

# precision & recall
logitvalid$pred <- predict(best1, newdata = logitvalid, type = "response")
summary(logitvalid$pred)
cut <- seq(0.05,0.95,0.05)
performance <- matrix(0,nrow=length(cut),ncol=6)
for(i in 1:length(cut))
{
  performance[i,1] <- cut[i]
  performance[i,2] <- perf(cut[i],logitvalid$Y,logitvalid$pred)$Precision
  performance[i,3] <- perf(cut[i],logitvalid$Y,logitvalid$pred)$Recall
  performance[i,4] <- perf(cut[i],logitvalid$Y,logitvalid$pred)$F_Score
  performance[i,5] <- perf(cut[i],logitvalid$Y,logitvalid$pred)$trueNegatives
  performance[i,6] <- perf(cut[i],logitvalid$Y,logitvalid$pred)$ClassificationRate
  
}
library("ggplot2")
library("reshape2")
cp <- data.frame(performance)
colnames(cp) <- c("Cutoff", "Precision","Recall","F_Score","trueNegatives","ClassificationRate")
mcp <- melt(cp, id.vars = "Cutoff", value.names = "Value", 
            variable.name = "Cutting_Point")
ggplot(data = mcp, aes(x=Cutoff, y=value, group = Cutting_Point, colour = Cutting_Point)) + 
  geom_line() +
  xlab("Cutting Point") +
  ylab("Value") +
  ggtitle("Performance Analysis")
# choose the cutoff by maximizing the classification rate
cut = cp[which(cp$ClassificationRate==max(cp$ClassificationRate,na.rm=T))[1],1]

# cut <- seq(0.05,0.95,0.05)
# performance <- seq(0,0,length(cut))
# for(i in 1:length(cut))
#   performance[i] <- perf(cut[i],logitvalid$Y,logitvalid$pred)
# library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
# library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
# cp <- data.frame(cut,performance)
# colnames(cp) <- c("Cut","Sensitivity","Specificity",
#                   "Classification Rate","Distance",
#                   "True Positive","True Negative")
# # matplot(cut,performance,type = "l")
# mcp <- melt(cp, id.vars = "Cut", value.names = "Value", 
#             variable.name = "Cutting_Point")
# ggplot(data = mcp, aes(x=Cut, y=value, group = Cutting_Point, colour = Cutting_Point)) + 
#   geom_line() +
#   xlab("Cutting Point") +
#   ylab("Value") +
#   ggtitle("Performance Analysis")
# Fscore <- 2*performance[,1]*performance[,5]/(performance[,1]+performance[,5])
# max(Fscore,na.rm=T)
# which(Fscore==max(Fscore,na.rm=T))
# cut = which(Fscore==max(Fscore,na.rm=T))*0.05
logitvalid$yhat <- ifelse(logitvalid$pred>cut, 1, 0)
precision <- length(which(logitvalid$Y==1&logitvalid$yhat==1))/
  length(which(logitvalid$yhat==1))
recall <- length(which(logitvalid$Y==1&logitvalid$yhat==1))/
  length(which(logitvalid$Y==1))

# chi test
Ddiff <- with(best1, null.deviance - deviance)
df <- with(best1, df.null - df.residual)
pvalue <- with(best1, pchisq(null.deviance - deviance, 
                            df.null - df.residual, lower.tail = FALSE))
# highly significant but precision and recall are not high enough

# log likelihood
logLik(best1)



logitdata2 <- subset(train,select = c(targamnt,recmon,tof,totord,
                                      ordcls1, ordcls2, ordcls3, ordcls4,
                                      ordcls5, ordcls6, ordcls7,
                                      ord185,ord285,ord385,ord485))
logitdata2 <- logitdata2[which(logitdata2$targamnt>=0),]
hist(log(logitdata$targamnt+1))
logitdata2$Y <- ifelse(logitdata2$targamnt>0,1,0)
logitdata2 <- logitdata2[,2:16]

# regression
fit2 <- glm(Y ~ ., data = logitdata2, family = binomial)
summary(fit2)
library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
vif(fit2)
library("glmulti", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
sub2 <- glmulti(fit2, level = 1, confsetsize = 1, plotty = F, report = T, family = binomial)
best2 <- glm(Y~1+recmon+ordcls2+ordcls3+ordcls4+ordcls6+ordcls7+ord185+ord285+ord385+ord485, 
             data = logitdata2, family = binomial)
summary(best2)

# validation
index2 <- sample(length(logitdata2$Y),floor(length(logitdata2$Y)*0.8))
logit2train <- logitdata2[index2,]
logit2valid <- logitdata2[-index2,]
fit2 <- glm(Y ~ ., data = logit2train, family = binomial)
summary(fit2)
sub2 <- glmulti(fit2, level = 1, confsetsize = 1, plotty = F, report = T, family = binomial)
#######
best2 <- glm(Y~1+recmon+ordcls2+ordcls3+ordcls4+ordcls6+ordcls7+ord185+ord285+ord385+ord485, 
             data = logit2train, family = binomial)
summary(best2)

# precision & recall
logit2valid$pred <- predict(best2, newdata = logit2valid, type = "response")
summary(logit2valid$pred)
cut <- seq(0.05,0.95,0.05)
performance <- matrix(0,nrow=length(cut),ncol=6)
for(i in 1:length(cut))
  performance[i,] <- perf(cut[i],logitvalid$Y,logitvalid$pred)
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
cp2 <- data.frame(cut,performance)
colnames(cp2) <- c("Cut","Sensitivity","Specificity",
                  "Classification Rate","Distance",
                  "True Positive","True Negative")
# matplot(cut,performance,type = "l")
mcp2 <- melt(cp2, id.vars = "Cut", value.names = "Value", 
            variable.name = "Cutting_Point")
ggplot(data = mcp2, aes(x=Cut, y=value, group = Cutting_Point, colour = Cutting_Point)) + 
  geom_line() +
  xlab("Cutting Point") +
  ylab("Value") +
  ggtitle("Performance Analysis")
Fscore <- 2*performance[,1]*performance[,5]/(performance[,1]+performance[,5])
max(Fscore,na.rm=T)
which(Fscore==max(Fscore,na.rm=T))
cut = which(Fscore==max(Fscore,na.rm=T))*0.05
logit2valid$yhat <- ifelse(logit2valid$pred>cut, 1, 0)
precision2 <- length(which(logit2valid$Y==1&logit2valid$yhat==1))/
  length(which(logit2valid$yhat==1))
recall2 <- length(which(logit2valid$Y==1&logit2valid$yhat==1))/
  length(which(logit2valid$Y==1))

# chi test
Ddiff <- with(best2, null.deviance - deviance)
df <- with(best2, df.null - df.residual)
pvalue <- with(best2, pchisq(null.deviance - deviance, 
                             df.null - df.residual, lower.tail = FALSE))
# highly significant but precision and recall are not high enough

# log likelihood
logLik(best2)
