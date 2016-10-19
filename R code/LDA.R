library(MASS)
train_lda <- within(train, rm(salcls1, salcls2, salcls3, salcls4, salcls5, salcls6, salcls7,
                              totord, totsale))

lda = lda(sale ~ ., data = train_lda)
summary(lda)
lda
lda$class

pred_lda = as.numeric(predict(object = lda, newdata = test)$class)-1
cut <- seq(0.05,0.95,0.05)
performance <- matrix(0,nrow=length(cut),ncol=6)
for(i in 1:length(cut))
{
  performance[i,1] <- cut[i]
  temp <- perf(cut[i],test$sale,pred_lda)
  performance[i,2] <- temp$Precision
  performance[i,3] <- temp$Recall
  performance[i,4] <- temp$F_Score
  performance[i,5] <- temp$trueNegatives
  performance[i,6] <- temp$ClassificationRate
}
cp <- data.frame(performance)
colnames(cp) <- c("Cutoff", "Precision","Recall","F_Score","trueNegatives","ClassificationRate")
cut_fscore = cp[which(cp$F_Score==max(cp$F_Score,na.rm=T))[1],1]

lda_perf <- perf(cut_fscore, test$sale, pred_lda)
lda_perf
threshold <- max(length(which(pred_lda==1))/length(pred_lda),
                 length(which(pred_lda==0))/length(pred_lda))

smote_lda <- within(train_smote, rm(salcls1, salcls2, salcls3, salcls4, salcls5, salcls6, salcls7,
                              totord, totsale))

lda = lda(sale ~ ., data = smote_lda)
summary(lda)
lda
lda$class

pred_lda = as.numeric(predict(object = lda, newdata = test)$class)-1
cut <- seq(0.05,0.95,0.05)
performance <- matrix(0,nrow=length(cut),ncol=6)
for(i in 1:length(cut))
{
  performance[i,1] <- cut[i]
  temp <- perf(cut[i],test$sale,pred_lda)
  performance[i,2] <- temp$Precision
  performance[i,3] <- temp$Recall
  performance[i,4] <- temp$F_Score
  performance[i,5] <- temp$trueNegatives
  performance[i,6] <- temp$ClassificationRate
}
cp <- data.frame(performance)
colnames(cp) <- c("Cutoff", "Precision","Recall","F_Score","trueNegatives","ClassificationRate")
cut_fscore = cp[which(cp$F_Score==max(cp$F_Score,na.rm=T))[1],1]

lda_perf <- perf(cut_fscore, test$sale, pred_lda)
lda_perf
threshold <- max(length(which(pred_lda==1))/length(pred_lda),
                 length(which(pred_lda==0))/length(pred_lda))
# Assess the accuracy of the prediction percent correct for each category of G
ct <- table(test$sale, lda$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))
