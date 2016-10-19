#######summary#######
summary(train$ord185)
summary(train$ord285)
summary(train$ord385)
summary(train$ord485)

#######box plots#######
# meaningless

#######histograms#######
# meaningless

#######stem#######
# meaningless

#######table#######
table(train$ord185)
table(train$ord285)
table(train$ord385)
table(train$ord485)

#######ratios#######
ordYr <- c(1:4)
totOrd <- c(length(train$ord185),length(train$ord285),length(train$ord385),length(train$ord485))
promCount <- c(length(which(train$ord185==1)),length(which(train$ord285==1)),length(which(train$ord385==1)),length(which(train$ord485==1)))
noPromCount <- totOrd - promCount
promRate <- promCount/totOrd
prom_summary <- data.frame(ordYr,totOrd,promCount,noPromCount,promRate)

cor(train[,18:21])

write.table(prom_summary,"prom_count.csv",sep = "\t")