library(caret)
library(randomForest)
library(ROCR)
library(gridExtra)

AllWineData <- read.table("AllWineDataPreProcessed.csv", header=TRUE, sep=";")
names(AllWineData)
plot(as.factor(AllWineData$quality))
title(main = "Quality distribution")
AllWineData$taste <- ifelse(AllWineData$quality < 5, "bad", "good")
AllWineData$taste[AllWineData$quality == 5] <- "normal"
AllWineData$taste[AllWineData$quality == 6] <- "normal"
AllWineData$taste[AllWineData$quality >= 8] <- "excellent"
AllWineData$taste <- as.factor(AllWineData$taste)
plot(AllWineData$taste)
title(main= "Quality Distribution")
index <- createDataPartition(AllWineData$quality, p=0.7, list=FALSE)
wine <- AllWineData[,c(-13)]
train <- wine[index,]
test <- wine[-index,]

#Find out ntree best
ntrees <- round(10^seq(1,3.2,by=0.2))
rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0
ii <- 1
for (nt in ntrees)
{ 
  print(nt)
  set.seed(2018)
  model.rf <- randomForest(taste ~ . - quality, data = train, ntree=nt, proximity=FALSE)
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}
lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])
plot(x = rf.results[,1], y=rf.results[,2], ylab = "OOB", xlab = "# trees", type = "o")
title("Estimate error rate")
grid.table(rf.results[,1:2])

#Model

model <- randomForest(taste ~ . - quality, data=train, ntree=ntrees.best,proximity=TRUE, importance=TRUE,
                      keep.forest=TRUE)
varImpPlot(model, main = "Importance of variables")
impvar <- importance(model)
plot(impvar)
prediction <- predict(model, newdata = test)
# What variables are being used in the forest (their total counts)
var <- varUsed(model, by.tree=FALSE, count = TRUE)
var = as.data.frame((var))
rownames(var) = colnames(test[,1:11])
barplot(as.matrix(t(var)), las=2, cex.names = 0.55)
title("Variables being used in the forest")

result <- table(prediction, test$taste)
round(100*(1-sum(diag(result))/sum(result)),2)
#Precision
(precision <- diag(result) / rowSums(result))
#Recall
(recall <- (diag(result) / colSums(result)))
#accuracy
(accuracy <- sum(diag(result)) / sum(result))
