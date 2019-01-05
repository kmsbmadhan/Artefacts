library(Boruta)

dTrain <- claim
dTrain <- read.csv("claims.csv", header = TRUE)
y <- "FraudFound_P"
x <- setdiff(names(dTrain), y)

names(dTrain)

colnames(new_train)

levels(dTrain$FraudFound_P)

dTrain$FraudFound_P <- as.factor(dTrain$FraudFound_P)

new_train <- cbind(dTrain$FraudFound_P, trainCaret)
new_train$`dTrain$FraudFound_P` <- new_train$Fraud

is.numeric(new_train)
boruta.train <- Boruta(FraudFound_P ~ ., data = dTrain, doTrace = 2)
names(dTrain)
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

new <- subset(boruta.df,boruta.df$decision == "Confirmed")

new[1]

colnames(new_train)

library(caret)
library(randomForest)
set.seed(456)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe <- rfe(new_train[,2:161], new_train[,1], rfeControl=control)
print(rfe, top=15)
plot(rfe, type=c("g", "o"), cex = 1.0)
predictors(rfe)
head(rfe$resample, 10)
