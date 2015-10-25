library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

set.seed(32343)

dat_train <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),
                      na.strings=c("", "NA"))
dat_test <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"), 
                     na.strings=c("", "NA"))

dim(dat_train)
dim(dat_test)

nsv <- nearZeroVar(dat_train, saveMetrics = TRUE)
head(nsv)

dat_train1 <- dat_train[names(dat_train) %in% 
                                row.names(nsv[nsv$zeroVar==FALSE & nsv$nzv==FALSE,])]
dat_test1 <- dat_test[names(dat_test) %in% 
                              c(row.names(nsv[nsv$zeroVar==FALSE & nsv$nzv==FALSE,]),
                                "problem_id")]

ncol(dat_train1); ncol(dat_test1)
names(dat_train1)

trainmiss <- apply(dat_train1,2,function(x) {sum(is.na(x))})
dat_train2 <- dat_train1[,which(trainmiss == 0)]

testmiss <- apply(dat_test1,2,function(x) {sum(is.na(x))})
dat_test2 <- dat_test1[,which(testmiss == 0)]
ncol(dat_train2); ncol(dat_test2)
names(dat_train2)

dat_train2 <- dat_train2[, -c(1:6)]
dat_test2 <- dat_test2[, -c(1:6)]

training <- dat_train2[inTrain, ]
testing <- dat_train2[-inTrain, ]
dim(training)
dim(testing)

plot(training$classe)

modlda = train(classe ~ ., data = training, method = "lda")
plda = predict(modlda, testing)
confusionMatrix(plda, testing$classe)

modct <- rpart(classe ~ ., data=training, method="class")
rpart.plot(modct, main="Classification Tree", under=TRUE, faclen=0)

pct <- predict(modct, testing, type = "class")
confusionMatrix(pct, testing$classe)

predicttest <- predict(modlda, dat_test2)
predicttest

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                files = paste0("problem_id_",i,".txt")
                write.table(x[i],file=files,col.names=FALSE,row.names=FALSE,quote=FALSE)
        }
}

pml_write_files(predicttest)



