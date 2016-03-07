options( java.parameters = "-Xmx10g")
suppressMessages(library(readr))
suppressMessages(library(vtreat))
suppressMessages(library(caret))
suppressMessages(library(FSelector))
library(e1071)
library(randomForest)
suppressMessages(library(rpart))
#suppressMessages(library(rpart.plot))
#knitr::opts_chunk$set(cache=TRUE)
train.data <- read_csv("train.csv", progress=FALSE)
test.data <- read_csv("test.csv")

print("Using vtreat package to prepare data")
treatments <- readRDS('treatments.RDS')
train <- prepare(treatments, train.data, pruneSig=1)
test  <- prepare(treatments, test.data, pruneSig=1)

is.duplicated <- function(col1, col2){
    duplicated <- length(which(as.integer(col1 == col2)== 0 ))==0
    return(duplicated)
}

print("Checking for duplicated columns")

find.duplicates <- function(dframe){
    duplicates <- c()
    for(i in 1:(length(colnames(dframe)) - 1)){
        for(j in (i + 1):length(colnames(dframe))){
            if(is.duplicated(dframe[,i], dframe[,j])){
                duplicates <- c(duplicates, colnames(dframe)[j])
            }
        }
    }
    return(duplicates)
}

duplicatedcols <- find.duplicates(train)
if(length(duplicatedcols) > 0){
    print(paste("Removing", length(duplicatedcols), "Duplicated columns"))
    train <- train[,!names(train) %in% duplicatedcols]
    test <- test[,!names(test) %in% duplicatedcols]
}else{
    print("No duplicated columns found")
}

print("Checking for constants")
get.constants <- function(dframe){
    consts <- c()
    for(i in 1:length(colnames(dframe))){
        if(length(unique(dframe[,i])) == 1){
            consts <- c(consts, colnames(dframe)[i])
        }
    }
    return(consts)
}
consts <- get.constants(train)

if(length(consts) > 0){
    print(paste("Captured", length(consts), "Constant cols now removing them"))
    train <- train[,!names(train) %in% consts]
    test <- test[,!names(test) %in% consts]
}else{
    print("No constant columns")
}
print("Deleting unused vars")
train.data <- NULL
ids <- test.data$ID
test.data <- NULL

gc()

correlations <- cor(train)
highCorr <- findCorrelation(correlations, cutoff = .75)

print(paste(length(highCorr), "Was found to be highly correlated, removing them"))
train <- train[,-highCorr]
test <- test[,-highCorr]
trainToBeTransformed <- train[,-which(names(train) %in% c("target"))]
trans <- preProcess(trainToBeTransformed, method = c("center", "scale", "pca"))
transformed <- predict(trans, train)
transformedTest <- predict(trans, test)

print("Selecting features")
weights <- information.gain(target~., transformed)
highest.gain <- weights[order(weights$attr_importance,decreasing=TRUE)[1:10],,drop=FALSE]

fol <- formula(paste("target ~",paste(rownames(highest.gain),collapse="+")))
model <- rpart(fol, data=transformed)
print("predicting")
predictions <- predict(model, transformedTest)
endFrame <- data.frame(ID = ids, PredictedProb=predictions)
write.csv(endFrame, "submission2.csv", row.names = FALSE)

