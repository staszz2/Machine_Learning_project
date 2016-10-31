# The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har

#I'm behind a proxy , so I download files over HTTP and load them from drive
#https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
#https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

main <- function ()
{
file_testing <- paste(getwd(),"Github/Machine_Learning_project/files","pml-testing.csv",sep="/")
file_training<- paste(getwd(),"Github/Machine_Learning_project/files","pml-training.csv",sep="/")

#suppressWarnings(library(rattle))
library(caret)
library(rpart)
library(randomForest)
library(rattle)

set.seed(5051)
data_testing <- read.csv(file_testing, na.strings=c("NA",""), header=TRUE)
data_training <- read.csv(file_training, na.strings=c("NA",""), header=TRUE)

inTrain <- createDataPartition(y=data_training$classe, p=0.6, list=FALSE)

#I will have 3 tier work flow:
# 1. Training - model fit
# 2. Testing - model check
# 3. Production - predicting 20 observations for course project
training <- data_training[inTrain, ] 
testing <- data_training[-inTrain, ]
production <- data_testing 


# 1. Preprocessing
training <- cleanData(training)
testing <- cleanData (testing)

# 2. Fitting decision tree model
modelRPart <- train(classe ~ ., data=training, method="rpart")
accuracy_rpart_is <- modelRPart$results[[2]][1]
#fancyRpartPlot(modelRPart$finalModel) # no need to print if it's no good

pred <- predict(modelRPart, testing)
matrix <- confusionMatrix(pred, testing$classe)
print(matrix)
accuracy_rpart_oos <- matrix$overall[[1]]

cat("In sample error: ", accuracy_rpart_is,"\nOut of sample error: ",accuracy_rpart_oos, "\n")


# 3. Fitting random forest model
modelRF <- randomForest(classe ~. , data=training)
modelRF
predictRF<- predict(modelRF, testing, type = "class")
print(confusionMatrix(predictRF, testing$classe))

#4. Production application

productionClean <- cleanData (production)

#Here i'm battling problems where Production dataset has different factor levels
#.. and rendom forest really does not like that
# This solution is from http://stackoverflow.com/a/36170319

common <- intersect(names(training), names(productionClean)) 
for (p in common) { 
  if (class(training[[p]]) == "factor") 
  { 
    levels(productionClean[[p]]) <- levels(training[[p]]) 
  } 
}

predictAssignment <- predict(modelRF, productionClean, type = "class")
predictAssignment

}

cleanData <- function(dataset)
{
  print(paste("Cleaning data with number of columns: ", ncol(dataset)))
  # Drop near zero variable columns
  nzvdata <- nearZeroVar(dataset, saveMetrics=TRUE)
  bad_columns_nzv <- nzvdata[nzvdata$nzv==TRUE | nzvdata$zeroVar==TRUE,]
  bad_colnames_nzv <- row.names(bad_columns_nzv)

  #count NA percentage per column of data
  rows <- nrow(dataset)
  na_count <-sapply(dataset, function(y) sum(length(which(is.na(y))))/rows)
  
  #Some NA ratios are very high... exclude them too
  columns_na_count <- data.frame(colnames(dataset), na_count)
  table(columns_na_count[,2])
  
  #100 of 160 variables are NA 98% of the time. This cannot be benefitial to the analysis
  #Delete them
  
  column_names_na <- subset(columns_na_count,na_count>0)
  
  bad_column_names <- c(bad_colnames_nzv,as.character(column_names_na[,1]))
  dataset <- dataset[,-which(names(dataset) %in% bad_column_names)]
  #ID and Usename column is not relevant to data analysis, drop it
  dataset <- dataset[,-c(1,2)]
  
  debugging <- FALSE
  if(debugging)
  {
    dataset <- dataset[1:20,]
  }
  print(paste("Returning data with number of columns: ", ncol(dataset)))
  dataset
}

main()