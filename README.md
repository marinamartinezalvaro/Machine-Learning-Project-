# Machine-Learning-Project-
Marina Martinez Machine Learning Project
  setwd("C:/Users/Marina/Documents/Dropbox/Microbioma/Learning and courses/Machine Learning R Coursera")
### 1.-Download libraries
      library(caret)
      library(randomForest)
      library(rpart) 
      library(rpart.plot)
      library(RColorBrewer)
      library(rattle)
#### 2.-set a seed
      set.seed(1234)
#### 3.-Read the  train and test files
      trainUrl<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
      testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
      training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
      testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
      dim(training)
      dim(testing)
      View(testing)
      colnames(training)
      summary(training)
      training$classe

### 4.- Checking how many missiong values are
        is.na(training$classe) # no missing values in the trait
        sum(is.na(training$max_roll_belt)) # Many NA (19216) in some of the traits. I can remove them, or fill with the nearest neighbour
        19216/nrow(training)  ## 97% of missing values!!

### 5.-Partioning training data set in two, 60% for training and 40% for testing
        inTrain<-createDataPartition(y=training$classe, p=0.6, list=FALSE)
        M_Train<-training[inTrain,]
        M_Test<-training[-inTrain,]
        dim(M_Train)
        dim(M_Test)

### 6.- Cleaning the data before using them to biult models

                ### 6.1.- Remove variables with low variability (Non Zer Varibility)
                myDataNZV <- nearZeroVar(M_Train)
                myNZVvars <- names(M_Train) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                                      "stddev_yaw_forearm", "var_yaw_forearm")
                M_Train <- M_Train[!myNZVvars]
                
                dim(M_Train)
                
               ### 6.2- Remove the first colum, which is the ID
                   M_Train <- M_Train[c(-1)]
                  
               ### 6.3.- Removing variables with many NA
                        #Check the proprotion of missing values in each predictor
                    Prop_NA<-data.frame()
                    dim(Prop_NA)
                          for(i in 1:ncol(M_Train)) {   
                                    P<-sum(is.na(M_Train[,i]))/nrow(M_Train)
                                    Prop_NA[1,i]<-P
                    colnames(Prop_NA)<-colnames(M_Train)
                    sum(Prop_NA == 0) ##60 variables with no missing values
                    sum(Prop_NA < 0.95) ### I am goin to keot only 60/160 variables
                       #Remove variables with NA
                    trainingV3 <- M_Train #creating another subset to iterate in loop
                    for(i in 1:length(M_Train)) { #for every column in the training dataset
                      if( sum( is.na( M_Train[, i] ) ) /nrow(M_Train) >= .6 ) { #if n?? NAs > 60% of total observations
                        for(j in 1:length(trainingV3)) {
                          if( length( grep(names(M_Train[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
                            trainingV3 <- trainingV3[ , -j] #Remove that column
                          }   
                        } 
                      }
                    }
                    #To check the new N?? of observations
                    dim(trainingV3)
                    ## [1] 11776    58
                    #Setting back to our set:
                    M_Train <- trainingV3
                    rm(trainingV3)
                    
        ### 6.4- Repeat the same for the M_test set and testing set
                    clean1 <- colnames(M_Train)
                    clean2 <- colnames(M_Train[, -58]) #already with classe column removed
                    M_Test <- M_Test[clean1]
                    testing <- testing[clean2]

                    dim(M_Test)
                    dim(testing)
                   
### 7-.Coerce the data into the same type.
                    for (i in 1:length(testing) ) {
                      for(j in 1:length(M_Train)) {
                        if( length( grep(names(M_Train[i]), names(testing)[j]) ) ==1)  {
                          class(testing[j]) <- class(M_Train[i])
                        }      
                      }      
                    }
                    #And to make sure Coertion really worked, simple smart ass technique:
                    testing <- rbind(M_Train[2, -58] , testing) #note row 2 does not mean anything, this will be removed right.. now:
                    testing <- testing[-1,]
                    

### 8.-Built different algorithms
                  # 8.1. Decision trees
                  modFitA1 <- rpart(classe ~ ., data=M_Train, method="class")
                  fancyRpartPlot(modFitA1)
                  predictionsA1 <- predict(modFitA1, M_Test, type = "class") ## predict M_Test
                  confusionMatrix(predictionsA1, M_Test$classe)
                  
                  # 8.2. Random Forest
                  modFitB1 <- randomForest(classe ~. , data=M_Train)
                  predictionsB1 <- predict(modFitB1, M_Test, type = "class")
                  confusionMatrix(predictionsB1, M_Test$classe)  ## RForest works better
                  
### 9.- Out of sample error

                  predictionsB2 <- predict(modFitB1, testing, type = "class")
                  
