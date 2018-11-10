#SVM - Assignment >> Digit Recogniser
#-----------------------------------
#Loading required packages
library(caret)
library(kernlab)
library(caTools)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

#Business Understanding 
#----------------------
# The goal is to make a handwritten digit recognition model that can correctly identify the digit (between 0-9) written in an image. 

#Loading the test and train data
train <- read_csv("mnist_train.csv", col_names = FALSE, na = c("", "NA"))
test <- read_csv("mnist_test.csv", col_names = FALSE, na = c("", "NA")) #Convert the blank and NA to NA
nrow(train) #60000 rows
nrow(test) #10000 rows

#Data Understanding
#------------------
str(train) #60000 rows with 785 columns in train data
str(test) #10000 rows with 785 columns in train data

head(train) #Checking first few data
head(test) #Checking first few data

summary(train)
summary(test)

#Data Preparation:
#-----------------

#Checking NA value in test and train dataset
sum(is.na(train)) 
sum(is.na(test)) #No NA value in train and test dataset

#Checking the duplicate row in test and train dataset
sum(duplicated(train))
sum(duplicated(test)) #No duplicate row in train and test data

#Changing column name of target variable to "digit"
names(train)[1] <- "digit"
names(test) [1] <- "digit"

#Making our target class i.e.  to factor
train$digit <- as.factor(train$digit) 
test$digit <- as.factor(test$digit)

#Checking number of character column in train and test data for dummy variable
sum(sapply(train, is.character)) 
sum(sapply(test, is.character)) #No character column in train and test data

#Data Exploration :
#------------------
#Making the barplot for target digit column
ggplot(data = train, mapping = aes(x = digit, fill = digit)) +
  geom_bar() #Plot clearly shows that the digit 1 has the maximum occurence

#Model building & Evaluation
#---------------------------
#Taking 15% of the random rows from train data due to machine efficiency
set.seed(100)

indices = sample.split(train$digit, SplitRatio = 0.15)
train_1 <- train[indices, ]
nrow(train_1) #9000 random rows 

#Constructing Model
#-------------------

#Using Linear Kernel
Model_linear <- ksvm(digit ~ ., data = train_1, scale = FALSE, kernel = "vanilladot") #Linear Kernel 
Eval_linear<- predict(Model_linear, test)
Model_linear #Fitted at C = 1

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit) #Accuracy - 0.9128

#Using Polynomial Kernel
Model_Poly <- ksvm(digit ~ ., data = train_1, scale = FALSE, kernel = "polydot") #Polynomial Kernel 
Eval_Poly<- predict(Model_Poly, test)
Model_Poly #Fitted at C = 1

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_Poly,test$digit) #Accuracy - 0.9128

#Using RBF Kernel
Model_RBF <- ksvm(digit ~ ., data = train_1, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)
Model_RBF 

# Sigma = 1.63186692632831e-07
# Cost C = 1 

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit) #Accuracy - 0.9581

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(201)
grid <- expand.grid(.sigma= c(1.63186692632831e-07,1.62186692632831e-07,1.61186692632831e-07),  .C = c(0.25, 0.5, 0.75, 1))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
#trcontrol = Our traincontrol method.

fit.svm <- train(digit~., data=train_1, method = "svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
plot(fit.svm) 

#Model is fine tuned with sigma = 1.631867e-07 & C = 1
#Accuracy - 0.9518897

Eval_fit<- predict(fit.svm, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_fit,test$digit) #Accuacy - 0.9581

#---------------------------------
#Applying PCA to reduce dimensions
#---------------------------------

##PCA
prin_comp <-  prcomp(train_1[-1], scale. = F)
names(prin_comp)

prin_comp$center #Mean of the variables
prin_comp$sdev #SD of the variables

#principal componenet loading of first few PCA and rows
prin_comp$rotation[1:6,1:6]

dim(prin_comp$x) #19000 * 784

#Biplot for PCAs
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#Aiming to find the components with maximum varience
#----------------------------------------------------

#Proportion of variance for Principal Components
prop_varex <- pr_var/sum(pr_var) * 100
prop_varex[1:20] #It shows that the 1st component explains the 9.73% varience, 2nd explains 7.18% 

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b") 

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
#About 85 to 100 principal components are able to tell 90% variations of the data

library("factoextra")
eig.val <- get_eigenvalue(prin_comp)
head(eig.val,100)
#First 86 principal components are able to explain 90% of the variation. This is an accpetably large percentage. 

#Making training dataset with principal components
train.pca <- data.frame(digit = train_1$digit, prin_comp$x)

#We are interested in first 86 PCAs
train.pca <- train.pca[, 1:87]

#Transforming test into PCA
test.pca <- predict(prin_comp, newdata = test[-1])
test.pca <- data.frame(test.pca)
test.pca <- cbind(digit = test$digit, test.pca) #Adding dependent column in test.pca

#We are interested in first 86 PCAs
test.pca <- test.pca[, 1:87]


#Constructing Model with PCAs
#----------------------------

#Using Linear Kernel
Model_l_pca <- ksvm(digit ~ ., data = train.pca, scale = FALSE, kernel = "vanilladot") #Linear Kernel
Eval_l_pca <- predict(Model_l_pca, test.pca)
Model_l_pca #Fitted at C = 1

#confusion matrix - Linear Kernel
confusionMatrix(Eval_l_pca,test.pca$digit) #Accuracy - 0.9155

#Using Polynomial Kernel
Model_P_pca <- ksvm(digit ~ ., data = train.pca, scale = FALSE, kernel = "polydot") #Polynomial Kernel
Eval_P_pca<- predict(Model_P_pca, test.pca)
Model_P_pca #Fitted at C = 1

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_P_pca,test.pca$digit) #Accuracy - 0.9155

#Using RBF Kernel
Model_RBF_pca <- ksvm(digit ~ ., data = train.pca, scale = FALSE, kernel = "rbfdot")
Eval_RBF_pca<- predict(Model_RBF_pca, test.pca)
Model_RBF_pca 


# Sigma = 0.00632015283442661 
# Cost C = 1 

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF_pca,test.pca$digit) #Accuracy - 0.9533

##Conclusion : Support Vector Machines model is successfully build and evaluated with and without PCA on test data with accuracy of about 95%
