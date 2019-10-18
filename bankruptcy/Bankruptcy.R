#Bankruptcy Case

setwd("bankruptcy")   # Adapt this to point to your data diectory
bankrupt.df <- read.csv("Bankruptcy.csv", header = TRUE)  # load Bankruptcy.csv
str(bankrupt.df) # get a sense of the columns of data
table(is.na(bankrupt.df))  # check for NAs and NaNs

# Thought: D is the decision variables (bankrupt yes/no)
# Thought: only 132 records, but well balanced (66 bankrupt, 66 healthy firms)
# Thought: will probably want to normalize the variables as some have larger ranges relative to others
# Thought: no NAs or NaNs (missing tems/numbers)
# Thought: have to get rid of column NO (just an identifier)
# Thought: while the year variable may tell us something related to the economic cycle, it is less fincnial than R1-R24, and may need to be stripped.

#Checking out the data
dim(bankrupt.df)  # find the dimension of data frame
head(bankrupt.df)  # take a peak at the first six rows
colnames(bankrupt.df) # print column names
summary(bankrupt.df) #get descriptive statistics of column variables

library(ggplot2)

# Create a box plots matrix of some of the columns, broken out by bankruptcy status 
boxplot(R1~D,data=bankrupt.df, main="Boxplots of R1 by D",xlab="Bankruptcy", ylab="R1 CASH/CURDEBT")
# etc etc
boxplot(R24~D,data=bankrupt.df, main="Boxplots of R24 by D",xlab="Bankruptcy", ylab="R1 CASH/CURDEBT")

#Checking for single-variable outliers 
test.df <- bankrupt.df
test.df$R1 <- scale(bankrupt.df$R1,center = TRUE, scale = TRUE)
test.df$R2 <- scale(bankrupt.df$R2,center = TRUE, scale = TRUE)
test.df$R3 <- scale(bankrupt.df$R3,center = TRUE, scale = TRUE)
test.df$R4 <- scale(bankrupt.df$R4,center = TRUE, scale = TRUE)
test.df$R5 <- scale(bankrupt.df$R5,center = TRUE, scale = TRUE)
test.df$R6 <- scale(bankrupt.df$R6,center = TRUE, scale = TRUE)
test.df$R7 <- scale(bankrupt.df$R7,center = TRUE, scale = TRUE)
test.df$R8 <- scale(bankrupt.df$R8,center = TRUE, scale = TRUE)
test.df$R9 <- scale(bankrupt.df$R9,center = TRUE, scale = TRUE)
test.df$R10 <- scale(bankrupt.df$R10,center = TRUE, scale = TRUE)
test.df$R11 <- scale(bankrupt.df$R11,center = TRUE, scale = TRUE)
test.df$R12 <- scale(bankrupt.df$R12,center = TRUE, scale = TRUE)
test.df$R13 <- scale(bankrupt.df$R13,center = TRUE, scale = TRUE)
test.df$R14 <- scale(bankrupt.df$R14,center = TRUE, scale = TRUE)
test.df$R15 <- scale(bankrupt.df$R15,center = TRUE, scale = TRUE)
test.df$R16 <- scale(bankrupt.df$R16,center = TRUE, scale = TRUE)
test.df$R17 <- scale(bankrupt.df$R17,center = TRUE, scale = TRUE)
test.df$R18 <- scale(bankrupt.df$R18,center = TRUE, scale = TRUE)
test.df$R19 <- scale(bankrupt.df$R19,center = TRUE, scale = TRUE)
test.df$R20 <- scale(bankrupt.df$R20,center = TRUE, scale = TRUE)
test.df$R21 <- scale(bankrupt.df$R21,center = TRUE, scale = TRUE)
test.df$R22 <- scale(bankrupt.df$R22,center = TRUE, scale = TRUE)
test.df$R23 <- scale(bankrupt.df$R23,center = TRUE, scale = TRUE)
test.df$R24 <- scale(bankrupt.df$R24,center = TRUE, scale = TRUE)   
summary(test.df)
View(test.df)

#Some variable cleanups
bankrupt.df$NO <- NULL #Strip column NO
# Fortunate & awesome: D, the outcome variable, is now the first column
bankrupt.df$D=as.factor(bankrupt.df$D)   

#Convert D to a factor as that seems to be needed for he function confusionMatrix to work
#rescale (normalize)
library(scales)
View(bankrupt.df)
bankrupt.df$R1 <- rescale(bankrupt.df$R1,to=c(0,1)) 
bankrupt.df$R2 <- rescale(bankrupt.df$R2,to=c(0,1)) 
bankrupt.df$R3 <- rescale(bankrupt.df$R3,to=c(0,1)) 
bankrupt.df$R4 <- rescale(bankrupt.df$R4,to=c(0,1)) 
bankrupt.df$R5 <- rescale(bankrupt.df$R5,to=c(0,1)) 
bankrupt.df$R6 <- rescale(bankrupt.df$R6,to=c(0,1)) 
bankrupt.df$R7 <- rescale(bankrupt.df$R7,to=c(0,1)) 
bankrupt.df$R8 <- rescale(bankrupt.df$R8,to=c(0,1)) 
bankrupt.df$R9 <- rescale(bankrupt.df$R9,to=c(0,1)) 
bankrupt.df$R10 <- rescale(bankrupt.df$R10,to=c(0,1)) 
bankrupt.df$R11 <- rescale(bankrupt.df$R11,to=c(0,1))
bankrupt.df$R12 <- rescale(bankrupt.df$R12,to=c(0,1)) 
bankrupt.df$R13 <- rescale(bankrupt.df$R13,to=c(0,1)) 
bankrupt.df$R14 <- rescale(bankrupt.df$R14,to=c(0,1)) 
bankrupt.df$R15 <- rescale(bankrupt.df$R15,to=c(0,1)) 
bankrupt.df$R16 <- rescale(bankrupt.df$R16,to=c(0,1)) 
bankrupt.df$R17 <- rescale(bankrupt.df$R17,to=c(0,1)) 
bankrupt.df$R18 <- rescale(bankrupt.df$R18,to=c(0,1)) 
bankrupt.df$R19 <- rescale(bankrupt.df$R19,to=c(0,1)) 
bankrupt.df$R20 <- rescale(bankrupt.df$R20,to=c(0,1)) 
bankrupt.df$R21 <- rescale(bankrupt.df$R21,to=c(0,1)) 
bankrupt.df$R22 <- rescale(bankrupt.df$R22,to=c(0,1)) 
bankrupt.df$R23 <- rescale(bankrupt.df$R23,to=c(0,1))
bankrupt.df$R24 <- rescale(bankrupt.df$R24,to=c(0,1)) 
summary(bankrupt.df) #check all mins and maxs are 0 and 1, respectively.

#Checking for multi-variable outliers using coordinate plot
#You will probably have to resize the plotting window, and have to use zoom.
library(MASS)
par(mfcol = c(2,1))
parcoord(bankrupt.df[bankrupt.df$D == 0,-1],main = "D = 0")
parcoord(bankrupt.df[bankrupt.df$D == 1,-1],main = "D = 1")

## Keep in mind, as you treat outliers, that you only have about 130 rows of data!

#Checking for highly correlated columns
install.packages("reshape")
installed.packages("tidyr")
library(reshape)
library(ggplot2)
cor.mat <- round(cor(bankrupt.df[,-1]),2)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat,aes(x=X1,y=X2,fill=value))+geom_tile() + geom_text(aes(x=X1,y=X2,label=value))
# Thought: several variables have correlations in the .9s, providing some guidance for thinning the variables.

# Matrix scatterplots
# Suppose you want to produce a scatterplot matrix of D versus columns 3, 7, and 11
library(ggplot2)
library(GGally)
ggpairs(bankrupt.df[,c(1,3,7,11)])

#Principal components analysis
pcs.cor <- prcomp(bankrupt.df[,-c(1,2)],scale. = T)
summary(pcs.cor)
# Looks like PC1 through PC7 picks up more than 85% of the cumulative variance
bankrupt.df <- cbind(bankrupt.df[1:2],pcs.cor$x[,1:7]) #I decided to keep YR

#Partition into training and validation. Using a random partition of 60% for training, rest for validation
#Sample the data to get a training and validation data set
#Very thin data set. May have to stick to just two partitions.
set.seed(1)
training.rows <- sample(rownames(bankrupt.df),dim(bankrupt.df)[1]*0.6)
training <- bankrupt.df[training.rows,]
validate.rows <- setdiff(row.names(bankrupt.df),training.rows)
validate <- bankrupt.df[validate.rows,]
dim(training)
dim(validate)

#Run a basic tree classifier
library(rpart)
library(rpart.plot)
default.ct <- rpart(D ~ .,data=training,method="class")
prp(default.ct,type=1,extra=1,under=TRUE,split.font=1,varlen=-10)

#Now classify records in validate using the tree
library(caret)
default.ct.point.pred.train <- predict(default.ct,training,type="class")
confusionMatrix(default.ct.point.pred.train,training$D)
default.ct.point.pred.validate <- predict(default.ct,validate,type="class")
confusionMatrix(default.ct.point.pred.validate,validate$D)

#Lift chart for default CART classifier
library(caret)
probabilities <- predict(default.ct,validate,type="prob")
lift.data <- data.frame(validate[,1],probabilities[,2])
colnames(lift.data) <- c("actual","prob")
lift.CARTdefault <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.CARTdefault,plot="gain",main="Default Tree Classifier")

#First tabulate error as a functon of the complexity parameter CP
cv.ct <- rpart(D~.,data=bankrupt.df,method="class",cp=0.00001,minsplit=3,xval=5)
printcp(cv.ct)
# Now let's try a pruned tree (based upon Figure9.12)
pruned.ct <- prune(cv.ct,cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct,type=1,extra=1,split.font=1,varlen=-10)

#Now classify records in validate using the pruned tree
pruned.ct.point.pred.train <- predict(pruned.ct,training,type="class")
confusionMatrix(pruned.ct.point.pred.train,training$D)
pruned.ct.point.pred.validate <- predict(pruned.ct,validate,type="class")
confusionMatrix(pruned.ct.point.pred.validate,validate$D)

#Lift chart for pruned CART classifier
library(caret)
probabilities <- predict(pruned.ct,validate,type="prob")
lift.data <- data.frame(validate[,1],probabilities[,2])
colnames(lift.data) <- c("actual","prob")
lift.CARTpruned <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.CARTpruned,plot="gain",main="Pruned Tree Classifier")

#Logistic regression classifier
library(gains)
logit.reg <- glm(D~.,data=training,family="binomial")
options(scipen=999)  # Turn off scientific notation
pred <- predict(logit.reg,validate,type="response")
confusionMatrix(as.factor(ifelse(pred>0.5,1,0)),validate$D)
# Note: 0.5 in the previous line is the cutoff parameter - a hyperparameter

#Lift chart for the logistic regressionclassifier
probabilities <- predict(logit.reg,validate,type="response")
lift.data <- data.frame(validate[,1],probabilities)
colnames(lift.data) <- c("actual","prob")
lift.logit <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.logit,plot="gain",main="Logistic Regression Classifier")

# kNN classifier
# I chose to implement the kNN classifier in the caret package because it optimizes for k
library(caret)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(D ~.,data = training,method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
# The next command shows the tabulated values of k and the optimal value of k
knn_fit
# Now examine confusion matrix
test_pred <- predict(knn_fit, newdata = validate)
confusionMatrix(test_pred,validate$D)
# Present the lift chart
probabilities <- predict(knn_fit, newdata = validate,type="prob")
lift.data <- data.frame(validate[,1],probabilities[,2])
colnames(lift.data) <- c("actual","prob")
lift.logit <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.logit,plot="gain",main="kNN Classifier")

#Random Forest
library(randomForest)
rf <- randomForest(D~.,data=training,ntree=500,mtry=4,nodeize=5,importance=TRUE,norm.votes=TRUE)
varImpPlot(rf,type=1)
rf.pred <- predict(rf,validate)
confusionMatrix(rf.pred,validate$D)
# Now create the associated liftchart
probabilities <- predict(rf,validate,type="prob")[,2]
plift.data <- data.frame(validate[,1],probabilities)
colnames(lift.data) <- c("actual","prob")
lift.logit <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.logit,plot="gain",main="Random Forest")

# Boost algorithm
library(adabag)
library(rpart)
library(caret)
boost <- boosting(D~.,data=training)
pred<-predict(boost,validate)
confusionMatrix(as.factor(pred$class),validate$D)
#Now create the associated lift chart
pred <- predict(boost,validate,type="prob")
plift.data <- data.frame(validate[,1],pred$prob[,2])
colnames(lift.data) <- c("actual","prob")
lift.logit <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.logit,label="gain",main="Boost Classifier")

#XGBoost (Extreme Gradient Boosting) is a boosting algorithm based on 
#Gradient Boosting Machines.  XGboost applies regularization technique 
#to reduce overfitting, and it is one of the differences from the gradient 
# boosting. Another advantage of XGBoost over classical gradient boosting 
#is that it is fast in execution speed.
install.packages("xgboost")
library(xgboost)
#Note, the training x data should be matrix type to use in xgboost model.
train_x <- data.matrix(training[,-1])
train_y <- training[,1]
test_x <- data.matrix(validate[,-1])
test_y <- validate[,1]
#Note that column 1 is the D variable 
#Next, we need to convert the train and test data into xgb matrix type.
xgb_train <- xgb.DMatrix(data=train_x, label=train_y)
xgb_test <- xgb.DMatrix(data=test_x, label=test_y)
xgbc <- xgboost(data=xgb_train, max.depth=6, nrounds=50) # Model building step
pred <- predict(xgbc, xgb_test)
pred_y = as.factor((levels(test_y))[round(pred)])
confusionMatrix(test_y, pred_y)
#Now create the associated lift chart
probabilities <- predict(xgbc,xgb_test,objective="multi:softprob")
lift.data <- data.frame(validate[,1],probabilities)
colnames(lift.data) <- c("actual","prob")
lift.xgboost <- lift(relevel(as.factor(lift.data[,1]),ref="1")~prob,data=lift.data)
xyplot(lift.xgboost,plot="gain",main="XGBoost Classifier")
# The lift chart plot appears to be glitchy
