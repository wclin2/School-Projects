#STAT 542 Project 1
#Team RSS

#Load Environment
library('mice') #md.pattern
library("moments")
library("randomForest")
library("e1071")

#Followings are the packages we once used in the project, 
#But they are not included in the final code.
# library('MASS')
# library('Hmisc') #describe
# library('timeDate') # kurtosis skewness
# library('ellipse') # plotcorr
# library('reshape2') # subset(melt(correlation), value > .75)
# library('VIM') # aggr
# library('stats')
# library('Metrics')




#Data Cleaning Part

#Loading Data
train <- read.csv("train.csv")
test <- read.csv("test.csv")


#Examine number of rows and columns
trainn<-nrow(train)
testn<-nrow(test)
price = train[,81] #1460
data = rbind(train[,-81], test)
dim(data) #2919, 80

#Eliminate columns with too many NAs
numNA = colSums(apply(data[, -c(1, 81)], 2, is.na))
number_of_missing = numNA[which(numNA != 0)]  # number of NA's
data_type = sapply(data[,names(which(numNA != 0))], class)  # type of data
cbind(number_of_missing, data_type)
drops = c("Alley", "PoolQC", "Fence", "MiscFeature","FireplaceQu")
data = data[ , !(names(data) %in% drops)]

#Classfy data columns by catagories
data.type = sapply(data[, -c(1, ncol(data))], class)  
cat_var = names(data)[which(c(NA, data.type, NA) == 'factor')]  # categorical variables
numeric_var =  names(data)[which(c(NA, data.type, NA) == 'integer')]  # continuous variables
names(data)
ID<-data[,1]
data <- data[,-1]

#Use mice function to fill up NAs
#https://www.rdocumentation.org/packages/mice/versions/2.25/topics/mice
tempData <- mice(data,m=10,maxit=1,meth='pmm',seed=500)
cdata <- complete(tempData)

#Log transformation
price <- log(price + 1)

#Log transformation for columns with skewness over 0.75
skewed_feats = sapply(cdata[, numeric_var], skewness)
skewed_feats = numeric_var[which(skewed_feats > 0.75)]
tcdata <- cdata
for(j in skewed_feats) {
  tcdata[, j] = log(cdata[, j] + 1)
}


#Data cleaned
dim(tcdata)
names(tcdata)

train= tcdata[1:trainn,]
test= tcdata[(trainn+1):nrow(tcdata),]
train= cbind(train,price)
head(train)
dim(train)
testx <- read.csv("test.csv")

#Multivariate Model Approach to remove outliers bt cooksd
mod <- lm(price ~ ., data= train[,c(numeric_var, "price")])
cooksd <- cooks.distance(mod)
#In general use, those observations that have a cook¡¦s distance greater than 4 times the mean may be classified as influential. This is not a hard boundary.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
length(influential)
dim(train)
train = train[-influential, ]


#Run time:
#   user   system   elapsed 
#  671.16   21.13    709.30

#Model Part

#StepAIC(simple linear model)

full.model = lm( price ~ ., data = train[,c(numeric_var, "price")])
stepAIC = step(full.model, trace=0, direction="both")            
AIC.model = eval(stepAIC$call)
Ytest.pred = predict(AIC.model, newdata= test);
#Return predicted price
yHat = exp(Ytest.pred) - 1
#Write Submission
write.table(matrix(data=c(testx$Id,yHat),ncol=2),file="mysubmission1.txt",
            row.names=FALSE,col.names=c("ID","SalePrice"),sep=',')

#Self evaluation times: 500 times

#Self-evaluation quantile: 
#       0%       25%       50%       75%      100% 
#0.1069510 0.1305383 0.1399910 0.1524692 0.1931223 

#Self-evaluation standard error of rmse: 0.01492717 / 500 times

#Kaggle evaluation: 0.13687

#Run time:
#   user   system   elapsed 
#   1.53    0.18     1.81


#Random Forest method
rp.model <- randomForest(price ~ .,data = train)
Ytest.pred = predict(rp.model, newdata= test)
#Return prediction price
yHat = exp(Ytest.pred) - 1
#Write Submission
write.table(matrix(data=c(testx$Id,yHat),ncol=2),file="mysubmission2.txt",
            row.names=FALSE,col.names=c("ID","SalePrice"),sep=',')

#Self evaluation times: 500 times

#Self-evaluation quantile: 

#     0%        25%        50%        75%       100% 
#0.1020275   0.1142701   0.1196771   0.1239120   0.1391330 

#Self-evaluation standard error of rmse: 0.00676

#Kaggle evaluation: 0.14300

#Run time:
#   user   system   elapsed 
#   0.00    0.01     0.01


#SVM method
svm.model= best.tune(svm,price~., data= train,ranges=list(cost=c(1,2,3)))
#Set kernel as default(radial) and choose the best cost value of SVM.

#(Theoretical 2.1 is the best cost value in kaggle submitting, but does not make
#much difference. Also, set precision to 0.1 in cost-choice is time consuming and 
#not efficient enough)

Ytest.pred = predict(svm.model, newdata= test)
#Return prediction price
yHat = exp(Ytest.pred) - 1
#Write txt file
write.table(matrix(data=c(testx$Id,yHat),ncol=2),file="mysubmission3.txt",
            row.names=FALSE,col.names=c("ID","SalePrice"),sep=',')

#Self evaluation times: 500 times

#Self-evaluation quantile: 

#0%         25%        50%        75%       100% 
#0.08980102 0.11620165 0.12502560 0.13400159 0.16680586

#Self-evaluation standard error of rmse: 0.01256764/ 500 times

#Kaggle evaluation: 0.12081 

#Run time:
#user   system   elapsed 
#0.02    0.02     0.05

#Note: The run time did not include the code finding the cost parameter.


