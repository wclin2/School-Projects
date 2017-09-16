### simulate train and test provided by Prof
# set.seed = 2
# data<-read.csv("loan.csv")
# ntest = round(0.25*length(data[,1]))
# test.id = sample(1:length(data[,1]), ntest)
# traindata = data[-test.id,]
# testdata = data[test.id,]
# testdata$loan_status = NULL
# write.csv(traindata, file= "train.csv",row.names=FALSE)
# write.csv(testdata, file= "test.csv",row.names=FALSE)


###
library(varhandle)
library(MLmetrics)
library(ranger)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

DataCleaning<-function(dataset)
{ 
  ######## delete varaibles we don't need
  dataset<-dataset[,!names(dataset) %in% c("member_id","emp_title","url","desc","title","zip_code","policy_code")]
  dataset<-dataset[,!names(dataset) %in% c("collections_12_mths_ex_med","mths_since_last_major_derog","annual_inc_joint","dti_joint","acc_now_delinq","tot_coll_amt","open_acc_6m","open_il_6m","open_il_12m","open_il_24m","mths_since_rcnt_il","total_bal_il","il_util","open_rv_12m","open_rv_24m","max_bal_bc","all_util","total_cu_tl","inq_last_12m")]
  
  ######## make sure numeric varaibles are numeric
  numeric_var = c("loan_amnt","funded_amnt","funded_amnt_inv","int_rate","installment","dti","inq_last_6mths","mths_since_last_delinq","mths_since_last_record","open_acc","pub_rec","revol_bal","revol_util","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","tot_cur_bal","total_rev_hi_lim","inq_fi","annual_inc","collection_recovery_fee","last_pymnt_amnt")
  for (i in 1:length(numeric_var)){
    if (!is.numeric(dataset[,names(dataset) %in% numeric_var[i]])){
      dataset[,names(dataset) %in% numeric_var[i]] = as.numeric(unfactor(dataset[,names(dataset) %in% numeric_var[i]]))
    }
  }
  
  ####### fill in 0 at nemeric variable N.A.
  for(i in 1:length(dataset)){
    if( is.numeric(dataset[[i]]))
    {
      dataset[,i][(which(is.na(dataset[,i])))]<-0
    }
  }
  return(dataset)
}

train<-DataCleaning(train)
test<-DataCleaning(test)

### turn factor into character in orter to merge train and test
factor2character = function (dataset){
  data.type = sapply(dataset, class)
  c = names(dataset)[which(data.type == 'factor')]  # categorical variables
  for (i in 1:length(c)){
    dataset[,names(dataset) %in% c[i]] = as.character(dataset[,names(dataset) %in% c[i]])
  }
  return(dataset)
}

train = factor2character(train)
test = factor2character(test)

####### form column "pred" in train
train[,(length(train)+1)]<-0
colnames(train)[(length(train))]<-"Pred"
for(i in 1:nrow(train))
  if(train$loan_status[i] %in% c('Default','Charged Off','Late (31-120 days)','Late (16-30 days)','Does not meet the credit policy. Status:Charged Off'))
  {train$Pred[i]<-1
  print(i)}
train<-train[,!names(train) %in% "loan_status"]

### merge and then divide
test$Pred = 2
temp = rbind(train, test)
data.type = sapply(temp, class)
c = names(temp)[which(data.type == 'character')]  # categorical variables
for (i in 1:length(c)){
  temp[,names(temp) %in% c[i]] = as.factor(temp[,names(temp) %in% c[i]])
}
train = temp[which(temp$id == train$id) , ]
test = temp[which(temp$id != train$id) , ]
test$Pred = NULL

### modeling
train<-train[,!names(train) %in% c("id")]
rmodel<-ranger(Pred~.,data=train)
Prob1<-predict(rmodel,test,type="response")
Prob1[which(Prob1$predictions<0)]<-0
Prob1[which(Prob1$predictions>1)]<-1
for(i in 1:nrow(test))
{
  if(test$collection_recovery_fee[i]>0)
  {
    Prob1$predictions[i]<-1
  }
  if((test$inq_fi[i]==0)==FALSE)
  {
    Prob1$predictions[i]<-0
  }
}

#####self evaluation
# LogLoss(Prob1$predictions,test$Pred)

#####
write.table(matrix(data=c(test$id,Prob1$predictions),ncol=2),file="mysubmission1.txt",
            row.names=FALSE,col.names=c("id","prob"),sep=',')