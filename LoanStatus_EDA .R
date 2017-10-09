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
test = testdata
train = traindata
###
library(varhandle)
library(MLmetrics)
library(ranger)
library(plyr)
library(dplyr)
library(stringr)
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
data = temp
bk = data

impactModel = function(xcol, depvar) {
  n = length(depvar)
  p = sum(depvar)/n
  # duplicate output for NA (average NA towards grand uniform average) 
  x = c(xcol,xcol)
  y = c(depvar, depvar)
  x[(1+n):(2*n)] = NA
  levelcounts = table(x, y, useNA="always")
  condprobmodel = (levelcounts[,2]+p)/(levelcounts[,1]+levelcounts[,2]+1) 
  # apply model example: applyImpactModel(condprobmodel,data[,varname])
  condprobmodel
}

################################################################
datatrain = train
datatest = test
#========================================================================
# emp_length
datatrain[which(datatrain$emp_length == 'RENT'), 'emp_length'] = 'n/a'
# unique(datatest$emp_length)[!unique(datatest$emp_length) %in% unique(datatrain$emp_length)]
# lv_emp = unique(datatrain$emp_length)[!unique(datatrain$emp_length) %in% unique(datatest$emp_length)]
# datatrain[which(datatrain$emp_length == lv_emp), 'emp_length'] = 'n/a'
#========================================================================
# home_ownership
# data[which(data$home_ownership == 'NONE' | data$home_ownership == 'ANY' | data$home_ownership =='65000.0'), 'home_ownership'] = 'OTHER'
impact_home_ownership = impactModel(datatrain$home_ownership, datatrain$Pred)
home_ownership_lookup = data.frame(home_ownership = c(names(impact_home_ownership)),
                    impact_home_ownership = c(as.numeric(impact_home_ownership)))
datatrain = datatrain %>%
  left_join(home_ownership_lookup, by='home_ownership')

lv_home = unique(datatest$home_ownership)[!unique(datatest$home_ownership) %in% unique(datatrain$home_ownership)]

datatest[which(datatest$home_ownership == lv_home), 'home_ownership'] = NA

datatest = datatest %>%
  left_join(home_ownership_lookup, by='home_ownership')

#========================================================================
# verification_status
datatrain[which(datatrain$verification_status == 'Nov-2015'), 'verification_status'] = NA
impact_verification_status = impactModel(datatrain$verification_status, datatrain$Pred)
verification_status_lookup = data.frame(verification_status = c(names(impact_verification_status)),
                                   impact_verification_status = c(as.numeric(impact_verification_status)))
datatrain = datatrain %>%
  left_join(verification_status_lookup, by='verification_status')

lv_veri = unique(datatest$verification_status)[!unique(datatest$verification_status) %in% unique(datatrain$verification_status)]

datatest[which(datatest$verification_status == lv_veri), 'verification_status'] = NA

datatest = datatest %>%
  left_join(verification_status_lookup, by='verification_status')

#========================================================================
# issue_d
impact_issue_d = impactModel(datatrain$issue_d, datatrain$Pred)
issue_d_lookup = data.frame(issue_d = c(names(impact_issue_d)),
                                        impact_issue_d = c(as.numeric(impact_issue_d)))
datatrain = datatrain %>%
  left_join(issue_d_lookup, by='issue_d')

lv_issue = unique(datatest$issue_d)[!unique(datatest$issue_d) %in% unique(datatrain$issue_d)]

datatest[which(datatest$issue_d == lv_issue), 'issue_d'] = NA

datatest = datatest %>%
  left_join(issue_d_lookup, by='issue_d')
#========================================================================
# purpose
data[which(data$purpose == 'Debt consolidation'), 'purpose'] = 'debt_consolidation'
data[which(data$purpose == 'Paying off my son\'s dept...'), 'purpose'] ='educational'

#========================================================================
# zip_code
data = data %>%
  mutate(zipcode = str_extract(zip_code, '\\d')) %>%
  select(-zip_code)
data[which(is.na(data$zipcode)), 'zipcode'] = '9'

#========================================================================
# delinq_2yrs
dput(levelofcharacter[,'delinq_2yrs'])
delinq.lookup <- data.frame(delinq_2yrs = c("", "10.0", "11.0", "12.0", "13.0", 
                                            "14.0", "15.0", "16.0", "17.0", "18.0",                                              "19.0", "20.0", "21.0", "22.0", "24.0",                                              "26.0", "27.0", "29.0", "30.0", "39.0",                                              "Jan-1997", "Jul-2006", 
                                            "2.0", "3.0", "4.0", "5.0", "6.0", "7.0",                                             "8.0", "9.0", "0.0", "1.0"),
                            New.delinq_2yrs = c(rep("other", 22),
                                                "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", 
                                                "8.0", "9.0", "0.0", "1.0"),
                            stringsAsFactors = FALSE)
data = data %>%
  left_join(delinq.lookup, by = 'delinq_2yrs') %>%
  mutate(delinq_2yrs = New.delinq_2yrs) %>%
  select(-New.delinq_2yrs)

#========================================================================
# earliest_cr_line
data[which(data$earliest_cr_line == '' | data$earliest_cr_line == '0.0' | data$earliest_cr_line == '1.0'), 'earliest_cr_line'] = 'Apr-1955'

data = data %>%
  mutate(earliest_cr_line_season = str_extract(earliest_cr_line, '[a-zA-Z]+')) %>%
  select(-earliest_cr_line)

data[which(data$earliest_cr_line_season == 'Jan' | data$earliest_cr_line_season == 'Feb' | data$earliest_cr_line_season == 'Mar'), 'earliest_cr_line_season'] = 'Spring'

data[which(data$earliest_cr_line_season == 'Apr' | data$earliest_cr_line_season == 'May' | data$earliest_cr_line_season == 'Jun'), 'earliest_cr_line_season'] = 'Summer'

data[which(data$earliest_cr_line_season == 'Jul' | data$earliest_cr_line_season == 'Aug' | data$earliest_cr_line_season == 'Sep'), 'earliest_cr_line_season'] = 'Fall'

data[which(data$earliest_cr_line_season == 'Oct' | data$earliest_cr_line_season == 'Nov' | data$earliest_cr_line_season == 'Dec'), 'earliest_cr_line_season'] = 'Winter'

#========================================================================
# total_acc
levelof_totacc = data %>%
  group_by(total_acc) %>%
  dplyr::summarize(Count = n())
data$New.total_acc = 1
data = data %>%
  left_join(levelof_totacc, by = 'total_acc') %>%
  mutate(New.total_acc = ifelse(Count < 2000, 'other', total_acc)) %>%
  mutate(total_acc = New.total_acc) %>%
  select(-New.total_acc) %>%
  select(-Count)
length(table(data$total_acc))

#========================================================================
# initial_list_status
data[which(data$initial_list_status == '9761.44' | data$initial_list_status == '0.0'), 'initial_list_status'] = 'w'

#========================================================================
# last_credit_pull_d
data = data %>%
  mutate(last_credit_pull_d_year = str_extract(last_credit_pull_d, '(\\d)+')) %>%
  mutate(last_credit_pull_d_season = str_extract(last_credit_pull_d, '[a-zA-Z]+')) %>%select(-last_credit_pull_d)

data[which(data$last_credit_pull_d_season == 'Jan' | data$last_credit_pull_d_season == 'Feb' | data$last_credit_pull_d_season == 'Mar'), 'last_credit_pull_d_season'] = 'Spring'
data[which(data$last_credit_pull_d_season == 'Apr' | data$last_credit_pull_d_season == 'May' | data$last_credit_pull_d_season == 'Jun'), 'last_credit_pull_d_season'] = 'Summer'
data[which(data$last_credit_pull_d_season == 'Jul' | data$last_credit_pull_d_season == 'Aug' | data$last_credit_pull_d_season == 'Sep'), 'last_credit_pull_d_season'] = 'Fall'
data[which(data$last_credit_pull_d_season == 'Oct' | data$last_credit_pull_d_season == 'Nov' | data$last_credit_pull_d_season == 'Dec'), 'last_credit_pull_d_season'] = 'Winter'

data[which(data$last_credit_pull_d_year == '0' | data$last_credit_pull_d_year == '2007' | data$last_credit_pull_d_year == '2008'), 'last_credit_pull_d_year'] = '2009'
data[which(is.na(data$last_credit_pull_d_year)), 'last_credit_pull_d_year'] = data[which(is.na(data$last_credit_pull_d_year))-1, 'last_credit_pull_d_year']

data[which(is.na(data$last_credit_pull_d_season)), 'last_credit_pull_d_season'] = data[which(is.na(data$last_credit_pull_d_season))+1, 'last_credit_pull_d_season']


################################################################

temp = data
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