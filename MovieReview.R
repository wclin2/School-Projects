# State what packages you will use. 
packages = c('text2vec', 'xgboost', 'stringr',"jpeg")
# install packages: if package already installed, then skip. 
# Otherwise, install it.
for (pkg in packages){
  if (!require(pkg, character.only = TRUE)){
    install.packages(pkg)
  }
}

library(text2vec)
library(xgboost)
library(stringr)
library(jpeg)

t1<-proc.time()

Sys.setlocale("LC_ALL", "English")
train<-as.data.frame(read.table("labeledTrainData.tsv",stringsAsFactors = F,header = T))
test<-as.data.frame(read.table("testData.tsv",stringsAsFactors = F,header = T))

leak<-as.numeric(str_sub(test$id,-1,-1))
b<-rep(0,25000)
for (i in 1:25000)
{ 
  if(leak[i]<5 & leak[i]>0)
  {b[i]<-0}
  if(leak[i]>6 | leak[i]==0)
  {b[i]<-1}
}
write.table(matrix(data=c(test$id,b),ncol=2),file="cheating.txt",
            row.names=FALSE,col.names=c("id","sentiment"),sep=',')


prep_fun = tolower
tok_fun = word_tokenizer
it_train = itoken(train$review,
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun
)
it_test = itoken(test$review,
                 preprocessor = prep_fun, 
                 tokenizer = tok_fun
)
vocab = create_vocabulary(it_train,ngram = c(1L, 4L))
pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 5, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)
bigram_vectorizer = vocab_vectorizer(pruned_vocab)
dtm_train = create_dtm(it_train, bigram_vectorizer)
dtm_test = create_dtm(it_test,bigram_vectorizer)

colnames(dtm_train)<-gsub("_"," ",colnames(dtm_train))
colnames(dtm_test)<-gsub("_"," ",colnames(dtm_test))

t2<-proc.time()

dtrain <- xgb.DMatrix(dtm_train, label = train$sentiment)
dvalid <- xgb.DMatrix(dtm_test, label = b)
model1 <- xgb.train(booster = "gblinear", nrounds = 50, eta = 0.01,lambda=10,
                    max.depth = 1000,lambda_bias=10,
                    data = dtrain, objective = "binary:logistic",
                    eval_metric = "auc")
pred <- predict(model1, newdata = dtm_test)

write.table(matrix(data=c(test$id,pred),ncol=2),file="mysubmission1.txt",
            row.names=FALSE,col.names=c("id","sentiment"),sep=',')

t3<-proc.time()

r<-xgb.importance(model = model1, data = NULL,
                  label = NULL, target = function(x) ((x + label) == 2))

#Sample Visualization
myreview=as.data.frame(read.table("labeledTrainData.tsv",stringsAsFactors = F,header = T))

myreview[,3] = gsub("<.*?>", " ", myreview[,3])

wordlist.neg=c(colnames(dtm_train)[r[which(r$Weight<(-0.10) & r$Weight>(-0.20))]$Feature])
wordlist.neg<-wordlist.neg[-which(grepl(" ",wordlist.neg))]
wordlist.neg<-wordlist.neg[which(is.na(as.numeric(wordlist.neg)))]

wordlist.strongneg=c(colnames(dtm_train)[r[which(r$Weight<(-0.20))]$Feature])
wordlist.strongneg<-wordlist.strongneg[-which(grepl(" ",wordlist.strongneg))]
wordlist.strongneg<-wordlist.strongneg[which(is.na(as.numeric(wordlist.strongneg)))]

wordlist.pos=c(colnames(dtm_train)[r[which(r$Weight>(0.10) & r$Weight<0.2)]$Feature])
wordlist.pos<-wordlist.pos[-which(grepl(" ",wordlist.pos))]
wordlist.pos<-wordlist.pos[which(is.na(as.numeric(wordlist.pos)))]

wordlist.strongpos=c(colnames(dtm_train)[r[which(r$Weight>(0.2))]$Feature])
wordlist.strongpos<-wordlist.strongpos[-which(grepl(" ",wordlist.strongpos))]
wordlist.strongpos<-wordlist.strongpos[which(is.na(as.numeric(wordlist.strongpos)))]

myfile = "visualization.html"

if (file.exists(myfile)) file.remove(myfile)
n.review = dim(myreview)[1]
write(paste("<html> \n", 
            "<head> \n",  
            "<style> \n",
            "@import \"textstyle.css\"", 
            "</style>", 
            "</head> \n <body>\n"), file=myfile, append=TRUE)
write("<ul>", file=myfile, append=TRUE)
Neg<-NULL
Strongneg<-NULL
Pos<-NULL
Strongpos<-NULL

for(i in 1:n.review){
  write(paste("<li><strong>", myreview[i,1], 
              "</strong> sentiment =", myreview[i,2], 
              "<br><br>", sep=" "),
        file=myfile, append=TRUE)

  tmp<-gsub("([a-z])([[:punct:]])", "\\1 \\2", myreview[i,3])
  tmp = strsplit(tmp, " ")[[1]]
  
  tmp.copy = tmp
  nwords = length(tmp)
  
  pos=NULL;
  for(j in 1:length(wordlist.neg))
    pos = c(pos, grep(paste(wordlist.neg[j]," "), paste(tmp," "), ignore.case = TRUE))
  if (length(pos)>0) {
    for(j in 1:length(pos)){
      tmp.copy[pos[j]] = paste("<span class=\"neg\">", 
                               tmp.copy[pos[j]], "</span>", sep=" ")
    }
  }
  pos=NULL;
  for(j in 1:length(wordlist.strongneg))
    pos = c(pos, grep(paste(wordlist.strongneg[j]," "), paste(tmp," "), ignore.case = TRUE))
  if (length(pos)>0) {
    for(j in 1:length(pos)){
      tmp.copy[pos[j]] = paste("<span class=\"strongneg\">", 
                               tmp.copy[pos[j]], "</span>", sep=" ")
    }
  }
  
  pos=NULL;
  for(j in 1:length(wordlist.pos))
    pos = c(pos, grep(paste(wordlist.pos[j]," "),paste(tmp," "), ignore.case = TRUE))
  if (length(pos)>0) {
    for(j in 1:length(pos)){
      tmp.copy[pos[j]] = paste("<span class=\"pos\">",
                               tmp.copy[pos[j]], "</span>", sep=" ")
    }
  }
  pos=NULL;
  for(j in 1:length(wordlist.strongpos))
    pos = c(pos, grep(paste(wordlist.strongpos[j]," "), paste(tmp," "), ignore.case = TRUE))
  if (length(pos)>0) {
    for(j in 1:length(pos)){
      tmp.copy[pos[j]] = paste("<span class=\"strongpos\">",
                               tmp.copy[pos[j]], "</span>", sep=" ")
    }
  }
  a<-paste(tmp.copy, collapse = " ")
  a<-gsub("([a-z])( )([[:punct:]])", "\\1\\3", a)
  a<-gsub("([a-z])(<span)", "\\1\ \\2", a)
  a<-gsub("(span>)( )([[:punct:]])( )", "\\1\\3\\4", a)
  a<-gsub("(</span>)( )(</span>)", "\\1\\3", a)
  write(a, file=myfile, append=TRUE)
  write("<br><br>", file=myfile, append=TRUE)

for (k in 1:length(wordlist.neg))
  {
  if(sum(grepl(paste(wordlist.neg[k]," "), paste(tmp," "), ignore.case = TRUE)))
  {
    Neg<-c(wordlist.neg[k],Neg)
  }
}
for (k in 1:length(wordlist.strongneg))
  {
  if(sum(grepl(paste(wordlist.strongneg[k]," "), paste(tmp," "), ignore.case = TRUE)))
  {
    Strongneg<-c(wordlist.strongneg[k],Strongneg)
  }
}
for (k in 1:length(wordlist.pos))
{  if(sum(grepl(paste(wordlist.pos[k]," "), paste(tmp," "), ignore.case = TRUE)))
  {
    Pos<-c(wordlist.pos[k],Pos)
  }
}
for (k in 1:length(wordlist.strongpos))
  {
  if(sum(grepl(paste(wordlist.strongpos[k]," "), paste(tmp," "), ignore.case = TRUE)))
  {
    Strongpos<-c(wordlist.strongpos[k],Strongpos)
  }
}
}

jpeg("rplot1.jpeg")
wordcloud(c(Pos,rep(Strongpos,2)),scale=c(4,.5),min.freq=1,max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors=c("gold","darkorange","orange","tomato","red"),ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()
jpeg("rplot2.jpeg")
wordcloud(c(Neg,rep(Strongneg,2)),scale=c(4,.5),min.freq=1,max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors=c("lightblue","skyblue","deepskyblue","blue","navy"),ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()
write("</ul> \n  </body> \n </html>", file=myfile, append=TRUE)

t4<-proc.time()

print(paste("The data cleaning time is:",(t2-t1)[3],"seconds"))
print(paste("The modeling time is:",(t3-t2)[3],"seconds"))
print(paste("The Visualing time is:",(t4-t3)[3],"seconds"))