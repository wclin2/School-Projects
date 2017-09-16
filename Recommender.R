# State what packages you will use. 
packages = c("dplyr","devtools","data.table","recosystem")
# install packages: if package already installed, then skip. 
# Otherwise, install it.
for (pkg in packages){
  if (!require(pkg, character.only = TRUE)){
    install.packages(pkg)
  }
}
#To install packages "rectools", run following codes:
#install_github("Pooja-Rajkumar/rectools")
library(dplyr)
library(devtools)
library(data.table)
library(rectools)
library(recosystem)

#=========================================================
t1<-proc.time()

# ratings data
train = read.table('train.dat', sep = ':', colClasses = c('integer', 'NULL'), header = FALSE)
test = read.csv("test.csv")
colnames(train) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
colnames(test) = c('ID', 'UserID', 'MovieID')

# movies data
txt <- readLines("movies.dat", encoding = "latin1")
txt_split <- lapply(strsplit(txt, "::"), function(x) as.data.frame(t(x), stringsAsFactors=FALSE))
movies <- do.call(rbind, txt_split)
names(movies) <- c("MovieID", "Title", "Genres")

#=========================================================
# genres
genres2 <- as.data.frame(tstrsplit(movies$Genres, '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:6)

genre_list = c('Action','Adventure','Animation','Children\'s','Comedy','Crime','Documentary','Drama','Fantasy','Film-Noir','Horror','Musical','Mystery','Romance','Sci-Fi','Thriller','War','Western')

genre_matrix <- matrix(0,3884,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

genre_matrix2$MovieID = movies$MovieID
genre_matrix3 = genre_matrix2
genre_matrix3$MovieID = as.numeric(genre_matrix3$MovieID)
train = train %>%
  left_join(genre_matrix3, by = 'MovieID')
test = test %>%
  left_join(genre_matrix3, by = 'MovieID')

#=========================================================
# UserData
txt <- readLines("users.dat")
txt <- gsub("::",",",txt)
userdat<-read.table(text=txt[1:6040],sep=",")
names(userdat) <- c("UserID","Gender","Age","Occupation","Zip-code")
userdat$Gender = as.numeric(userdat$Gender)
userdat$`Zip-code` = as.numeric(userdat$`Zip-code`)

train = train %>%
  left_join(userdat, by = 'UserID')
train$Timestamp = NULL

test = test %>%
  left_join(userdat, by = 'UserID')
test$Timestamp = NULL

rownames(train) = 1:nrow(train)
train[, c("Action", "Adventure", "Animation",
          "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
          "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",
          "Thriller", "War", "Western", "Gender", "Age", "Occupation")] <- 
  lapply(train[, c("Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western", "Gender", "Age", "Occupation")], as.factor)

rownames(test) = 1:nrow(test)
test[, c("Action", "Adventure", "Animation",
         "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
         "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",
         "Thriller", "War", "Western", "Gender", "Age", "Occupation")] <- 
  lapply(test[, c("Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western", "Gender", "Age", "Occupation")], as.factor)
test<-test[,c(-1)]

t2<-proc.time()

train_set<-data_memory(train$UserID,train$MovieID,rating=train$Rating)
test_set<-data_memory(test$UserID,test$MovieID)

r = Reco()
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                               costp_l1 = 0, costq_l1 = 0,
                               nthread = 4, niter = 50))
r$train(train_set, opts = c(opts$min, nthread = 4, niter = 50))
pred1<-r$predict(test_set,out_memory())
pred1<-mean(train$Rating)-mean(pred1)+pred1
for(i in 1:length(pred1))
{
  if(pred1[i]>5)
    {pred1[i]<-5}
  if(pred1[i]<1)
  {pred1[i]<-1}
}

model1 = trainMM(train[,c(1:25)])
pred2 = predict(model1, test[,1:24])
pred2[which(is.na(pred2))] = mean(train$Rating)
pred2<-mean(train$Rating)-mean(pred2)+pred2
for(i in 1:length(pred1))
{
  if(pred2[i]>5)
  {pred2[i]<-5}
  if(pred2[i]<1)
  {pred2[i]<-1}
}

t3<-proc.time()

print(paste("The data cleaning time is:",(t2-t1)[3],"seconds"))
print(paste("The modeling time is:",(t3-t2)[3],"seconds"))

test = read.csv("test.csv")
write.table(matrix(data=c(test$ID,test$user,test$movie,pred1),ncol=4),file="mysubmission1.csv",
            row.names=FALSE,col.names=c("ID", "user", "movie","rating"),sep=",")
write.table(matrix(data=c(test$ID,test$user,test$movie,pred2),ncol=4),file="mysubmission2.csv",
            row.names=FALSE,col.names=c("ID", "user", "movie","rating"),sep=",")
