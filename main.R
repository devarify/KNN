#Arif Yulianto | 1301168560
#Kelas: IFX-40-01

setwd("E:/PCAP/Adaboost/Latihan")

#====================pre-processing

#load Dataset
datatraining <- read.csv('datatraining.csv', header = TRUE, sep = ';')
datatesting <- read.csv('datatesting.csv', header = TRUE, sep = ';')

#random index
set.seed(9850)
gp <- runif(nrow(datatraining))
data_train <- datatraining[order(gp),]
View(data_train)

data_trains <- data_train[, -1] #menghilangkan variabel nama
datatrain_target <- data_trains[1:3500, 1:5]
datatest_target <- data_trains[3501:4000, 1:5]


summary(data_trains)
#normalisasi
train_datanorm <- as.data.frame(lapply(data_trains[,c("Like","Provokasi","Komentar", "Emosi")], normalisasi))
summary(train_datanorm)
train_datatr <- train_datanorm[1:3500, ]
train_datats <- train_datanorm[3501:4000,]

sqrt(3500)

#KNN
#library(class)


datatr_predict <- knn(train_datatr, train_datats, datatrain_target$Hoax, k = 81)
table(datatr_predict, datatest_target$Hoax)
mean(datatr_predict == datatest_target$Hoax)
datatrpredict <- cbind(data_train[3501:4000, ], datatr_predict)
View(datatrpredict)


#===========Function KNN
function (train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE) 
{
  train <- as.matrix(train)
  if (is.null(dim(test))) 
    dim(test) <- c(1, length(test))
  test <- as.matrix(test)
  if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
    stop("no missing values are allowed")
  p <- ncol(train)
  ntr <- nrow(train)
  if (length(cl) != ntr) 
    stop("'train' and 'class' have different lengths")
  if (ntr < k) {
    warning(gettextf("k = %d exceeds number %d of patterns", 
                     k, ntr), domain = NA)
    k <- ntr
  }
  if (k < 1) 
    stop(gettextf("k = %d must be at least 1", k), domain = NA)
  nte <- nrow(test)
  if (ncol(test) != p) 
    stop("dims of 'test' and 'train' differ")
  clf <- as.factor(cl)
  nc <- max(unclass(clf))
  Z <- .C(VR_knn, as.integer(k), as.integer(l), as.integer(ntr), 
          as.integer(nte), as.integer(p), as.double(train), as.integer(unclass(clf)), 
          as.double(test), res = integer(nte), pr = double(nte), 
          integer(nc + 1), as.integer(nc), as.integer(FALSE), as.integer(use.all))
  res <- factor(Z$res, levels = seq_along(levels(clf)), labels = levels(clf))
  if (prob) 
    attr(res, "prob") <- Z$pr
  res
}




accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,6] == test_data[i,7]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}


#===========================================

data_training <- datatraining[, -1]
View(data_training)
data_testing <- datatesting[, -1]
View(data_testing)


#Fungsi Normalisasi
normalisasi <- function(x) {
  return((x - min(x)) / (max(x) - min(x) ) )
}
normalisasi(c(1,2,3,4,5))
#memasukan fungsi normalisasi pada data
train_datanorm <- as.data.frame(lapply(data_training[,c("Like","Provokasi","Komentar","Emosi")], normalisasi))
summary(train_datanorm)
test_datanorm <- as.data.frame(lapply(data_testing[,c("Like","Provokasi","Komentar","Emosi")], normalisasi))
summary(test_datanorm)

#=================Perhitungan KNN
library(class)
data_predict <- knn(train_datanorm, test_datanorm, data_training$Hoax, k = 81)
View(data_predict)
datapredict <- cbind(datatesting, data_predict)
str(datapredict)
summary(datapredict)
#=================export ke xlsx
library(xlsx)
write.xlsx(datapredict, file = "Data_prediksi.xls", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


library(ggvis)
datatraining %>% ggvis(~Like, ~Provokasi, fill = ~Hoax) %>% layer_points()
datatraining %>% ggvis(~Like, ~Emosi, fill = ~Hoax) %>% layer_points()
datatraining %>% ggvis(~Like, ~Komentar, fill = ~Hoax) %>% layer_points()
datatraining %>% ggvis(~Provokasi, ~Emosi, fill = ~Hoax) %>% layer_points()
datatraining %>% ggvis(~Provokasi, ~Komentar, fill = ~Hoax) %>% layer_points()
datatraining %>% ggvis(~Emosi, ~Komentar, fill = ~Hoax) %>% layer_points()

#============Fungsi perhitungan Euclidean Distance
euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

