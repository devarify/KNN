str(dataset_hoax)
set.seed(9850)
gp <- runif(nrow(dataset_hoax))

dataset <- dataset_hoax[order(gp),]

str(dataset)
dim(dataset)

summary(dataset[,c("Like","Provokasi","Komentar","Emosi")])

normalisasi <- function(x) {
  return((x - min(x)) / (max(x) - min(x) ) )
}

normalisasi(c(1,2,3,4,5))

dataset_n <- as.data.frame(lapply(dataset[,c("Like","Provokasi","Komentar","Emosi","Hoax")], normalisasi))
View(dataset_n)
str(dataset_n)
summary(dataset_n)

hoax_training <- dataset_n[1:3000, ]
hoax_testing <- dataset_n[3001:4000, ]

hoax_training_target <- dataset[1:3000, 2:6]
hoax_testing_target <- dataset[3001:4000, 2:6]

data_testing <- datatesting

require(class)

sqrt(4000)
c1 <- hoax_training_target

m1 <- knn(train = hoax_training, test = hoax_testing, cl = hoax_training_target$Hoax, k=5)
m1
plot(m1)
table(m1)
View(m1)

table(hoax_testing_target$Hoax, m1)

dim(hoax_training)
dim(hoax_testing)
dim(hoax_training_target)