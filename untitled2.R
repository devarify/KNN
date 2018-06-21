data_training <- as.data.frame(lapply(datatraining[,c("Like","Provokasi","Komentar","Emosi")], normalisasi))
data_testing <- as.data.frame(lapply(datatesting[,c("Like","Provokasi","Komentar","Emosi")], normalisasi))


uji1 <- knn(train = data_training, test = data_testing, cl = datatraining$Hoax, k=5)

dim(data_training)
dim(data_testing)
dim(datatraining)

table(datatraining$Hoax, uji1)

View(uji1)

write.xlsx(uji1, file = "uji1.xls", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)