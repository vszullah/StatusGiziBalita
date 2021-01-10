# StatusGiziBalita
library(randomForest)
library(caret)
library(readxl)

gizi <- read_excel("Data Gizi.xlsx")
gizi

TrainingIndex <- createDataPartition(gizi$Status, p=0.8, list = FALSE)
TrainingSet <- gizi[TrainingIndex,] # Training Set
TestingSet <- gizi[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
TrainSet

gizi$Status = factor(gizi$Status)
model <- randomForest(Status ~ ., data=gizi)

saveRDS(model, "model.rds")
model
