#Load libraries
source("scripts/libraries.R")

data <- read.transactions("data/ElectronidexTransactions2017.csv", format = "basket", sep = ",")
summary(data)
inspect(data[1:5])
itemLabels(data) %>% sample(10)
