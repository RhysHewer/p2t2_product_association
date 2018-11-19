#Load libraries
source("scripts/libraries.R")

#load data (transaction)
load("output/transData.RDS") # transaction data individual items (sparse matrix)
load("output/prodData.RDS") # transaction data indivifual items (S3 data)
load("output/typeData.RDS") # transaction data product type (sparse matrix)

#Load data - Blackwell products info
blackData <- read.csv("data/existingProductAttributes.csv")

##Are there any interesting patterns or item relationships within Electronidex's transactions?

#general EDA - high numbers of many-item transactions, mean of 4.83
summary(transData)
summary(itemFrequency(transData))

#item frequency - nothing interesting here
transFreq <- itemFrequency(transData) %>% as.data.frame %>% rownames_to_column()
colnames(transFreq) <- c("rowNames", "support")
transFreq.filter <- transFreq %>% arrange(desc(support)) %>% head(10) 
transFreq.filter

#rules creation
basketRulesItem <- apriori(transData, parameter = list(supp = 0.002, conf = 0.3, minlen = 2))
basketRulesItem

itemFrame <- data.frame(
                lhs = labels(lhs(basketRulesItem)),
                rhs = labels(rhs(basketRulesItem)), 
                basketRulesItem@quality)

#highest support
itemFrameSupport <- itemFrame %>% arrange(desc(support)) %>% head(5)
itemFrameSupport

#highest confidence
itemFrameConf <- itemFrame %>% arrange(desc(confidence)) %>% head(5)
itemFrameConf

#highest lift
itemFrameLift <- itemFrame %>% arrange(desc(lift)) %>% head(5)
itemFrameLift

#good metrics
itemFramePos <- itemFrame %>% filter(support > 0.025, confidence > 0.40, lift > 1 )
itemFramePos
