#Load libraries
source("scripts/libraries.R")

#load data (transaction)
load("output/transData.RDS") # transaction data individual items (sparse matrix)
load("output/prodData.RDS") # transaction data indivifual items (S3 data)
load("output/typeData.RDS") # transaction data product type (sparse matrix)

#Load data - Blackwell products info
blackData <- read.csv("data/existingProductAttributes.csv")

##Load Data - Blackwell demographics
blackDemo <- read_csv2("data/Blackwell_Demographic_Data.csv")
colnames(blackDemo)[1] <- "instore"

#Load Data - profit predictions Blackwell
profPred <- read.csv("data/BWNewProfitPredictions.csv")

#Treemap Blackwell Product types
#summary + visualisation of profit/volumne/margin/price blackwells product types
blackData <- blackData %>% filter(Product.Type != "Extended Warranty") %>% 
        mutate(profit = (Volume * Price * Profit.margin)/10^3, unitProfit = Price * Profit.margin)
blackData

blackProfit <- blackData %>% group_by(Product.Type) %>%
        summarise(absProfit = sum(profit), unitProfit = mean(unitProfit), volume = sum(Volume), 
                  meanMargin = mean(Profit.margin), avgPrice = mean(Price))

blackProfit

treemap(blackProfit, index = "Product.Type", vSize = "volume", title = "Blackwell Sales Volume by Product Type")
treemap(blackProfit, index = "Product.Type", vSize = "absProfit", title = "Blackwell Profit by Product Type")


#Treemap Electronidex Product Type
electroProducts <- itemFrequency(typeData) %>% as.data.frame() %>% rownames_to_column()
names(electroProducts) <- c("ProductType", "Support")


treemap(electroProducts, index = "ProductType", vSize = "Support", title = "Electronidex Sales Volume (est.) by Product Type")

#Blackwell instore v online 
onlinePurch <- blackDemo %>% group_by(instore) %>%
        summarise(count = n(), percent = (count/sum(onlinePurch$count))*100)
onlinePurch$instore[onlinePurch$instore == 0] <- "Online"
onlinePurch$instore[onlinePurch$instore == 1] <- "In Store"

g6 <- ggplot(onlinePurch, aes(instore, percent)) +
        geom_col(fill = "#31913c") +
        theme_bw() +
        ylab("% Transactions") + 
        xlab("Purchase Location") + 
        ggtitle("Blackwell: Transaction Location") +
        geom_text(aes(label = percent %>% round(0)), vjust = -0.3)
g6

#Blackwell items per purchase
numItems <- blackDemo %>% group_by(items) %>%
        summarise(num_items = n())
numItems <- numItems %>% mutate(percent = (num_items/sum(numItems$num_items))*100)

g7 <- ggplot(numItems, aes(items, percent)) +
        geom_col(fill = "#5c8cdb") +
        theme_bw() +
        ylab("% Transactions") + 
        xlab("Number of Items") + 
        ggtitle("Blackwell: Number of Items distribution")
g7

#Electronidex items per purchase
sizeTrans <- size(transData)
transTable <- table(sizeTrans) %>% as.data.frame()
transTable <- transTable %>% mutate(percent = (Freq/sum(transTable$Freq))*100)

g8 <- ggplot(transTable, aes(sizeTrans, percent)) +
        geom_col(fill = "#519b3b") +
        theme_bw() +
        ylab("% Transactions") + 
        xlab("Number of Items") + 
        ggtitle("Electronidex: Number of Items distribution")
g8
