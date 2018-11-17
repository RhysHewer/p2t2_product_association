#Load libraries
source("scripts/libraries.R")

#load data (transaction)
load("output/transData.RDS") # transaction data individual items (sparse matrix)
load("output/prodData.RDS") # transaction data indivifual items (S3 data)
load("output/typeData.RDS") # transaction data product type (sparse matrix)

#Load data - Blackwell products info
blackData <- read.csv("data/existingProductAttributes.csv")

##BLACKWELL DATA
#EDA of blackwell profitability
blackData <- blackData %>% filter(Product.Type != "Extended Warranty") %>% mutate(profit = (Volume * Price * Profit.margin)/10^3, unitProfit = Price * Profit.margin)
blackProfit <- blackData %>% group_by(Product.Type) %>%
        summarise(absProfit = sum(profit), unitProfit = mean(unitProfit), volume = sum(Volume))
blackProfit

g1 <- ggplot(blackProfit, aes(reorder(Product.Type, -absProfit), absProfit, fill = unitProfit)) +
        geom_col() +
        scale_fill_gradient(low = "skyblue", high = "blue4") + 
        theme_bw() +
        ylab("Profit ($ thousands)") + 
        xlab("Product Type") + 
        ggtitle("Profit per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_text(aes(label = absProfit %>% round(0)), vjust = -0.3)
g1

g2 <- ggplot(blackProfit, aes(reorder(Product.Type, -absProfit), volume)) +
        geom_col(fill = "#45c16a") +
        theme_bw() +
        ylab("Sales Volume") + 
        xlab("Product Type") + 
        ggtitle("Sales Volume per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_text(aes(label = volume %>% round(0)), vjust = -0.3)
g2


###ELECTROINDEX DATA
##Product type frequency

electroProducts <- itemFrequency(typeData) %>% as.data.frame() %>% rownames_to_column()
names(electroProducts) <- c("ProductType", "Support")
electroProducts <- electroProducts %>% mutate(Occurance = Support * 9835)

g3 <- ggplot(electroProducts, aes(reorder(ProductType, -Support), Support)) +
        geom_col(fill = "#ae82bc") +
        theme_bw() +
        ylab("Frequency") + 
        xlab("Product Type") + 
        ggtitle("Frequency per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_text(aes(label = Support %>% round(2)), vjust = -0.3)
g3


###Investigating trends in Electroindex Product Types
#preliminary data investigation
summary(typeData) #summary of dataset
inspect(typeData[1:5]) #inspect specific transaction sets
itemLabels(typeData) %>% sample(10) # random sample of 10 item names
size(typeData[1:10])
summary(itemFrequency(typeData)) # summary of support values of dataset

#zero items transaction (return to this!!)

itemFrequency(typeData)




#preliminary visualisation
itemFrequencyPlot(typeData, support = 0.05)
itemFrequencyPlot(typeData, topN = 10, cex.names = 0.7, col = brewer.pal(8, 'Dark2'))

typeData %>% sample(25) %>% image()

#apriori package
basketRules <- apriori(typeData, parameter = list(supp = 0.05, conf = 0.10, minlen = 2))
basketRules

#Rules specific to desktops
basketRules.Desktop <- apriori (typeData, parameter = list(supp = 0.05, conf = 0.10, minlen = 2), # get rules that lead to buying 'desktop'
                                appearance = list (default="lhs",rhs="desktop"), control = list (verbose=F))
basketRules.Desktop %>% sort(by = "confidence") %>% head(5) %>% inspect()



basketRules %>% sort(by = "lift") %>% head(5) %>% inspect()
basketRules %>% sort(by = "lift") %>% tail(5) %>% inspect()

basketRules %>% sort(by = "support") %>% head(5) %>% inspect()
basketRules %>% sort(by = "support") %>% tail(5) %>% inspect()

basketRules %>% sort(by = "confidence") %>% head(5) %>% inspect()
basketRules %>% sort(by = "confidence") %>% tail(5) %>% inspect()

inspect(basketRules[1:5])
summary(basketRules)

ItemRules <- basketRules %>% subset(items %in% "ASUS Chromebook")
ItemRules %>% inspect()

##Results visualisation
plot(basketRules)
plot(basketRules[1:10], method="graph", control = list( type = "items")) 

#Shiny app
ruleExplorer(basketRules)
