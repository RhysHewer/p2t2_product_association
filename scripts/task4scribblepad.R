library(dplyr)
library(arules)
library(tidyr)
library(readr)
library(arules)
library(arulesViz)
library(RColorBrewer)

#load data
data <- read.transactions("C:/Users/Rhys/Downloads/basket.csv", format = "basket", sep = ",")

## EDA
inspect(data)
summary(data)
itemLabels(data)
size(data)
summary(itemFrequency(data))

#dataframe of support values
freq <- itemFrequency(data) %>% as.data.frame %>% rownames_to_column()
colnames(freq) <- c("rowNames", "support")
freq.filter <- freq %>% arrange(desc(support)) %>% head(3) 
freq.filter

#form association rules
basketRules <- apriori(data, parameter = list(supp = 0, conf = 0.0, minlen = 2))
basketRules

inspect(basketRules)
basketRules %>% sort(by = "lift") %>% tail(10) %>% inspect()
class(basketRules)

#association rules as dataframe
basketFrame <- data.frame(
        lhs = labels(lhs(basketRules)),
        rhs = labels(rhs(basketRules)), 
        basketRules@quality)
basketFrame
basketRules@quality

#EDA of association rules
fagsPlus <- basketFrame %>% filter(lhs == "{fags}", support > 0.05, confidence > 0.3, lift > 1)
fagsPlus

buyFags <- basketFrame %>% filter(rhs == "{fags}", support > 0.05, confidence > 0.3, lift > 1)
buyFags

plot(basketRules, method="grouped")
scat <- plot(basketRules)

itemFrequencyPlot(data, topN = 3, cex.names = 0.7, col = brewer.pal(3,'Blues'))

gtest <- ggplot(freq.filter, aes(reorder(rowNames, -support), support, fill = support)) + 
        geom_col() +
        scale_fill_gradient(low = "skyblue", high = "blue4") +
        theme_bw() +
        ylab("Support") + 
        xlab("Product Type") + 
        ggtitle("Support per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_text(aes(label = support %>% round(2)), vjust = -0.3)
gtest

####
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


##Convert rules to dataframe !!!! Can then use dplyr to manipulate.
basketFrame <- data.frame(
        lhs = labels(lhs(basketRules)),
        rhs = labels(rhs(basketRules)), 
        basketRules@quality)



