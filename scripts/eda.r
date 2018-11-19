#Load libraries
source("scripts/libraries.R")

#load data (transaction)
load("output/transData.RDS") # transaction data individual items (sparse matrix)
load("output/prodData.RDS") # transaction data indivifual items (S3 data)
load("output/typeData.RDS") # transaction data product type (sparse matrix)

#Load data - Blackwell products info
blackData <- read.csv("data/existingProductAttributes.csv")

##BLACKWELL DATA

#summary + visualisation of profit/volumne/margin/price blackwells product types
blackData <- blackData %>% filter(Product.Type != "Extended Warranty") %>% 
        mutate(profit = (Volume * Price * Profit.margin)/10^3, unitProfit = Price * Profit.margin)

blackProfit <- blackData %>% group_by(Product.Type) %>%
        summarise(absProfit = sum(profit), unitProfit = mean(unitProfit), volume = sum(Volume), 
                  meanMargin = mean(Profit.margin), avgPrice = mean(Price))

blackProfit


g1 <- ggplot(blackProfit, aes(reorder(Product.Type, -absProfit), absProfit, fill = unitProfit)) +
        geom_col() +
        scale_fill_gradient(low = "skyblue", high = "blue4") + 
        theme_bw() +
        ylab("Profit ($ thousands, timescale unknown)") + 
        xlab("Product Type") + 
        ggtitle("Blackwell: Profit per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0), legend.position = c(0.9, 0.85)) +
        geom_text(aes(label = absProfit %>% round(0)), vjust = -0.3)
g1

g2 <- ggplot(blackProfit, aes(reorder(Product.Type, -absProfit), volume)) +
        geom_col(fill = "#45c16a") +
        theme_bw() +
        ylab("Sales Volume") + 
        xlab("Product Type") + 
        ggtitle("Blackwell: Sales Volume per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_text(aes(label = volume %>% round(0)), vjust = -0.3)
g2

grid.arrange(ncol = 2, g1, g2)

###ELECTROINDEX DATA
##Product type frequency
electroProducts <- itemFrequency(typeData) %>% as.data.frame() %>% rownames_to_column()
names(electroProducts) <- c("ProductType", "Support")
electroProducts <- electroProducts %>% mutate(Occurance = Support * 9835)

g3 <- ggplot(electroProducts, aes(reorder(ProductType, -Occurance), Occurance)) +
        geom_col(fill = "#ae82bc") +
        theme_bw() +
        ylab("Sales Volume (month, est.)") + 
        xlab("Product Type") + 
        ggtitle("Electronidex: Sales Volume (est.) per Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
        geom_text(aes(label = Support %>% round(2)), vjust = -0.3)
g3

##comparison of blackwell and electronidex shared product types (not sure how useful this is)
compProd <- electroProducts %>% filter(ProductType == "accessories" | ProductType == "monitor" | ProductType == "laptop" |
                                       ProductType == "desktop" | ProductType == "printer" | ProductType == "printink" | ProductType == "tablet")

# assume that profit margin + price of electronidex products are similar to blackwell 
compProd$profitMargin <- c(0.05, 0.172, 0.107, 0.142, 0.139, 0.317, 0.17)
compProd$avgPrice <- c(60.1, 550, 754, 1052, 217, 51, 472)

compProd <- compProd %>% mutate(profit = (Occurance * profitMargin * avgPrice)/10^3)

g4 <- ggplot(compProd, aes(reorder(ProductType, -profit), profit)) +
        geom_col(fill = "#996210") +
        theme_bw() +
        ylab("Profit ($ thousands per month)") + 
        xlab("Product Type") + 
        ggtitle("Electronidex: Profit per shared Product Type") +
        theme(axis.text.x=element_text(angle = -45, hjust = 0), legend.position = c(0.9, 0.85)) +
        geom_text(aes(label = profit %>% round(0)), vjust = -0.3)
g4

grid.arrange(ncol = 2, g3, g4)



###Investigating trends in Electroindex Product Types

### Generate association rules and convert to dataframe for manipulation
basketRulesType <- apriori(typeData, parameter = list(supp = 0.01, conf = 0.1, minlen = 2))
basketFrameType <- data.frame(
        lhs = labels(lhs(basketRulesType)),
        rhs = labels(rhs(basketRulesType)), 
        basketRulesType@quality)

##Filter based on LHS shared products and support/confidence/lift metrics summarise these
sharedProducts <- ("accessories|monitor|laptop|desktop|printer|printink|tablet")
notSharedProducts <- ("mouse|keyboard|mousekeyboard|compheadphones|activeheadphones|compcord|speaker|compstand|
                      extdrive|smarthome")

basketLeft <- basketFrameType %>% filter(!grepl(notSharedProducts, lhs)) %>%
        filter(support > 0.05, confidence > 0.6, lift > 1)

basketLeftAvg <- basketLeft %>% group_by(rhs) %>%
        summarise(avgSupport = mean(support), avgConfidence = mean(confidence), avgLift = mean(lift))
basketLeftAvg        


##Filter based on RHS shared products and support/confidence/lift metrics summarise these
basketRight <- basketFrameType %>% filter(!grepl(notSharedProducts, rhs)) %>%
        filter(support > 0.05, confidence > 0.6, lift > 1)

g5 <- ggplot(basketRight, aes(rhs, lhs)) +
        geom_count(aes(size = confidence, colour = support)) +
        scale_colour_gradient(low = "skyblue", high = "darkblue") +
        theme_bw() +
        ylab("LHS Itemset") + 
        xlab("Shared Product Type") + 
        ggtitle("Association Rules: RHS shared products")
g5

basketRightFocus <- basketRight %>% filter(support > 0.075, confidence > 0.75, lift > 1.2)
basketRightFocus

recProdTypes <- basketRight %>% group_by(lhs) %>%
        summarise(sum = n()) %>%
        filter(sum > 1)

## Most frequent Electronidex products - Mouse & Keyboard combination/Active Headphones/Keyboard/Mouse
freq <- itemFrequency(transData) %>% as.data.frame %>% rownames_to_column()
colnames(freq) <- c("rowNames", "support")

is.keyboard <- freq$rowNames %in% keyboard.list
freq$prodType[is.keyboard] <- "keyboard"

is.mouse <- freq$rowNames %in% mice.list
freq$prodType[is.mouse] <- "mouse"

is.mousekeyboard <- freq$rowNames %in% mousekeyboard.list
freq$prodType[is.mousekeyboard] <- "mousekeyboard"

is.activeheadphones <- freq$rowNames %in% activeheadphones.list
freq$prodType[is.activeheadphones] <- "activeheadphones"

#Blackwell don't stock apple desktops or laptops (only smartphone) so remove apple peripherals
freq <- freq %>% filter(!str_detect(rowNames, "Apple"))

freq.filter <- freq %>% na.omit() %>% group_by(prodType) %>%
        top_n(n = 2, wt = support) %>% arrange(prodType)
freq.filter
