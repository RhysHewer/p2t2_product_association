#Load libraries
source("scripts/libraries.R")

#load csv data
transData <- read.transactions("data/ElectronidexTransactions2017.csv", format = "basket", sep = ",")
prodData <- read.csv("data/ElectronidexTransactions2017.csv", header = FALSE)


### Loading PDF data
text <- pdf_text("data/ElectronidexItems2017.pdf")

text2 <- strsplit(text, "\n")
text2 <- text2 %>% unlist(recursive = TRUE)

text3 <- gsub("\r", "", text2)

prod.list <- text3 %>% as.data.frame()

##create product type lists



laptop.list <- prod.list[2:11,1] %>% as.character() %>% trimws("l")
desktop.list <- prod.list[13:21,1] %>% as.character() %>% trimws("l")
monitor.list <- prod.list[23:32,1] %>% as.character() %>% trimws("l")
mice.list <- prod.list[34:43,1] %>% as.character() %>% trimws("l")
keyboard.list <- prod.list[45:53,1] %>% as.character() %>% trimws("l")
mousekeyboard.list <- prod.list[55:63,1] %>% as.character() %>% trimws("l")
compheadphones.list <- prod.list[64:74,1] %>% as.character() %>% trimws("l")
activeheadphones.list <- prod.list[76:81,1] %>% as.character() %>% trimws("l")
compcords.list <- prod.list[83:91,1] %>% as.character() %>% trimws("l")
accessories.list <- prod.list[93:96,1] %>% as.character() %>% trimws("l")
speakers.list <- prod.list[98:106,1] %>% as.character() %>% trimws("l")
printers.list <- prod.list[108:112,1] %>% as.character() %>% trimws("l")
printink.list <- prod.list[114:118,1] %>% as.character() %>% trimws("l")
compstands.list <- prod.list[120:124,1] %>% as.character() %>% trimws("l")
comptablets.list <- prod.list[126:130,1] %>% as.character() %>% trimws("l")
extdrives.list <- prod.list[132:136,1] %>% as.character() %>% trimws("l")
smarthome.list <- prod.list[138:142,1] %>% as.character() %>% trimws("l")

#string replacement lists function
prodReplace <- function(x,y){
        list1 <- x %>% as.data.frame()
        list1$name <- y
        list2 <- list1$name
        names(list2) <- list1$.
        list2
}

laptop.replace <- prodReplace(laptop.list, "laptop")
desktop.replace <- prodReplace(desktop.list, "desktop")
monitor.replace <- prodReplace(monitor.list, "monitor")
mice.replace <- prodReplace(mice.list, "mouse")
keyboard.replace <- prodReplace(keyboard.list, "keyboard")
mousekeyboard.replace <- prodReplace(mousekeyboard.list, "mousekeyboard")
compheadphones.replace <- prodReplace(compheadphones.list, "compheadphones")
activeheadphones.replace <- prodReplace(activeheadphones.list, "activeheadphones")
compcords.replace <- prodReplace(compcords.list, "compcord")
accessories.replace <- prodReplace(accessories.list, "accessories")
speakers.replace <- prodReplace(speakers.list, "speaker")
printers.replace <- prodReplace(printers.list, "printer")
printink.replace <- prodReplace(printink.list, "printink")
compstands.replace <- prodReplace(compstands.list, "compstand")
comptablets.replace <- prodReplace(comptablets.list, "tablet")
extdrives.replace <- prodReplace(extdrives.list, "extdrive")
smarthome.replace <- prodReplace(smarthome.list, "smarthome")

#backup replace lists
laptop.replace <- c("LG Touchscreen Laptop" = "laptop", "Acer Aspire" = "laptop", "HP Laptop" = "laptop",
                    "Apple Macbook Pro" = "laptop", "Apple MacBook Air" = "laptop", "Dell Laptop" = "laptop",
                    "Alienware AW17R4-7345SLV-PUS 17\" Laptop" = "laptop", "HP Notebook Touchscreen Laptop PC" = "laptop",
                    "Alienware Laptop" = "laptop", "Apple MacBook Pro" = "laptop", 
                    "Eluktronics Pro Gaming Laptop" = "laptop", "ASUS Chromebook" = "laptop")

desktop.replace <- c("Lenovo Desktop Computer" = "desktop", "iMac" = "desktop", "HP Desktop" = "desktop",
                     "ASUS Desktop" = "desktop", "Dell Desktop" = "desktop", "Intel Desktop" = "desktop",
                     "Acer Desktop" = "desktop", "CYBERPOWER Gamer Desktop" = "desktop", "Dell 2 Desktop" = "desktop")

monitor.replace <- c("Acer Monitor" = "monitor", "LG Monitor" = "monitor", "ASUS Monitor" = "monitor", 
                     "ASUS 2 Monitor" = "monitor", "Dell Monitor" = "monitor", "Samsung Monitor" = "monitor",
                     "Sceptre Monitor" = "monitor", "ViewSonic Monitor" = "monitor", "AOC Monitor" = "monitor",
                     "HP Monitor" = "monitor")

mice.replace <- c("3-Button Mouse" = "mouse", "Logitech Wireless Mouse" = "mouse", 
                  "Microsoft Basic Optical Mouse" = "mouse", "Logitech 3-button Mouse" = "mouse",
                  "Redragon Gaming Mouse" = "mouse", "HP Wireless Mouse" = "mouse", "Generic Black 3-Button" = "mouse",
                  "Wireless Portable Mouse" = "mouse", "Gaming Mouse Professional" = "mouse", 
                  "Slim Wireless Mouse" = "mouse")

keyboard.replace <- c("HP USB Keyboard" = "keyboard", "Logitech Wireless Keyboard" = "keyboard", 
                      "Rii LED Keyboard" = "keyboard", "Logitech Keyboard" = "keyboard", 
                      "Backlit LED Gaming Keyboard" = "keyboard", "Dell Wired Keyboard" = "keyboard",
                      "Apple Wired Keyboard" = "keyboard", "Apple Wireless Keyboard" = "keyboard", 
                      "Apple Magic Keyboard" = "keyboard")

mousekeyboard.replace <- c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo" = "mousekeyboard", 
                           "Logitech Desktop MK120 Mouse and keyboard Combo" = "mousekeyboard",
                           "Logitech MK270 Wireless Keyboard and Mouse Combo" = "mousekeyboard",       
                           "Dell KM117 Wireless Keyboard & Mouse" = "mousekeyboard",
                           "EagleTec Wireless Combo Keyboard and Mouse" = "mousekeyboard",
                           "Microsoft Wireless Comfort Keyboard and Mouse" = "mousekeyboard",         
                           "Microsoft Wireless Desktop Keyboard and Mouse" = "mousekeyboard",
                           "Rii LED Gaming Keyboard & Mouse Combo" = "mousekeyboard",                
                           "Logitech MK360 Wireless Keyboard and Mouse Combo" = "mousekeyboard")

compheadphones.replace <- c("Computer Headphones" = "compheadphones", "Zombie Gaming Headset" = "compheadphones",                      
                            "Logitech ClearChat Headset" = "compheadphones", 
                            "Panasonic On-Ear Stereo Headphones RP-HT21" = "compheadphones", 
                            "PC Gaming Headset" = "compheadphones", "Kensington Headphones" = "compheadphones",                      
                            "Logitech Stereo Headset" = "compheadphones", "Koss Home Headphones" = "compheadphones",
                            "Microsoft Headset" = "compheadphones", "Ailihen Stereo Headphones" = "compheadphones",                 
                            "XIBERIA Gaming Headset" = "compheadphones")

activeheadphones.replace <- c("Apple Earpods" = "activeheadphones", "Monster Beats By Dr Dre" = "activeheadphones",                   
                              "Otium Wireless Sports Bluetooth Headphones" = "activeheadphones", 
                              "Panasonic In-Ear Headphone" = "activeheadphones", 
                              "APIE Bluetooth Headphones" = "activeheadphones",
                              "Philips Flexible Earhook Headphones" = "activeheadphones",
                              "Panasonic On-Ear Stereo Headphones" = "activeheadphones",
                              "APIE Bluetooth Headphone" = "activeheadphones",
                              "Otium Wireless Sports Bluetooth Headphone" = "activeheadphones",
                              "Philips Flexible Earhook Headphone" = "activeheadphones") 

compcords.replace <- c("HDMI Cable 6ft" = "compcord", "Ethernet Cable" = "compcord", 
                       "Etekcity Power Extension Cord Cable" = "compcord", "Audio Cable" = "compcord",
                       "VGA Monitor Cable" = "compcord", "iPhone Charger Cable" = "compcord",               
                       "HDMI Adapter" = "compcord", "USB Cable" = "compcord", "Samsung Charging Cable" = "compcord")        

accessories.replace <- c("Microsoft Office Home and Student 2016" = "accessories", "Computer Game" = "accessories",
                         "Belkin Mouse Pad" = "accessories", "Large Mouse Pad" = "accessories")

speakers.replace <- c("Cambridge Bluetooth Speaker" = "speaker", "JBL Splashproof Portable Bluetooth Speaker" = "speaker",
                      "DOSS Touch Wireless Bluetooth" = "speaker", "Logitech Multimedia Speakers" = "speaker",             
                      "Rokono Mini Speaker" = "speaker", "Cyber Acoustics" = "speaker",                           
                      "Bose Companion Speakers" = "speaker", "Mackie CR Speakers" = "speaker",                        
                      "Sonos" = "speaker")    

printers.replace <- c("Epson Printer" = "printer", "HP Wireless Printer" = "printer", 
                      "Canon Office Printer" = "printer", "Brother Printer" = "printer", "DYMO Label Manker" = "printer") 

printink.replace <- c("Epson Black Ink" = "printink", "HP Black & Tri-color Ink" = "printink", "Canon Ink" = "printink",
                      "Brother Printer Toner" = "printink", "DYMO Labeling Tape" = "printink", 
                      "printer Toner" = "printink")    

compstands.replace <- c("Halter Acrylic Monitor Stand" = "compstand", "Height-Adjustable Standing Desk"  = "compstand",
                        "Multi Media Stand"  = "compstand", "Halter Mesh Metal Monitor Stand"  = "compstand",
                        "Full Motion Monitor Mount"  = "compstand")

comptablets.replace <- c("iPad" = "tablet", "iPad Pro" = "tablet", "Fire HD Tablet" = "tablet",
                         "Samsung Galaxy Tab" = "tablet", "Kindle" = "tablet", "tabletlet" = "tablet", 
                         "tablet Pro" = "tablet")

extdrives.replace <- c("1TB Portable External Hard Drive" = "extdrive", "2TB Portable External Hard Drive" = "extdrive",
                       "5TB Desktop Hard Drive" = "extdrive", "Slim 2TB Portable External Hard Drive" = "extdrive",
                       "3TB Portable External Hard Drive" = "extdrive", "Slim extdrive" = "extdrive")  

smarthome.replace <- c("Apple TV" = "smarthome", "Google Home" = "smarthome", "Smart Light Bulb" = "smarthome",
                       "Fire TV Stick" = "smarthome", "Roku Express" = "smarthome")

#string replacement function
stringReplace <- function(z){
        inList <- z
        repLace <- prodData
        repLace <- sapply(repLace, function(x) str_replace_all(x, inList))
        data.frame(repLace)
}

productList <- c(laptop.replace, desktop.replace, monitor.replace, mice.replace, keyboard.replace, mousekeyboard.replace,
               compheadphones.replace, activeheadphones.replace, compcords.replace, accessories.replace, speakers.replace,
               printers.replace, printink.replace, compstands.replace, comptablets.replace, extdrives.replace, smarthome.replace)

typePrep <- stringReplace(productList)

##backup
#string replacement function
typePrep <- as.data.frame(sapply(prodData, function(x) str_replace_all(x, laptop.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, desktop.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, monitor.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, mice.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, keyboard.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, mousekeyboard.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, compheadphones.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, activeheadphones.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, compcords.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, accessories.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, speakers.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, printers.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, printink.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, compstands.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, comptablets.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, extdrives.replace)))
typePrep <- as.data.frame(sapply(typePrep, function(x) str_replace_all(x, smarthome.replace)))

 

#CSV creation/re-import
write.table(typePrep, file = "output/typePrep.csv", sep=",", row.names = FALSE, col.names = FALSE)
typeData <- read.transactions("output/typePrep.csv", format = "basket", sep=",")
inspect(typeData[1:10])
summary(typeData)
itemLabels(typeData)
