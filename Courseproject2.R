##JohnHopkins University course in 
#Reproducible Research with Dr Roger Peng

#COURSE PROJECT 2

library(dplyr)
library(ggplot2)

#Load data
path <- getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
              , destfile = paste(path, "StormData.csv.bz2", sep = "/"))

dir() #Check if data present

#Import dataset
StormData <- read.csv(bzfile("StormData.csv.bz2"), header = TRUE)

#Check dataset
head(StormData)
str(StormData)


# Health impact -----------------------------------------------------------

storm.fatalities <- StormData %>% select(EVTYPE, FATALITIES) %>% 
                    group_by(EVTYPE) %>% 
                    summarise(total = sum(FATALITIES)) %>% 
                    arrange(-total)

head(storm.fatalities, 10)

storm.injuries <- StormData %>% select(EVTYPE, INJURIES) %>% 
                  group_by(EVTYPE) %>% 
                  summarise(total = sum(INJURIES)) %>% 
                  arrange(-total)

head(storm.injuries, 10)


# Economic impact ---------------------------------------------------------

storm.damage <- StormData %>% select(EVTYPE, PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)

head(storm.damage)

sort(table(storm.damage$PROPDMGEXP), decreasing = TRUE)[1:10]
sort(table(storm.damage$CROPDMGEXP), decreasing = TRUE)[1:10]

#Transfrom character values to numeric values in PROPDMGEXP & CROPDMGEXP
storm.damage <- StormData %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

#- blank -> *0
#- (-)   -> *0
#- (?)   -> *0
#- (+)   -> *1
#- H, h  -> hundreds (*100)
#- K, k  -> kilos    (*1,000)
#- M, m  -> millions (*1,000,000)
#- B,b   -> billions (*1,000,000,000)

symbols <- sort(unique(as.character(storm.damage$PROPDMGEXP)))
symbols
# [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K" "m" "M"
multiplier <- c(0, 0, 0, 1, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10^9, 10^2, 10^2, 10^3, 10^6, 10^6)

storm.convert <- data.frame(symbols, multiplier)
storm.convert

#Fuse symbols with numeric values
storm.damage$property.multiplier <- storm.convert$multiplier[match(storm.damage$PROPDMGEXP, storm.convert$symbols)]
storm.damage$crop.multiplier <- storm.convert$multiplier[match(storm.damage$CROPDMGEXP, storm.convert$symbols)]

storm.property <- storm.damage %>%
                  mutate(DMG = PROPDMG*property.multiplier) %>%
                  select(EVTYPE, DMG) %>%
                  arrange(-DMG)

head(storm.property)

storm.crop <- storm.damage %>%
              mutate(DMG = CROPDMG*crop.multiplier) %>%
              select(EVTYPE, DMG) %>%
              arrange(-DMG)

head(storm.crop)


storm.damage.overall <- storm.damage %>% 
                        mutate(PROPDMG = PROPDMG*property.multiplier) %>%
                        mutate(CROPDMG = CROPDMG*crop.multiplier) %>% 
                        mutate(TOTAL.DMG = PROPDMG+CROPDMG)
head(storm.damage.overall)

storm.damage.total <- storm.damage.overall %>% 
                      group_by(EVTYPE) %>% 
                      summarize(TOTAL.DMG.EVTYPE = sum(TOTAL.DMG)) %>% 
                      arrange(-TOTAL.DMG.EVTYPE) 

head(storm.damage.total)


# Plot data ---------------------------------------------------------------

##Health

#Fatalities
ggplot(storm.fatalities[1:10,], aes(x = reorder(EVTYPE, -total), y = total)) +
    geom_bar(stat="identity", fill = "coral") +
    scale_y_continuous(breaks=seq(0,6000,1000)) +
    ggtitle("Top 10 Weather Events with Highest Total Fatalities")+
    labs(x="Event type", y="Total Fatalities")+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

#Injuries
ggplot(storm.injuries[1:10,], aes(x = reorder(EVTYPE, -total), y = total)) +
    geom_bar(stat="identity", fill = "seagreen") +
    scale_y_continuous(breaks=seq(0,100000,25000)) +
    ggtitle("Top 10 Weather Events with Highest Total Injuries")+
    labs(x="Event type", y="Total Injuries") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))


#Combine fatalities & injuries
storm.fatalities$Type <- "Fatalities"
storm.injuries$Type <- "Injuries"

storm.health <- rbind(storm.injuries, storm.fatalities)

storm.health <- storm.health %>%
                arrange(-total, EVTYPE)

head(storm.health,10)


ggplot(storm.health[1:10,], aes(x = EVTYPE, y = total, fill = Type)) +
  coord_flip() +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = c("coral", "seagreen")) +
  ggtitle("Top 10 Weather Events with Highest Impacts on Human Health") + 
  labs(x="Event type", y="Total health impacts") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=0, vjust=0.5, hjust=1))


##Economy

#Property
ggplot(storm.property[1:20,], aes(x=reorder(EVTYPE, -DMG), y=DMG/10^9)) +
  geom_bar(stat="identity", fill = "midnightblue") + 
  scale_y_continuous(breaks=seq(0,150,25)) +
  ggtitle("Top 10 Weather Events causing Highest Property Damage") + 
  labs(x="Event type", y="Total Ecomic impact (billion USD)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

#Crops
ggplot(storm.crop[1:20,], aes(x=reorder(EVTYPE, -DMG), y=DMG/10^9)) +
  geom_bar(stat="identity", fill = "mediumorchid") + 
  ggtitle("Top 10 Weather Events causing Highest Crop Damage") + 
  labs(x="Event type", y="Total Ecomic impact (billion USD)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))


#Combine property & crop damages
storm.property$Type <- "Property"
storm.crop$Type <- "Crop"

storm.damage <- rbind(storm.property, storm.crop)

storm.damage <- storm.damage %>%
                arrange(-DMG, EVTYPE)


ggplot(storm.damage[1:20,], aes(x = EVTYPE, y = DMG/10^9, fill = Type)) +
  coord_flip() +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("mediumorchid", "midnightblue")) +
  ggtitle("Top 10 Weather Events with Greatest Economic Consequences") + 
  labs(x="Event type", y="Total Ecomic impact (billion USD)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
