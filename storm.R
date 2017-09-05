setwd("~/Desktop/Coursera/RepData_PeerAssessment2/")
library(dplyr)
library(ggplot2)


if(!(exists("storm.raw"))){
    storm.raw <- read.csv("repdata%2Fdata%2FStormData.csv.bz2")
}

storm.raw$PUBLIC.HEALTH <- storm.raw$FATALITIES + storm.raw$INJURIES

storm.health <- storm.raw[,c("EVTYPE","PUBLIC.HEALTH")]
storm.health <- storm.health[storm.health$PUBLIC.HEALTH != 0, ]
storm.health.total <- aggregate(PUBLIC.HEALTH ~ EVTYPE,
                                data = storm.health,
                                FUN = sum,
                                na.rm = TRUE)
storm.health.total <- arrange(storm.health.total,desc(PUBLIC.HEALTH))
storm.health.total <- storm.health.total[storm.health.total$PUBLIC.HEALTH > 1000, ]

# Combines the Heat event
storm.health.total[2,2] <- storm.health.total[2,2] + storm.health.total[6,2]

# Combines the Thunderstorm Wind event
storm.health.total[3,2] <- storm.health.total[3,2] + storm.health.total[9,2]

# Combines the Flood event
storm.health.total[4,2] <- storm.health.total[4,2] + storm.health.total[7,2]

# Renames some of the event type names
storm.health.total$EVTYPE[c(2,3)] <- c("HEAT", "THUNDERSTORM WIND")

storm.health.total <- arrange(storm.health.total,desc(PUBLIC.HEALTH))


# Takes the top 5 most costly events
storm.health.total <- storm.health.total[1:5,]

options(scipen=5)
par(mar=c(5,11,2,3))
    # mfrow = c(2,1))
barplot(storm.health.total$PUBLIC.HEALTH,
        names.arg = head(storm.health.total$EVTYPE),
        horiz = TRUE,
        col = "red",
        las = 1,
        xlim = c(0,100000),
        xlab = "Amount of Human Fatalities and Injuries",
        main = "Most Harmful Weather Events on Public Health")


# Converts the monetary exponents to lower-case characters 
storm.raw$PROPDMGEXP <- tolower(as.character(storm.raw$PROPDMGEXP))
storm.raw$CROPDMGEXP <- tolower(as.character(storm.raw$CROPDMGEXP))

# Removes entries that do not have the monetary identifier assocaited with property and crop damage entries
storm.damages <- storm.raw[storm.raw$PROPDMGEXP %in% c("k", "m", "b"), ]
storm.damages <- storm.raw[storm.raw$CROPDMGEXP %in% c("k", "m", "b"), ]

storm.damages$PROPDMG <- ifelse(storm.damages$PROPDMGEXP == "k", storm.damages$PROPDMG/1000000, storm.damages$PROPDMG)
storm.damages$PROPDMG <- ifelse(storm.damages$PROPDMGEXP == "m", storm.damages$PROPDMG/1000, storm.damages$PROPDMG)

storm.damages$CROPDMG <- ifelse(storm.damages$CROPDMGEXP == "k", storm.damages$CROPDMG/1000000, storm.damages$CROPDMG)
storm.damages$CROPDMG <- ifelse(storm.damages$CROPDMGEXP == "m", storm.damages$CROPDMG/1000, storm.damages$CROPDMG)

storm.damages$DAMAGES.TOTAL <- storm.damages$PROPDMG + storm.damages$CROPDMG

storm.damages.total <- storm.damages[, c("EVTYPE", "DAMAGES.TOTAL")]
storm.damages.total$EVTYPE <- as.character(storm.damages.total$EVTYPE)
storm.damages.total <- aggregate(DAMAGES.TOTAL ~ EVTYPE,
                                 data = storm.damages.total,
                                 FUN = sum,
                                 na.rm = TRUE)

storm.damages.total <- arrange(storm.damages.total,desc(DAMAGES.TOTAL))
storm.damages.total <- storm.damages.total[storm.damages.total$DAMAGES.TOTAL > 10, ]

# Combines the Flood events
storm.damages.total[1,2] <- storm.damages.total[1,2] + storm.damages.total[2,2] + storm.damages.total[10,2]
storm.damages.total[2,2] <- NA
storm.damages.total[10,2] <- NA

# Combines Ice Storm and Hail
storm.damages.total[4,2] <- storm.damages.total[4,2] + storm.damages.total[5,2]
storm.damages.total[5,2] <- NA
storm.damages.total$EVTYPE[4] <- "ICE STORM/HAIL"

storm.damages.total <- arrange(na.omit(storm.damages.total),desc(DAMAGES.TOTAL))

par(mar=c(5,12,2,3))
barplot(storm.damages.total$DAMAGES.TOTAL,
        names.arg = storm.damages.total$EVTYPE,
        horiz = TRUE,
        col = "blue",
        las = 1,
        xlim = c(0,300),
        xlab = "Dollar Amount of Damage (in Billions)",
        main = "Most Impactful Weather Events on the Economy")

