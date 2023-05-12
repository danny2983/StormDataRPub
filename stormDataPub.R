
## ----cache=TRUE----------------------------------------------------------
stormData <- read.csv(bzfile("./data/repdata_data_StormData.csv.bz2"))


## ------------------------------------------------------------------------
# number of unique event types
length(unique(stormData$EVTYPE))
stormData$EVTYPE <- toupper(stormData$EVTYPE)
# replace all punct. characters with a space
stormData$EVTYPE <- gsub("[[:blank:][:punct:]+]", " ", stormData$EVTYPE)
length(unique(stormData$EVTYPE))
# update the data frame
#stormData$EVTYPE <- eventTypes


## ------------------------------------------------------------------------
library(plyr)
totalEvnt <- ddply(stormData, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury
fatality <- head(totalEvnt[order(totalEvnt$fatalities, decreasing = T), ], 10)
injury<- head(totalEvnt[order(totalEvnt$injuries, decreasing = T), ], 10)

fatality[, c("EVTYPE", "fatalities")]
injury[, c("EVTYPE", "injuries")]

transformUnits <- function(e) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (e %in% c('h', 'H'))
        return(2)
    else if (e %in% c('k', 'K'))
        return(3)
    else if (e %in% c('m', 'M'))
        return(6)
    else if (e %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(e))) 
        return(as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid value.")
    }
}


## ----cache=TRUE----------------------------------------------------------
prop_dmg_exp <- sapply(stormData$PROPDMGEXP, FUN=transformUnits)
stormData$prop_dmg <- stormData$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(stormData$CROPDMGEXP, FUN=transformUnits)
stormData$crop_dmg <- stormData$CROPDMG * (10 ** crop_dmg_exp)

# Compute the economic loss by event type
library(plyr)
losses <- ddply(stormData, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss
losses <- losses[(losses$prop_dmg > 0 | losses$crop_dmg > 0), ]
prop_dmg_evnt <- head(losses[order(losses$prop_dmg, decreasing = T), ], 10)
crop_dmg_evnt <- head(losses[order(losses$crop_dmg, decreasing = T), ], 10)

prop_dmg_evnt[, c("EVTYPE", "prop_dmg")]
crop_dmg_evnt[, c("EVTYPE", "crop_dmg")]


## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

p1 <- ggplot(data=fatality,
             aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
    geom_bar(stat="identity") +
    coord_flip() +
    ylab("Fatalities") +
    xlab("Event") +
    theme(legend.position="none")

p2 <- ggplot(data=injury,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    ylab("Injuries") +
    xlab("Event") +
    theme(legend.position="none")

grid.arrange(p1, p2, top="Most deadly weather events US between (1950-2011)")


library(ggplot2)
library(gridExtra)

p1 <- ggplot(data=prop_dmg_evnt,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
    geom_bar(stat="identity") +
    coord_flip() +
    xlab("Event") +
    ylab("Property Damage USD (log)") +
    theme(legend.position="none")

p2 <- ggplot(data=crop_dmg_evnt,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    xlab("Event") +
    ylab("Crop Damage USD") + 
    theme(legend.position="none")

grid.arrange(p1, p2, top="Weather costs US between (1950-2011)")


