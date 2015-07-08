# Health and Economic Impact of Storm Events in the US
Leon Duplay  
8 July 2015  

## Synopsis

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This analysis uses U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database to evaluate health and economic impact of storm events. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The data is downloaded from the NOAA Storm Database ([link here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)) and the analysis addresses the following questions:

* Across the United States, which types of events are most harmful with respect to population health?
* Across the United States, which types of events have the greatest economic consequences?

## Data Acquisition & Processing

In this first step, we download and install all needed libraries (messages off to save space) as well as preparing the data for processing (download & uncompress).

Once the data is loaded, we process the data by cleaning the exponents encoding (from a seperate char column to the correct values), and group the values by type of event. The clean data is now ready for analysis and results.


```r
# install required R.utils package to uncompress .bz2 file
if (!"R.utils" %in% installed.packages()) install.packages("R.utils")

# load needed libraries
library(R.utils)
library(dplyr)
library(ggplot2)
```


```r
# Download and unzip data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("data/Stormdata.csv")) {
    download.file(url,destfile ="data/Stormdata.csv.bz2",method="internal")
    bunzip2("data/Stormdata.csv.bz2",destname="data/Stormdata.csv",overwrite=TRUE)
}

# Load data and transform to data.table
if(!exists("Stormdata"))
    Stormdata <- read.csv("data/Stormdata.csv", header=TRUE, row.names=NULL, 
                    stringsAsFactors=FALSE, na.strings = "NA")

# Clean dataset by subsetting columns
Stormdata <- select(Stormdata, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

# Clean exponents
exponent <- function(x) { switch(toupper(x), "K" = 10E3, "M" = 10E6, "B" = 10E9 , 1)}
Stormdata$PROPDMGEXP <- sapply(Stormdata$PROPDMGEXP, FUN="exponent")
Stormdata$CROPDMGEXP <- sapply(Stormdata$CROPDMGEXP, FUN="exponent")
```

## Results
