---
title: "Severe Weather events in the US - 1950 to 2011"
subtitle: " A story of tornadoes, heatwaves and floods"
author: "Lamine Gaye"
date: "08/07/2019"
output: 
  html_document:
    keep_md: true
---




# Synopsis
The aim of this report is to examine past storms and severe weather events in the US in order to determine, in one hand which types of events are most harmful with respect to population health, and on the other hand which types of events have the greatest economic consequences. Answering these questions may guide us on how to prioritize resources in the area of disaster prevention and relief. 
In order to answer these questions, we have obtained and examined data from the US National Oceanic and Atmospheric Administration's (NOAA) storm database, specifically data from 1950 to 2011.   
In our analysis, we find that **tornadoes have been the most dangerous for human life and health**, ahead of  excessive heat and floods. In terms of economic impact, **floods cause the most damage to property and crops**, ahead of hurricanes and tornadoes. 

# Data Processing 

## Downloading and reading the data
We first download the data from the NOAA Storm Database. A copy of the data can be found 
[here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2). It is a CSV file, and we read in contents in memory.   
For further information about the data, please check:  
- National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)  
- National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).  



```r
library(tidyverse)
library(lubridate)
library(stringr)
```


```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- file.path(".", "StormData.csv.bz2")
download.file(url, method = "curl", destfile = destfile)
stormdata <- as_tibble(read.csv(destfile, stringsAsFactors = FALSE))
```

## Exploring the data
We first look at the basic properties of the data inn order to orient our analysis. 

```r
dim(stormdata)
```

```
## [1] 902297     37
```

```r
names(stormdata)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```
It is an extensive database with over 900,000 lines and 37 variables. We see variable names such as "EVTYPE" which can help us identify the different types of weather related events. We will focus on variables "FATALITIES" and "INJURIES" to assess impact on human life, and "PROPDMG" and "CROPDMG" for the economic impact. 


```r
#How many event types do we have?
evtype <- stormdata$EVTYPE
length(unique(evtype))
```

```
## [1] 985
```

```r
head(unique(evtype),20)
```

```
##  [1] "TORNADO"                   "TSTM WIND"                
##  [3] "HAIL"                      "FREEZING RAIN"            
##  [5] "SNOW"                      "ICE STORM/FLASH FLOOD"    
##  [7] "SNOW/ICE"                  "WINTER STORM"             
##  [9] "HURRICANE OPAL/HIGH WINDS" "THUNDERSTORM WINDS"       
## [11] "RECORD COLD"               "HURRICANE ERIN"           
## [13] "HURRICANE OPAL"            "HEAVY RAIN"               
## [15] "LIGHTNING"                 "THUNDERSTORM WIND"        
## [17] "DENSE FOG"                 "RIP CURRENT"              
## [19] "THUNDERSTORM WINS"         "FLASH FLOOD"
```
There are **over 600 types of events**, but looking at a small sample we already notice quite a few duplicates. We will have to remove some of these duplicates to have a clearer picture when we aggregate the data.  


```r
stormdata <- stormdata %>% 
        mutate(year = year(mdy_hms(BGN_DATE))) %>% # year of event
        mutate(EVTYPE= str_to_upper(EVTYPE)) %>% # upper case
        mutate(EVTYPE= ifelse(grepl("HEAT",EVTYPE),"EXCESS HEAT",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("UNSEASONABLY WARM",EVTYPE),"EXCESS HEAT",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("FLOOD",EVTYPE),"FLOOD",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("RIP CURRENT",EVTYPE),"RIP CURRENT",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("THUNDERSTORM WIND",EVTYPE),"THUNDERSTORM WIND",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("TSTM WIND",EVTYPE),"THUNDERSTORM WIND",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("COLD",EVTYPE),"EXTREME COLD",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("LOW TEMPERATURE",EVTYPE),"EXTREME COLD",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("HURRICANE",EVTYPE),"HURRICANE",EVTYPE))
```


Our approach to identify the type of events with  most impact is to **aggregate casualties and damages across all years** of the data. However, we also want to **incorporate the idea of frequency**, in order to keep the time dimension in our analysis. This will help us differentiate rare event types that have great impact and other events that occur almost every year, and aggregate to large tolls slowly and almost predictably. We expect this frequency dimension to affect public response policies.  



```r
# How many years of dota do we have?
years <- length(unique(year(mdy_hms(stormdata$BGN_DATE))))
years
```

```
## [1] 62
```


# Results
## Impact on human health 

To assess the impact of events of human health, we filter the data for events that caused at least one death or at least one injury, in order to take the widest possible picture, while discarding events recorded solely for their economic impact.  
After grouping data by event type, we aggregate fatalities and injuries in each group, across the whole US, across all the years the data was recorded.  We also keep track of the event count, that is the number of times an event type has  occurred over the years. We introduce the notion of **frequency: what percentage of years this particular type of event has occurred and made casualties?**  
Finally we order event types by total death toll, injuries and frequency. 


```r
harmful <- stormdata %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>%
        group_by(EVTYPE) %>%
        summarise(deaths= sum(FATALITIES),injuries = sum(INJURIES),count = n(),
                  frequency = round(n_distinct(year) /years *100, 1))%>%
        arrange(desc(deaths), desc(injuries), desc(frequency))
```
We can now have a look at the 10 weather event types that have most impacted human life and health.


```r
harmful[1:10,]
```

```
## # A tibble: 10 x 5
##    EVTYPE            deaths injuries count frequency
##    <chr>              <dbl>    <dbl> <int>     <dbl>
##  1 TORNADO             5633    91346  7928     100  
##  2 EXCESS HEAT         3178     9241   945      30.6
##  3 FLOOD               1525     8604  1411      30.6
##  4 LIGHTNING            816     5230  3305      30.6
##  5 THUNDERSTORM WIND    753     9493  4026      46.8
##  6 RIP CURRENT          577      529   639      29  
##  7 EXTREME COLD         458      320   339      30.6
##  8 HIGH WIND            248     1137   525      30.6
##  9 AVALANCHE            224      170   239      29  
## 10 WINTER STORM         206     1321   227      29
```
It is very clear that **tornadoes are the most lethal weather event, with more than 5000 lives claimed, and over 90,000 injured**. Even more impressively, we see that they cause harm to populations **every single year for 62 years**. Hence it is clear that tornadoes should be a top priority for policies to keep populations prepared and safe.   
Another notable fact is that, despite the geographical position of the US in temperate climate latitudes (or maybe because of it) **heatwaves cause rather surprisingly large number of deaths**, and as frequently as extreme cold, or floods.  

In order to visualize these results, we calculate the cumulative death toll for each of our top 3 categories across the period.


```r
tornado <- stormdata %>% filter(EVTYPE=="TORNADO") %>%
        group_by(year)%>%summarise(tornado_deaths= sum(FATALITIES))%>%
        mutate(cum_tornado_toll = cumsum(tornado_deaths)) %>%
        select(year, cum_tornado_toll) %>% rename(tornado = cum_tornado_toll)
        
heat <- stormdata %>% filter(EVTYPE=="EXCESS HEAT") %>%
        group_by(year)%>%summarise(heat_deaths= sum(FATALITIES))%>%
        mutate(cum_heat_toll = cumsum(heat_deaths))%>%
        select(year, cum_heat_toll)%>% rename(heat=cum_heat_toll)

flood <- stormdata %>% filter(EVTYPE=="FLOOD") %>%
        group_by(year)%>%summarise(flood_deaths= sum(FATALITIES))%>%
        mutate(cum_flood_toll = cumsum(flood_deaths))%>%
        select(year, cum_flood_toll)%>% rename(flood=cum_flood_toll)
```



```r
cum_toll <- left_join(tornado, heat, by = "year")
cum_toll <- left_join(cum_toll, flood, by = "year")
cum_toll <- cum_toll %>% mutate(heat= ifelse(is.na(heat),0,heat))%>%
        mutate(flood = ifelse(is.na(flood),0,flood))%>%
        gather(key = "event" ,value = cum_death_toll, -year)

graph <- ggplot(data = cum_toll, aes(x=year, y=cum_death_toll,color= event))
graph <- graph + geom_line() + labs(x="Year", y="cumulative death toll")
graph <- graph + labs(title="Weather events threatening human life in the US")
graph <- graph + labs(subtitle="Cumulative death toll from 1951 to 2011")
print(graph)
```

<img src="Severe_Weather_files/figure-html/graph-1.png" style="display: block; margin: auto;" />


## Economic consequences
We carry out a similar analysis in order to determine the most damaging event types from an economic standpoint. 
First, however, we need to make variables "PROPDMG" and "CROPDMG" fit for comparisons and summation. The order of magnitude of these variables is encoded in separate variables "PROPDMGEXP" and "CROPDMGEXP" which indicate whether figures are in billions, or millions and so forth. Hence we create a numeric multiplier for each measure.   


```r
damages <- stormdata %>% 
        filter(PROPDMG > 0 | CROPDMG > 0) %>%
        mutate(PROPDMGEXP = str_to_upper(PROPDMGEXP),
               CROPDMGEXP = str_to_upper(CROPDMGEXP)) %>%
        mutate(prop_mult = ifelse(PROPDMGEXP=="B", 1e9, 1) ) %>%  # Billions
        mutate(prop_mult = ifelse(PROPDMGEXP=="M", 1e6, prop_mult) ) %>% # Millions
        mutate(prop_mult = ifelse(PROPDMGEXP=="K", 1e3, prop_mult) ) %>% # Thousands
        mutate(prop_mult = ifelse(PROPDMGEXP=="H", 100, prop_mult) ) %>%
        mutate(prop_mult = ifelse(grepl("[0-9]",PROPDMGEXP), 10, prop_mult) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="B", 1e9, 1) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="M", 1e6, crop_mult) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="K", 1e3, crop_mult) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="H", 100, crop_mult) ) %>%
        mutate(crop_mult = ifelse(grepl("[0-9]",CROPDMGEXP), 10, crop_mult) ) %>%
        mutate(prop_dmg= PROPDMG * prop_mult, crop_dmg= CROPDMG * crop_mult)
```

Here again we summarize by event type, we take the total economic impact by combining damages to crops and property into one number, across all years and across the whole country. Figures are expressed in millions USD.


```r
damages <- damages %>% 
        group_by(EVTYPE) %>%
        summarise(economic_damage= round((sum(prop_dmg) + sum(crop_dmg))/1e6) ,
        count = n()) %>%
        mutate(damage_per_occurrence = round(economic_damage/count)) %>%
        arrange(desc(economic_damage), desc(damage_per_occurrence))
```


```r
damages[1:10,]
```

```
## # A tibble: 10 x 4
##    EVTYPE            economic_damage  count damage_per_occurrence
##    <chr>                       <dbl>  <int>                 <dbl>
##  1 FLOOD                      179910  32037                     6
##  2 HURRICANE                   90271    213                   424
##  3 TORNADO                     57352  39361                     1
##  4 STORM SURGE                 43324    173                   250
##  5 HAIL                        18758  25969                     1
##  6 DROUGHT                     15019    266                    56
##  7 THUNDERSTORM WIND           12623 117700                     0
##  8 ICE STORM                    8967    667                    13
##  9 TROPICAL STORM               8382    407                    21
## 10 WINTER STORM                 6715   1389                     5
```
We see that, **in total over the years, floods have caused more economic damage (180 bn USD) than any other event**. But it is worth noting that **hurricane cause half as much damage (90 bn USD), in just over 200 occurrences** compared to 32000 cases of floods. It is another case where one type of event accumulates damages incrementally over time, while another causes devastation at a substantially higher rate at each occurrence.   
