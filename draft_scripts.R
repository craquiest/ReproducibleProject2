library(beepr) 
library(tidyverse)
library(lubridate)
library(stringr)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

 
destfile <- file.path(".", "StormData.csv.bz2")
download.file(url, method = "curl", destfile = destfile)

#dataset <- as_tibble(read.csv(destfile, stringsAsFactors = FALSE))
#stormy <- read.csv(destfile, stringsAsFactors = FALSE,nrows = 10000)
#colclass  <- sapply(stormy, class)

stormdata <- as_tibble(read.csv(destfile, stringsAsFactors = FALSE))
beep(2)

#storm_summary <- sapply(storm_data, summary)
evtype <- stormdata$EVTYPE
head(unique(evtype),20)
#evtype <- str_replace_all(evtype, "HEAT" = "EXCE")

years <- length(unique(year(mdy_hms(stormdata$BGN_DATE))))

stormdata <- stormdata %>% 
        mutate(year = year(mdy_hms(BGN_DATE))) %>%
        mutate(EVTYPE= str_to_upper(EVTYPE)) %>%
        mutate(EVTYPE= ifelse(grepl("HEAT",EVTYPE),"EXCESS HEAT",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("UNSEASONABLY WARM",EVTYPE),"EXCESS HEAT",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("FLOOD",EVTYPE),"FLOOD",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("RIP CURRENT",EVTYPE),"RIP CURRENT",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("THUNDERSTORM WIND",EVTYPE),"THUNDERSTORM WIND",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("TSTM WIND",EVTYPE),"THUNDERSTORM WIND",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("COLD",EVTYPE),"EXTREME COLD",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("LOW TEMPERATURE",EVTYPE),"EXTREME COLD",EVTYPE))%>%
        mutate(EVTYPE= ifelse(grepl("HURRICANE",EVTYPE),"HURRICANE",EVTYPE))

       
harmful <- stormdata %>% #filter(year > 2000) %>%
        filter(FATALITIES > 0 | INJURIES > 0) %>%
        group_by(EVTYPE) %>%
        summarise(deaths= sum(FATALITIES),injuries = sum(INJURIES),count = n(),
                  frequency = round(n_distinct(year) /years *100, 1))%>%
        arrange(desc(deaths), desc(injuries), desc(frequency))

# harm <- stormdata %>% 
#         filter(FATALITIES >0 | INJURIES >0) %>%
#         mutate(year = year(mdy_hms(BGN_DATE))) %>%
#         group_by(EVTYPE) %>%
#         summarise(deaths= sum(FATALITIES),injuries = sum(INJURIES),count = n(),
#                   frequency = round(n_distinct(year) /years *100, 1))%>%
#         arrange(desc(deaths), desc(injuries), desc(frequency))


damages <- stormdata %>% #filter(year > 2000) %>%
        filter(PROPDMG > 0 | CROPDMG > 0) %>%
        mutate(PROPDMGEXP = str_to_upper(PROPDMGEXP),CROPDMGEXP = str_to_upper(CROPDMGEXP)) %>%
        mutate(prop_mult = ifelse(PROPDMGEXP=="B", 1e9, 1) ) %>%
        mutate(prop_mult = ifelse(PROPDMGEXP=="M", 1e6, prop_mult) ) %>%
        mutate(prop_mult = ifelse(PROPDMGEXP=="K", 1e3, prop_mult) ) %>%
        mutate(prop_mult = ifelse(PROPDMGEXP=="H", 100, prop_mult) ) %>%
        mutate(prop_mult = ifelse(grepl("[0-9]",PROPDMGEXP), 10, prop_mult) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="B", 1e9, 1) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="M", 1e6, crop_mult) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="K", 1e3, crop_mult) ) %>%
        mutate(crop_mult = ifelse(CROPDMGEXP=="H", 100, crop_mult) ) %>%
        mutate(crop_mult = ifelse(grepl("[0-9]",CROPDMGEXP), 10, crop_mult) ) %>%
        mutate(prop_dmg= PROPDMG * prop_mult, crop_dmg= CROPDMG * crop_mult)

damages <- damages %>% 
        group_by(EVTYPE) %>%
        summarise(economic_damage= round((sum(prop_dmg) + sum(crop_dmg))/1e9,1) ,
        count = n(),frequency = round(n_distinct(year) /years *100, 1)) %>%
        arrange(desc(economic_damage), desc(count), desc(frequency))


