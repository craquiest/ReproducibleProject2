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

cum_toll <- left_join(tornado, heat, by = "year")
cum_toll <- left_join(cum_toll, flood, by = "year")
cum_toll <- cum_toll %>% mutate(heat= ifelse(is.na(heat),0,heat))%>%
        mutate(flood = ifelse(is.na(flood),0,flood))%>%
        gather(key = "event" ,value = cum_death_toll, -year)

graph <- ggplot(data = cum_toll, aes(x=year, y=cum_death_toll,color= event))
graph <- graph + geom_line() + labs(x="Year", y="cumulative death toll")
graph <- graph + labs(title="Weather events threatening human life")
graph <- graph + labs(subtitle="US: 1951 ~ 2011")
print(graph)
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


