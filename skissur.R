library(echarts4r)
library(magrittr)
library(lubridate)
dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
values <- rnorm(length(dates), 20, 6)
year <- data.frame(date = dates, values = values)
year %>% 
  e_charts(date) %>% 
  e_calendar(range = "2018",
             top="60", 
             left = 150, 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_heatmap(values, coord_system = "calendar") %>% 
  e_visual_map(max = 30) %>% 
  e_title("Calendar", "Heatmap")





library(plyr)

rass <- vedur %>% 
  select(date="dags2",values="Lofthiti..degC.","dagur") %>% 
  ddply(rass,"dagur",summarize,values=mean("values"))

vedur %>% 
  select(date="dags2",values="Lofthiti..degC.","dagur") %>%
  ddply("dagur",summarize,date=mean("values")) %>% 
  e_charts(date) %>% 
  e_calendar(range = "2018",
             top="60", 
             left = 150, 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_heatmap(values, coord_system = "calendar") %>% 
  e_visual_map(max = 30) %>% 
  e_title("Calendar", "Heatmap")






