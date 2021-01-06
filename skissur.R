
rass <- read.csv('Skagastr&ouml;nd_01012020-31122020.txt',header = T,sep = "\t")


library(echarts4r)
library(magrittr)
library(plyr)
library(purrr)

file <- "C:/Users/valty/Documents/vinna/github/vedur/skjol"
setwd(file)
files <- list.files(pattern="*.txt")
vedur <- files %>% map_dfr(read.csv2, sep="\t") 

vedur$dags <- strptime(vedur$Timabil, "%H:%M %d.%m.%Y")
#vedur$dags2 <- strptime(vedur$Timabil, "%d/%m/%Y %H:%M")
vedur$dagsw <- ifelse(is.na(as.Date(as.character(vedur$Timabil),"%H:%M %d.%m.%Y")),as.character.Date(vedur$dags2),as.character.Date(vedur$dags))
vedur$dags <- as.POSIXct(vedur$dagsw,tz="GMT")
vedur <- vedur[order(vedur$dags,decreasing=F),]
vedur$dags2 <- as.Date(vedur$dags,format="%d.%m.%Y")
vedur$year <- format(vedur$dags,"%Y")
vedur <- vedur[!vedur$year==2021,]

g <- vedur$year
l <- split(vedur, g)
l <- lapply(l, function(x) mean(x$Lofthiti..degC.))



vedur %>% ddply(.(dags2),summarize,'°C'=round(mean(Lofthiti..degC.),1)) %>% 
  dplyr::mutate(year = format(dags2, "%Y")) %>% # get year from date
  group_by(year) %>%
  e_charts(dags2) %>% 
  e_calendar(range = "2011",top="50", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_calendar(range = "2012",top="140", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2013",top="230", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2014",top="320", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2015",top="410", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2016",top="50", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_calendar(range = "2017",top="140", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2018",top="230", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2019",top="320", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2020",top="410", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_heatmap(`°C`, coord_system = "calendar") %>% 
  e_visual_map(max = 16, min = -11) %>% 
  e_title("Hitastig", "Skagaströnd") %>% 
  e_tooltip("item")





vedur %>% ddply(.(dags2),summarize,'°C'=round(max(Hvida..m.s.),1)) %>% 
  dplyr::mutate(year = format(dags2, "%Y")) %>% # get year from date
  group_by(year) %>%
  e_charts(dags2) %>% 
  e_calendar(range = "2011",top="50", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_calendar(range = "2012",top="140", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2013",top="230", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2014",top="320", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2015",top="410", cellSize="10",
             left = "5%", 
             width = "40%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2016",top="50", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_calendar(range = "2017",top="140", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2018",top="230", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2019",top="320", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2020",top="410", cellSize="10",
             right = "5%", 
             width = "40%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('lau', 'sun', 'mán', 'þri', 'mið', 'fim', 'fös')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_heatmap(`°C`, coord_system = "calendar") %>% 
  e_visual_map(max = 50, min = 0) %>% 
  e_title("Hitastig", "Skagaströnd") %>% 
  e_tooltip("item")



