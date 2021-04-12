
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
  e_calendar(range = "2011", orient="vertical", cellSize="10",top="140",
             left = "3%", 
             width = "7%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_calendar(range = "2012", orient="vertical", cellSize="10",top="140",
             left = "13%", 
             width =  "7%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2013", orient="vertical", cellSize="10",top="140",
             left = "23%", 
             width =  "7%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2014", orient="vertical", cellSize="10",top="140",
             left = "33%", 
             width =  "7%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2015", orient="vertical", cellSize="10", top="140",
             left = "43%", 
             width =  "7%",
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2016", orient="vertical", cellSize="10",top="140",
             left = "53%", 
             width =  "7%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>% 
  e_calendar(range = "2017", orient="vertical", cellSize="10",top="140",
             left = "63%", 
             width =  "7%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2018", orient="vertical", cellSize="10",top="140",
             left = "73%", 
             width =  "7%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2019", orient="vertical", cellSize="10",top="140",
             left = "83%", 
             width =  "7%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
             monthLabel = list(
               nameMap = month(1:12, label = TRUE, abbr = T))) %>%
  e_calendar(range = "2020",cellSize="10", orient="vertical",top="140", symbolSize="1",
             left = "93%", 
             width =  "7%", 
             dayLabel = list(
               firstDay=2, 
               nameMap = c('L', 'S', 'M', 'Þ', 'Mi', 'F', 'Fö')),
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





#############
sysfonts::font_add_google("Montserrat", "Montserrat")
sysfonts::font_add_google("Sacramento", "Sacramento")
sysfonts::font_add_google("Catamaran", "Catamaran")
showtext::showtext_auto()


rass <- list()
A <- list.files("C:/Users/BioPol VS/Documents/Vinnumappa/Vedur/github/vedur/skjol", full.names = T)
for (i in 1:length(A)) {
  rass[[i]] <- readr::read_delim(A[i], delim = "\t", locale = locale(decimal_mark = ","), trim_ws = TRUE, col_types = cols(
    `Timabil` = col_datetime(format = "%H:%M %d.%m.%Y"))) 
  print(str(rass[i]))
}

DF <- do.call(rbind,rass)


df <- DF %>%   mutate(manudur = format(Timabil, "%m"),
                      man = format(Timabil, "%b")) %>%
  group_by(manudur)
df$manudur = factor(format(strptime(df$Timabil,'%Y-%m-%d'),'%b'), levels=format(ISOdate(2000, 1:12, 1), "%b"), ordered=TRUE)

p.wr2 <- plot.windrose(data = df,
                       spd = "Vindur (m/s)",
                       dir = "Vindatt (deg)")
p <- p.wr2 + facet_wrap(~manudur, nrow = 3) +
  patchwork::plot_annotation(
    title = "Costa del Skagaströnd",
    subtitle = "Tíðni vindátta á Skagaströnd (2011-2020)",
    caption = "Gögn: www.mogt.is",
    theme = theme(plot.title = element_text(size = 30,
                                            hjust=0.5,
                                            vjust = -0.1,
                                            family="Sacramento"),
                  plot.subtitle = element_text(size = 15,
                                               hjust=0.5,
                                               vjust= -0.1,
                                               family="Montserrat"),
                  plot.caption = element_text(size = 12,
                                              family="Catamaran"),
                  plot.background = element_rect(fill = NA, colour = 'black', size = 3))
  )
####
  ggsave("CostadelSkags.png", p, height=5, width=5, dpi=150)
  ggsave("CostadelSkags.pdf", p, device = "pdf", height=5, width=5, dpi=150)



  
 df <- DF %>%  mutate(hour = format(Timabil, "%H"),
                 manudur = format(Timabil, "%b"),
                 manudurstor = format(Timabil, "%B")) %>% 
    filter(manudur == 'júl.')
  
 png(filename=paste(paste("Tidni",unique(df$manudurstor),sep = '-'),'png',sep = '.'),12,7,"cm",pointsize=16,res=200, family = "Sacramento")
  windContours(hour = df$hour,
               wd = df$`Vindatt (deg)`,
               ws = df$`Vindur (m/s)`,
               keytitle = paste(paste("Tíðni vinda á tíma dags í",unique(df$manudurstor),sep = " "),'[%] 2011-2020', sep = ' ' ))
dev.off()



pal <- hp(n = 128, house = "Hufflepuff")
image(tab.wd_smooth, col = pal)




library(echarts4r)
dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
values <- rnorm(length(dates), 20, 6)

year <- data.frame(date = dates, values = values)

year %>% 
  dplyr::mutate(year = format(date, "%Y")) %>% # get year from date
  group_by(year) %>% 
  e_charts(date) %>% 
  e_calendar(range = c("2017","2018"),orient="vertical",left="40%") %>% 
  e_scatter(values, coord_system = "calendar") %>% 
  e_visual_map(max = 30) %>% 
  e_title("Calendar", "Heatmap")%>%
  e_tooltip("item")  





