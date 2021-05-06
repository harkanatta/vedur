
rass <- read.csv('skjol/Skagastr&ouml;nd_01012020-31122020.txt',header = T,sep = "\t")


library(echarts4r)
library(magrittr)
library(plyr)
library(purrr)

# file <- "C:/Users/valty/Documents/vinna/github/vedur/skjol"
# setwd(file)
files <- list.files("skjol/",pattern="*.txt",full.names = T)
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

library(lubridate)

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





############# Ekki gleyma Sys.setlocale("LC_ALL", "Icelandic")
library(tidyverse)
sysfonts::font_add_google("Montserrat", "Montserrat")
sysfonts::font_add_google("Sacramento", "Sacramento")
sysfonts::font_add_google("Catamaran", "Catamaran")
showtext::showtext_auto()


files <- list.files("skjol/",pattern="*.txt",full.names = T)
vedur <- files %>% map_dfr(read_delim, delim = "\t", locale = locale(decimal_mark = ","), trim_ws = TRUE, col_types = cols(`Timabil` = col_datetime(format = "%H:%M %d.%m.%Y")))


df <- vedur %>%   mutate(manudur = format(Timabil, "%m"),
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
#  ggsave("CostadelSkags.png", p, height=5, width=5, dpi=150)
#  ggsave("CostadelSkags.pdf", p, device = "pdf", height=5, width=5, dpi=150)



for (i in c("01", "12", "11", "10", "09", "08", "07", "06", "05", "04", "03", "02")) {
  

 df <- vedur %>%  mutate(hour = format(Timabil, "%H"),
                 manudur = format(Timabil, "%m"),
                 manudurstor = format(Timabil, "%B")) %>% 
    filter(manudur == i)

 png(filename=paste(paste(i,"tidni",sep = '-'),'png',sep = '.'),12,7,"cm",pointsize=16,res=200, family = "Sacramento")
  windContours(hour = df$hour,
               wd = df$`Vindatt (deg)`,
               ws = df$`Vindur (m/s)`,
               keytitle = paste(paste("Tíðni vinda á Skagaströnd í",unique(df$manudurstor),sep = " "),'[%] 2011-2020', sep = ' ' ))
dev.off()
}



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



#########################################################################
Sys.setlocale("LC_ALL", "Icelandic")
library(tidyverse)
library(purrr)
library(ggplot2)

# file <- "C:/Users/valty/Documents/vinna/github/vedur/skjol"
# setwd(file)
files <- list.files("skjol/",pattern="*.txt",full.names = T)
vedur <- files %>% purrr::map_dfr(read_delim, delim = "\t", locale = locale(decimal_mark = ","), trim_ws = TRUE, col_types = cols(`Timabil` = col_datetime(format = "%H:%M %d.%m.%Y")))


df <- vedur %>%
  mutate(manudur = format(Timabil, "%m"),
         dagur = format(Timabil, "%d"),
         man = factor(format(Timabil, "%b"), 
                      levels=format(ISOdate(2000, 1:12, 1), "%b"), ordered=TRUE)) %>%
  #group_by(man,dagur) %>% 
  group_by(man) %>% 
  dplyr::summarise(Vindur = mean(`Vindur (m/s)`),
                   var = var(`Vindur (m/s)`),
                   sd = sd(`Vindur (m/s)`),
                   stderror = plotrix::std.error(`Vindur (m/s)`))

p1 <- ggplot(df,aes(man,Vindur)) +
  geom_point(size = 7) +
  #geom_pointrange(aes(ymin=Vindur-sd, ymax=Vindur+sd)) +
  geom_errorbar(aes(ymin=Vindur-sd, ymax=Vindur+sd), width=.2,
                position=position_dodge(0.05)) +
  ggtitle("Standard deviation") +
  ylab("Vindur (m/s)") +
  xlab("") +
  theme_minimal()

p2 <- ggplot(df,aes(man,Vindur)) +
  geom_point(size = 7) +
  #geom_pointrange(aes(ymin=Vindur-sd, ymax=Vindur+sd)) +
  geom_errorbar(aes(ymin=Vindur-var, ymax=Vindur+var), width=.2,
                position=position_dodge(0.05)) +
  ggtitle("Variation") +
  ylab("Vindur (m/s)") +
  xlab("") +
  theme_minimal()
#

library(patchwork)

p1p2 <- p1 + p2
ggsave("devOGvar.png", p1p2, device = "png", height=7, width=12, dpi=60)


df <- vedur %>%
  mutate(ar = format(Timabil, "%Y"),
         manudur = format(Timabil, "%m"),
         dagur = format(Timabil, "%I"),
         man = factor(format(Timabil, "%b"), 
                      levels=format(ISOdate(2000, 1:12, 1), "%b"), ordered=TRUE)) %>%
  filter(ar!=2021) %>% 
  #group_by(ar, man, dagur) %>% 
  group_by(man) %>% 
  dplyr::summarise(Vindur = mean(`Vindur (m/s)`))

library(ggbeeswarm)
litir <- harrypotter::hp(11,house = "hufflepuff")
library("ggsci")

#p3 <- ggplot(df,aes(man,Vindur, color=ar)) +
p3 <- ggplot(df,aes(man,Vindur)) +
  #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_violin(position = position_dodge(width = 0.9)) +
  #geom_quasirandom(dodge.width = 0.9, varwidth = TRUE) +
  geom_beeswarm(alpha=.7,size=7,priority='none') +
  #harrypotter::scale_colour_hp_d(option = "Ravenclaw", name = "Ár") +
  scale_color_d3() +
  ylab("Vindur (m/s)") +
  xlab("") +
  theme_minimal()
p3
ggsave("vindurManKlstAr.png", p3, device = "png", height=7, width=12, dpi=100)





dir.create("examples")
setwd("examples")
# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()
# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
C:/Program Files/ImageMagick-7.0.11-Q16-HDRI
system('"C:\\Program Files (x86)\\LyX 2.0\\imagemagick\\convert.exe" -delay 20 -loop 0 files_*.png animation.gif')
system("convert -delay 80 *.png example_1.gif")
# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))












# 52 vikur:


df <- vedur %>%  mutate(hour = format(Timabil, "%H"),
                        manudur = format(Timabil, "%W"),
                        manudurstor = format(Timabil, "%B"))

A <- table(df$manudur)
B <- (attributes(A)[2])

for (i in B$dimnames[[1]]) {
  
  df <- vedur %>%  mutate(hour = format(Timabil, "%H"),
                          manudur = format(Timabil, "%W"),
                          manudurstor = format(Timabil, "%B")) %>% 
  filter(manudur == i)
  
  png(filename=paste(paste(i,"tidni",sep = '-'),'png',sep = '.'),12,7,"cm",pointsize=16,res=200, family = "Sacramento")
  windContours(hour = df$hour,
               wd = df$`Vindatt (deg)`,
               ws = df$`Vindur (m/s)`,
               keytitle = paste(paste("Tíðni vinda á Skagaströnd í",unique(df$manudurstor),sep = " "),'[%] 2011-2020', sep = ' ' ))
  dev.off()
}








DF <- df[sample(dim(df)[1],100),]
DF <- DF %>%  mutate(hour = format(Timabil, "%H"),
                  manudur = format(Timabil, "%W"),
                  manudurstor = format(Timabil, "%B"))
