#Density plot
Sys.setlocale("LC_ALL", "Icelandic")
library(tidyverse)
library(purrr)
library(ggplot2)

files <- list.files("skjol/",pattern="*.txt",full.names = T)
vedur <- files %>% purrr::map_dfr(read_delim, delim = "\t", locale = locale(decimal_mark = ","), trim_ws = TRUE, col_types = cols(`Timabil` = col_datetime(format = "%H:%M %d.%m.%Y")))


df <- vedur %>%
  mutate(ar = format(Timabil, "%Y"),
         dagur = format(Timabil, "%d"),
         man = factor(format(Timabil, "%b"), 
                      levels=format(ISOdate(2000, 1:12, 1), "%b"), ordered=TRUE),
         att = round(`Vindatt (deg)`* 8 / 360),
         attir = case_when(
           att == '0' ~ "N",
           att == '1' ~ "NA",
           att == '2' ~ "A",
           att == '3' ~ "SA",
           att == '4' ~ "S",
           att == '5' ~ "SV",
           att == '6' ~ "V",
           att == '7' ~ "NV",
           TRUE ~ "N"
         )
  )

p <- df %>%
  mutate(attir = fct_relevel(attir, 
                             "N", "NA", "A", 
                             "SA", "S", "SV", 
                             "V", "NV")) %>%
  ggplot(aes(x=man, after_stat(count), group=attir, fill=attir)) +
  geom_density(adjust=1.5, position="fill")

#með bakgrunnsmynd
img <- jpeg::readJPEG("fjall_Vicki.jpg")
p2 <- df %>%
  mutate(attir = fct_relevel(attir, 
                             "N", "NA", "A", 
                             "SA", "S", "SV", 
                             "V", "NV")) %>%
  ggplot(aes(x=man, group=attir)) +
  labs(y = "") +
  ggpubr::background_image(img)+
  geom_density(adjust=1.5, position="fill")+
  theme_minimal()

#ggsave("densityPlot.png", p, height=5, width=7, dpi=150)
#ggsave("bakgrunnur_Vicki.png", p2, height=5, width=7, dpi=150)
