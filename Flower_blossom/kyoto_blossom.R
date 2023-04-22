# params ------------------------------------------------------------------
setwd("/Users/Francisco/Documents/GitHub/Visualisations/Visualisations/Flower_blossom/")
url_kyoto <- "http://atmenv.envi.osakafu-u.ac.jp/aono/kyophenotemp4/"
file_kyoto <- "kyoto_blossom_dates.csv"   # cache data
icon_sakura <- "sakura.png" # cache icon


# config ------------------------------------------------------------------

library(tidyverse)
library(fs)
library(janitor)
library(httr)
library(rvest)
library(glue)
library(ggimage)
library(lubridate)


# data --------------------------------------------------------------------

# icon
if (!file_exists(icon_sakura)) {
  GET("https://www.flaticon.com/download/icon/7096433?icon_id=7096433&author=232&team=232&keyword=Sakura&pack=packs%2Fsakura-festival-59&style=522&format=png&color=&colored=2&size=512&selection=1&premium=0&type=standard&search=cherry+blossom",
      write_disk(icon_sakura))
}


# plot --------------------------------------------------------------------

# Read the CSV file
data <- read_csv(file_kyoto)

# Remove empty rows, transform dates, and calculate the day of the year
data <- data %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(full_flowering_date = ymd(glue("{str_pad(ad, 4, pad = '0')}{str_pad(fufd, 4, pad = '0')}"))) %>%
  mutate(full_flowering_date_doy = yday(full_flowering_date)) %>% 
  mutate(random_size = sample(c(0.015, 0.02, 0.025, 0.03), length(ad), replace = TRUE))


  ggplot(data = data,aes(ad, parse_date_time(full_flowering_date_doy, orders = "j"))) +
  geom_smooth(color = NA, fill = "chocolate4", alpha = 0.5)+
  geom_image(aes(size = I(random_size)), image = icon_sakura) +
  geom_smooth(color = "chocolate3", se = FALSE, alpha = 0.5)+
  scale_y_datetime(labels = scales::date_format("%b %d"),
                   breaks = "weeks", minor_breaks = "days") +
  labs(title = "Cherry blossom",
       subtitle = "Kyoto",
       x = "year",
       y = "date",
       caption = glue("http://r.iresmi.net/ {Sys.Date()}
                      data: {url_kyoto}
                      icon by Vitaly Gorbachev")) +
  theme_minimal() 

ggsave("cherry_blossom.png", width = 25, height = 18, units = "cm", dpi = 300)