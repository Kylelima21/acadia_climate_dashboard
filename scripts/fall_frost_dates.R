## Create first fall frost date analysis and figure


#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#
library(tidyverse)




#---------------------------------------------#
####     Figure - McFarland Hill Data      ####
#---------------------------------------------#

## Read in data export from https://ard-request.air-resource.com/data.aspx
## then clean it
dat <- read.csv("data/McFarland_Hill_export_20241022.csv") %>% 
  as_tibble() %>% 
  rename_with(tolower) %>% 
  mutate(across(o3_ppb:sol_w_m2, ~ifelse(. == -999, NA, .))) %>% 
  separate_wider_delim(., cols = date_time, delim = " ", names = c("date", "time")) %>% 
  separate_wider_delim(., cols = utc_date_time, delim = " ", names = c("date.utc", "time.utc")) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         tmp_degc = coalesce(tmp_degc, tmp_2_degc)) %>% 
  dplyr::select(-tmp_2_degc) %>% 
  mutate(plot.date = str_replace(date, "\\d\\d\\d\\d\\-", "2000-"),
         year = year(date))


## Get the dates of the first frost
frost <- dat %>% 
  group_by(date, year) %>% 
  summarise(min_tmp_c = min(tmp_degc), .groups = "drop") %>% 
  filter(month(date) > 7) %>% 
  group_by(year) %>% 
  filter(min_tmp_c <= 0) %>% 
  slice(1)


## Add in this years frost date
d24 <- data.frame(date = as.Date("2024-10-17"),
           year = 2024,
           min_tmp_c = 0)

frost2 <- rbind(frost, d24)


## Edit dataframe for plotting
plotdat <- frost2 %>% 
  mutate(first.frost = str_remove(date, "\\d\\d\\d\\d\\-")) %>% 
  dplyr::select(-date)

# frdat <- frost2 %>% 
#   mutate(date = str_replce(date, "\\d\\d\\d\\d\\-", "2000-"))


## Get 2024 so we can distinguish this year's point
p24 <- plotdat %>% 
  filter(year == 2024)


## Plot
plotdat %>% 
  ggplot(aes(x = year, y = first.frost)) +
  geom_point(color = "grey40") +
  geom_point(aes(x = year, y = first.frost), color = "#f22e28", data = p24) +
  geom_text(data = p24, label = "2024", vjust = -1, size = 10/.pt,
            fontface = "bold") +
  geom_smooth(aes(group = 1), fullrange = T,
              method = "lm", se = F, color = "black") +
  scale_x_continuous(limits = c(1998, 2025), breaks = c(2000, 2005, 2010, 2015, 2020)) +
  labs(x = "Year", 
       y = "Date of first fall frost") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 1),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0,15,0,0)),
        axis.title.x = element_text(margin = margin(15,0,0,0)))

## Save plot
ggsave("outputs/first_frost_figure.png", dpi = 700, height = 5, width = 6)



## Just a fun plot of mean temps
dat %>% 
  group_by(month(date), year) %>% 
  summarise(mean_tc = mean(tmp_degc, na.rm = T)) %>% 
  #filter(year >= 2019) %>%
  ggplot(aes(x = `month(date)`, y = mean_tc, group = -year)) + 
  geom_line(aes(color = year))
  



#---------------------------------------------#
####       Figure - NOAA Daily Data        ####
#---------------------------------------------#


read.csv("data/nClimGrid_daily_clean.csv") %>% 
  as_tibble()











