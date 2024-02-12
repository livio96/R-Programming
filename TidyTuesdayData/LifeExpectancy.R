library(dplyr)
library(tidyverse)
library(tidytuesdayR)


tuesdata <- tidytuesdayR::tt_load('2023-12-05')
complete_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_different_ages.csv")

albania_data_set <- filter(complete_data, Code=="ALB")

twenty_five_year_old_data <- albania_data_set %>% 
  select(Year, LifeExpectancy25)

chart <- ggplot(data = albania_data_set)+
  geom_line(aes(x=Year, y=LifeExpectancy0), color='Red')+
  geom_line(aes(x=Year, y=LifeExpectancy10), color="Blue")+
  geom_line(aes(x=Year, y=LifeExpectancy25), color="Pink")+
  geom_line(aes(x=Year, y=LifeExpectancy45), color="Yellow")+
  geom_line(aes(x=Year, y=LifeExpectancy65), color="Green")+
  geom_line(aes(x=Year, y=LifeExpectancy80), color="Purple")

chart + coord_cartesian(xlim = c(1960, 2025), ylim = c(0, 90))
plot(chart)
?ggplot

'sum_data <- ItemsOnHand %>% 
  group_by(MANUFACTURER) %>% 
  summarise(on_hand = sum(`ON HAND`, na.rm = TRUE))
view(sum_data)

ggplot(data=sum_data)+
  geom_line(aes(x = MANUFACTURER, y = on_hand))'
