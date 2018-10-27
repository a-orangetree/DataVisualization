library(tidyverse)
library(ggthemes)

videogame_sales_data <- read_csv('vgsales.csv') 

videogame_sales_data %>% 
  mutate(Global_Sales_Test = NA_Sales + EU_Sales + JP_Sales + Other_Sales,
         isSame = near(Global_Sales_Test, Global_Sales, .05)) %>% 
  select(isSame) %>% 
  unique()

videogame_sales_data %>% 
  mutate(Global_Sales_Test = NA_Sales + EU_Sales + JP_Sales + Other_Sales,
         isSame = abs(Global_Sales_Test - Global_Sales) < .5) %>% 
  select(isSame) %>% 
  unique()

unique(filter(videogame_sales_data2, Year == 2016)$Platform)

glimpse(videogame_sales_data)

unique(video$isSame)




videogame_sales_data2 <- videogame_sales_data %>% 
  gather(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales, key = 'Region', value = 'Sales')


ggplot(videogame_sales_data, aes(Year), fill = 'black') +
  geom_bar() +
  theme_hc() +
  labs(y = 'Number of Observations')

ggplot(videogame_sales_data2,aes(Region, Sales)) +
  geom_boxplot() +
  theme_tufte()


Nintendo_Activision_data <- filter(videogame_sales_data2, Publisher %in% c('Nintendo', 'Activision'),
                                   Region != 'Global_Sales')

ggplot(Nintendo_Activision_data, aes(Region, Sales, fill = Publisher)) +
  geom_bar(stat = 'identity') +
  theme_grey() +
  labs(title = 'Total Sales by Region') +
  theme(legend.position = 'bottom')


year2016_data <- (filter(videogame_sales_data, Year == 2016))
                  
unique(year2016_data$Publisher)


ggplot(year2016_data, aes(NA_Sales, EU_Sales, label = Name, color = Platform)) +
  geom_text(check_overlap = TRUE, vjust = 'inward', hjust = 'inward') +
  facet_wrap(~ Genre) +
  scale_y_log10() +
  scale_x_log10() +
  theme_light()
