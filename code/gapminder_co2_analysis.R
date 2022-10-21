
library('tidyverse')

##Call in full gapminder dataframe
gapminder_data <- read_csv("data\\gapminder_data.csv")

##summarizing the full dataset
summarise(gapminder_data, averageLifeExp=mean(lifeExp))

##Summarizing by using the chain command
gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))

##Settin the summary equal to a variable
gapminder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp=mean(lifeExp))

##filtering the main dataset using filter()
gapminder_data %>% 
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

##Find the earliest year in the data set and average the gdp column
gapminder_data %>% 
  filter(year == min(year)) %>%
  summarize(avgDP=mean(gdpPercap))


