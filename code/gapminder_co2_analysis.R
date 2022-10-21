
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


##Grouping data based on an identifier using groupby()
gapminder_data %>%
  group_by(year) %>%
  summarize(average = mean(lifeExp))

##can also pass multiple summarize functions
gapminder_data %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp), min = min(lifeExp))

## mutate() function to add new columns
gapminder_data %>%
  mutate(gdp = gdpPercap*pop, 
         popInMillions = pop/1000000)

## subset columns or change their order using select()
gapminder_data %>%
  select(pop, year)

##Reorder the columns
gapminder_data %>%
  select(continent, country, year, pop, gdpPercap, lifeExp)
  

##converting between long and wide data using pivot_wider() and pivot_longer()
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

##final dataset for analysis

gapminder_data_2007 <- gapminder_data %>%
                          filter(year == 2007 & continent == "Americas") %>%
                          select(-year, -continent)


##Data cleaning
##import data and skip the first line to fix the headers
read_csv("data//co2-un-data.csv", skip=1)

##can read in skip all headers and make our own
read_csv("data//co2-un-data.csv", skip=2, 
         col_names=c("Region","Country","Year","Series","Value",'Footnotes',"Source"))

##rename a single column header
read_csv("data//co2-un-data.csv", skip=1) %>%
  rename(country = ...2)

## rename all columns
read_csv("data//co2-un-data.csv", skip=1) %>%
  rename_all(tolower)

# practicing select
co2_emissions_dirty <- read_csv("data//co2-un-data.csv", skip=2, 
                          col_names=c("region","country","year","series","value",'footnotes',"source"))

##Determine which year to convert to wide format with by checking how many observations happen each year
##Also fix the names in the series and the country columns
co2_emissions <- co2_emissions_dirty %>%
                          select(country, year, series, value) %>%
                          mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                                                 "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
                          mutate(country = recode(country, "Bolivia (Plurin. State of)" = "Bolivia",
                                                 "Venezuela (Boliv. Rep. of)"="Venezuela",
                                                 "United States of America"="United States")) %>%
                          pivot_wider(names_from = series, values_from = value) %>%
                          filter(year == 2005) %>%
                          select(-year)

##Fix the country names in the gapfinder dataframe
gapminder_data_2007 <- gapminder_data_2007 %>%
                        mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
                        group_by(country) %>%
                        summarize(lifeExp = sum(lifeExp*pop)/sum(pop),
                                  gdpPercap = sum(gdpPercap*pop)/sum(pop),
                                  pop=sum(pop))


##Checking to see if there are multiple variable with the same name
anti_join(gapminder_data_2007, co2_emissions, by="country")

##Joining dataframes
gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions)

gapminder_co2 <- gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country=="Mexico", "north", "South"))


