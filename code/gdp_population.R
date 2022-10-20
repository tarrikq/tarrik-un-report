library('tidyverse')
library('xlsx')
library('plotly')

gapminder_1997 <- read_csv('gapminder_1997.csv')

ggplot(data = gapminder_1997) +
  aes(x=gdpPercap, y= lifeExp, color=continent, size= pop/1000000) + 
  labs(x="GDP Per Capita",y="Life Expectancy",size="Population (in millions)", title="DO people in wealthy countries live longer")+
  geom_point()+
  scale_color_brewer(palette = "Set1")

gapminder_data <- read_csv('gapminder_data.csv')
  
ggplot(data = gapminder_data) +
  aes(x=year, y= lifeExp, color=continent, size= pop/1000000, group=country) + 
  labs(x="year",y="Life Expectancy",size="Population (in millions)", title="DO people in wealthy countries live longer")+
  geom_line()+
  scale_color_brewer(palette = "Set1")

p1 <-ggplot(data=gapminder_data, aes(x=continent, y= lifeExp)) +
  geom_boxplot()

ggplotly(p1)


##Multiple plots
p2 <-ggplot(data=gapminder_1997, aes(x=continent, y= lifeExp)) +
  geom_violin(color="lightslateblue", fill="Whitesmoke")+
  geom_jitter(aes(size=pop, alpha=0.7))
ggplotly(p2)



##Density plots
p3<-ggplot(data=gapminder_1997, aes(x=lifeExp))+
  geom_histogram(bins=15)
ggplotly(p3)

p4<-ggplot(data=gapminder_1997, aes(x=lifeExp))+
  geom_density(kernel="gaussian")
ggplotly(p4)


##ggplot2 themes

p5<-ggplot(data=gapminder_1997, aes(x=lifeExp))+
  geom_histogram()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
ggplotly(p5)

### Facets 

## facet wrap
p6 <- ggplot(data=gapminder_1997, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  facet_wrap(vars(continent))
ggplotly(p6)

##facet grid
p7 <- ggplot(data=gapminder_1997, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  facet_grid(rows =vars(continent))
ggplotly(p7)

##Saving plots

ggsave("awesome_plot.jpg", width = 6 , height = 4, plot = p2 )

##Animated plots

#install.packages(c('gganimate','gifski'))
library('gganimate')
library('gifski')

p8 <- ggplot(gapminder_data, aes(x=log(gdpPercap), y = lifeExp, size = pop, color=continent))+
  geom_point()
ggplotly(p8)

staticHansPlot <- ggplot(gapminder_data, aes(x=log(gdpPercap), y = lifeExp, size = pop/1000000, color=continent))+
  geom_point(alpha=0.5)+
  scale_color_brewer(palette = "Set1")+
  labs(x= "GDP Per Capita", y = "Life Expectancy", color= "continent", size = "Population (in millions)")+
  theme_classic()
ggplotly(staticHansPlot)

animatedHansPlot <- staticHansPlot +
  transition_states(year, transition_length = 1, state_length = 1)+
  ggtitle("{closest_state}")
animatedHansPlot

##Save out animated plot
anim_save("hansAnimatedPlot.gif", plot = animatedHansPlot, renderer=gifski_renderer())



