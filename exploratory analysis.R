library(ggplot2)
library(tidyverse)
attach(mydata)
## Explore hours per week vs occupation
# extract the means as table to use for hashed line in the plot below
mu <- mydata %>% 
  select(age,occupation) %>%
  group_by(occupation)%>%
  summarize( avg = mean(age)) %>% 
  view()

p <-ggplot(mydata, aes(x=hours.per.week, fill=occupation)) +
  ggtitle("Age vs Income density plot")+
  geom_density(alpha = 0.4)+ # alpha is set for transparency 
  geom_vline(data=mu, aes(xintercept=avg, color=occupation),
             linetype="dashed")
# change filling color
p+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette = "Set1") + theme_bw()

## Explore the Age
# extract the means as table to use for hashed line in the plot below
mu <- mydata %>% 
  select(age,income) %>%
  group_by(income)%>%
  summarize( avg = mean(age))

# create the density plot with hashed lines that describe the mean of each income class
# http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
p <-ggplot(mydata, aes(x=age, fill=income)) +
  ggtitle("Age vs Income density plot")+
  geom_density(alpha = 0.4)+ # alpha is set for transparency 
  geom_vline(data=mu, aes(xintercept=avg, color=income),
             linetype="dashed")
# change filling color
p+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette = "Set1") + theme_bw()
# -------------------------------------------------------------------------------------
## Explore Education
# extract the means as table to use for hashed line in the plot below
mu2 <- mydata %>% 
  select(educational.num,income) %>%
  group_by(income)%>%
  summarize( avg = mean(age))

# create the density plot
p <-ggplot(mydata, aes(x=educational.num, fill=income)) +
  geom_density(alpha = 0.4)+ # alpha is set for transparency
  ggtitle("education years vs income density plot")
# change filling color
p+scale_fill_brewer(palette="Set1") + theme_bw()
# create boxplots
p <-ggplot(mydata, aes(x=income,y = educational.num, fill=income)) + # x and y are explicitly declared to make the boxplot vertical
  geom_boxplot(alpha = 0.4)+ # geom_boxplot is used to create a boxplot instead of geom_density for density plot
  ggtitle("education years vs income density plot")
p+scale_fill_brewer(palette="Set1") + theme_bw()
# -------------------------------------------------------------------------------------
## Explore workclass
# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# create a table with counts of low and high income for each working class
mydata_work <- mydata %>%
  filter(workclass != 'volunteer') %>% 
  group_by(workclass,income) %>% 
  count() %>% 
  view
# create the barplot
mydata_work %>% 
  ggplot(aes(workclass,n,fill =income)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) + #use stat to supply x and y for the graph 
  ggtitle("workclass vs income barplot") + 
  scale_fill_brewer(palette = "Set1") + theme_bw()
# -------------------------------------------------------------------------------------
## Explore occupation
# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# create a table with counts of low and high income for each occupation
mydata_occ <- mydata %>%
  filter(workclass != 'volunteer') %>% 
  group_by(occupation,income) %>% 
  count() %>% 
  view
# create the barplot
mydata_occ %>% 
  ggplot(aes(occupation,n,fill =income)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) + #use stat to supply x and y for the graph 
  ggtitle("occupation vs income barplot") + 
  scale_fill_brewer(palette = "Set1") + theme_bw() + coord_flip() # coord_flip to see the occupation tabs clear
# -------------------------------------------------------------------------------------
## Explore race
# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# create a table with counts of low and high income for each race
mydata_race <- mydata %>%
  filter(workclass != 'volunteer') %>% 
  group_by(race,income) %>% 
  count() %>% 
  view
# create the barplot
mydata_race %>% 
  ggplot(aes(race,n,fill =income)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) + #use stat to supply x and y for the graph 
  ggtitle("race vs income barplot") + 
  scale_fill_brewer(palette = "Set1") + theme_bw() + coord_flip() # coord_flip to see the occupation tabs clear
# -------------------------------------------------------------------------------------
## Explore the income of male vs female
# https://r-graph-gallery.com/piechart-ggplot2.html
# create 2 tables with counts of low and high income for each gender
mydata_male <- mydata %>% 
  filter(gender == "Male") %>% 
  group_by(gender,income) %>% 
  count() %>% 
  view

mydata_female <- mydata %>% 
  filter(gender == "Female") %>% 
  group_by(gender,income) %>% 
  count()
# create a pie chart for each group
mydata_male%>%
  ggplot(aes(x = '',y = n,fill =income)) +
  geom_bar(stat = "identity", width = 2,alpha = 0.6)+
  coord_polar("y", start=0)+
  ggtitle("male high vs low income") + 
  scale_fill_brewer(palette = "Set1") + theme_bw() 

mydata_female%>%
  ggplot(aes(x = '',y = n,fill =income)) +
  geom_bar(stat = "identity", width = 2,alpha = 0.6)+
  coord_polar("y", start=0)+
  ggtitle("female high vs low income") + 
  scale_fill_brewer(palette = "Set1") + theme_bw() 
