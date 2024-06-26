
# R for Data Science

James Lee 10/06/2021

Our aim is to understand the changes countries see over the course of 15
years (1962-2007). This relates to variables such as their GDP, average
life expectancy, Imports and Exports and energy use. Assessing
relationships between these variables allow us to probe for more
substantial information or trends in changing global environment.

`{r message=FALSE, warning=FALSE} #Relevant R libraries library(readr) library(tidyverse) library(ggplot2) library(dplyr) library(plotly) library(devtools) library(maditr) library(ggpubr) library(rstatix) #Read in gapminder_clean.csv gapminder_clean <- tibble(read_csv("gapminder_clean.csv"))`

## gapminder\_clean

A dataset containing several countries, recording general quantitative
data over the course of 15 years.

## CO2 emissions changing with GDP per capita

We invoke a correlation analysis to assess the relationship between a
global GDP per capita and CO2 emissions in 1962.

`` {r message=FALSE, warning=FALSE} df_1962 = gapminder_clean %>%    filter(Year == '1962') %>% # restrict the dataset to represent only values from 1962   filter(is.na(`CO2 emissions (metric tons per capita)`) == FALSE) %>%   filter(is.na(gdpPercap) == FALSE) %>%   summarise(cor = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$estimate,             pval = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$p.value   ) # summarise using cor.test to ascertain relationships and their significance #There is a positive correlation between GDP per capita and CO2 emissions #Pearson's = 0.93 #p = 1.13e-46 gapminder_clean %>%   filter(Year == '1962') %>%    filter(is.na(`CO2 emissions (metric tons per capita)`) == FALSE) %>%   filter(is.na(gdpPercap) == FALSE) %>%   ggplot(aes(y = `CO2 emissions (metric tons per capita)`, x = gdpPercap)) +    geom_point() + theme_bw() ``

Now we have looked at 1962 and see that there is a high positive
correlation between GDP per capita and CO2 emission rates. Although this
is just a snapshot of the change seen in other years. Importantly as
countries make future plans to reduce their carbon footprint or other
countries wish to further industrialise we may see that there are
specific changes correlation over time.

``` {r}
list_of_correlations = gapminder_clean %>%
  group_by(Year) %>%
  summarise(cor = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$estimate,
            pval = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$p.value
            ) %>%
  ungroup()
list_of_correlations
```

We can see that 1967 is the year with the highest correlation between
GDP per capita and CO2 emissions. This goes to show that going onwards
to 2007, countries on average have become less reliant on industry which
outputs high levels of CO2 emissions as main financial contribution.

Now let’s visualise this a bit better by making an interactive graph.

``` {r}
plot_1967 = gapminder_clean %>%
  filter(Year == '1967') %>%
  ggplot( aes(gdpPercap,`CO2 emissions (metric tons per capita)`, size = pop, color = continent, label = `Country Name` )) +
  geom_point() +
  theme_bw()
ggplotly(plot_1967)
```

## The relationship between energy use and the different continents

We want to look at the energy use of countries grouped in continents.
Let’s look first at whether there is a detectable difference in energy
use across these continents. Considering we are grouping via continents
it gives a generalisation of the countries within those groups.

``` {r}
cat('ANOVA')
model_ENERGY_CONTINENT_aov =  gapminder_clean %>%
  aov(`Energy use (kg of oil equivalent per capita)`~continent, data = .)
summary(model_ENERGY_CONTINENT_aov) 
#F = 21.88, p<0.05
shapiro.test(resid(aov(`Energy use (kg of oil equivalent per capita)`~continent, data = gapminder_clean)))
#but residuals are not normally distributed
kruskal.test(`Energy use (kg of oil equivalent per capita)`~continent, data = gapminder_clean)
#yes there is a relationship between continent and Energy use
#x^2 = 339.15, p<0.05
 
```

Our analysis of variance detects some significance however the analysis
gives us non-normally distributed residuals which requires us to test
this data non-parametrically. We lose a bit of statistical power but
carrying out a log transformation is not really applicable in this sense
as we would be re-framing the relationship as a log relationship.

Following a Kruskal-Wallis chi-squared test we can safely say that there
is some difference in Energy use depending between continents.

`` {r message=FALSE, warning=FALSE} gapminder_clean['continent'][is.na(gapminder_clean['continent'])] = 'Other' # change all NAs for continent to other # filter NAs and select Country, Energy use and continent as columns to obtain means of continents df_continent_points = gapminder_clean %>%   filter(is.na(`Energy use (kg of oil equivalent per capita)`) == FALSE) %>%   select(`Country Name`,`Energy use (kg of oil equivalent per capita)`, continent) %>%   group_by(`Country Name`, continent) %>%   summarize(value = mean(`Energy use (kg of oil equivalent per capita)`)) %>%   ungroup() # filter NAs and select Country, Energy use and continent as columns to obtain individual points for countries df_continent_countries = gapminder_clean %>%   filter(is.na(`Energy use (kg of oil equivalent per capita)`) == FALSE) %>%   select(`Country Name`,`Energy use (kg of oil equivalent per capita)`,continent) %>%   group_by(continent) %>%   summarize(mean = mean(`Energy use (kg of oil equivalent per capita)`),             sd = sd(`Energy use (kg of oil equivalent per capita)`),             se = sd(`Energy use (kg of oil equivalent per capita)`)/sqrt(length(continent))) %>%   ungroup() #our plot plot_ENERGY_CONTINENT = df_continent_countries %>%     ggplot() + geom_bar(aes(y = mean, x = continent, fill = continent), stat = 'identity') +     geom_jitter(data = df_continent_points, aes(x = continent, y = value, fill = continent, label = `Country Name`)) +      scale_y_continuous(limits = c(0,21000), expand = c(0,0)) +      labs(y = 'Energy use (kg of oil equivalent per capita)') +      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),           panel.background = element_blank(), axis.line = element_line(colour = "black")) ggplotly(plot_ENERGY_CONTINENT) ``

## Looking at differences in imports between Europe and Asia

Furthering this comparison between continents we can look closely at the
difference between two continents. Let’s take Europe and Asia and look
at the difference in Imports by identifying the change in Imports of
goods and services as a percentage of total GDP in the years after 1990.

``` {r}
#Clean data
ASIA = gapminder_clean %>%
  filter(Year > 1990) %>%
  filter(continent == 'Asia') %>%
  filter(is.na(`Imports of goods and services (% of GDP)`) == FALSE) %>%
  select(`Country Name`,`Imports of goods and services (% of GDP)`, continent, Year)
EUROPE = gapminder_clean %>%
  filter(Year > 1990) %>%
  filter(continent == 'Europe') %>%
  filter(is.na(`Imports of goods and services (% of GDP)`) == FALSE) %>%
  select(`Country Name`,`Imports of goods and services (% of GDP)`, continent, Year)
#Individually test for normality using KS-test (not shapiro because we expect there to be some skew)
ks.test(ASIA$`Imports of goods and services (% of GDP)`, "pnorm")
ks.test(EUROPE$`Imports of goods and services (% of GDP)`, "pnorm")
#non-normal distribution of Imports therefore use non-parametric or use log
EUROPE_ASIA_AOV = rbind(ASIA,EUROPE) # bind dataset
# wilcox test to test difference between continents as data is not normally distributed
EUROPE_ASIA_AOV %>%
  wilcox_test(`Imports of goods and services (% of GDP)`~continent) %>%
  add_significance()
```

Now we can plot something…

`` {r warning=FALSE} # plot the resultant difference visually ggplotly(EUROPE_ASIA_AOV %>%    ggplot(data = ., aes(x = continent, y = `Imports of goods and services (% of GDP)`, colour = Year)) +   geom_bar(stat = 'summary', fun = 'mean') + geom_jitter(data = EUROPE_ASIA_AOV, aes(label = `Country Name`)) +   stat_compare_means(method = "wilcox", label.x = 1.5, label.y = 200) +    scale_y_continuous(expand = c(0,0), limits = c(0,250)) +   theme_bw()) ``

## Countries with high population densities

To assess countries with the highest population densities we will need
to make a comparison between the top countries in terms of population
density.

``` {r}
N = c() # empty lists for filling
HIGHEST = c() 
#for loop to slice top ten countries in terms of population density for each time point
for (i in levels(as.factor(gapminder_clean$Year))) {
  X = gapminder_clean %>%
    select(Year,`Country Name` ,`Population density (people per sq. km of land area)`) %>%
    filter(Year == i) %>%
    na.omit() %>%
    arrange(desc(`Population density (people per sq. km of land area)`)) %>%
    slice(1:10)
  HIGHEST = rbind(HIGHEST,X[1,2])
  N = rbind(N,X)
}
N
```

So this tibble displays the top ten countries in terms of population
density per time point. The tibble is ordered within each year by
highest population density to lowest. If we want to display this
visually:

``` {r}
N$Max = c(rep(HIGHEST$`Country Name`, each = 10))
PLOT_HIGHEST_POP = N %>%
  ggplot(data = ., aes(x = Year, y = `Population density (people per sq. km of land area)`, fill = `Country Name`, label = Max)) +
  geom_bar(stat = 'identity') + 
  scale_y_continuous(expand = c(0,0), limits = c(0,60000)) + 
  theme_bw()
ggplotly(PLOT_HIGHEST_POP)
```

This plot shows the Population density of several countries. Each
country (represented by the different colours) have been within the top
ten highest countries within at least one of the time points.

\#\#Life expectancy

Finally let us look at life expectancy and how that changes. More
importantly let us look at the countries which experienced the highest
increase in life expectancy

``` {r}
#clean data so we only obtain values from 2007 and 1962
df_1962_and_2007 = gapminder_clean %>%
  filter(Year %in% c('2007', '1962')) %>%
  select(`Country Name`, Year, `Life expectancy at birth, total (years)`) %>%
  na.omit() %>%
  spread(Year,`Life expectancy at birth, total (years)`)
#obtain differences from both years and arrange the differences in descending order
df_1962_and_2007 = df_1962_and_2007 %>%
  mutate(`Change in life expectancy at birth, total (years)` = df_1962_and_2007$`2007` - df_1962_and_2007$`1962`) %>%
  arrange(desc(`Change in life expectancy at birth, total (years)`)) %>%
  select(`Country Name`, `Change in life expectancy at birth, total (years)`)
df_1962_and_2007
```

We find that Maldives is the country with the greatest difference in
Life expectancy. Let’s plot it…

``` {r}
#Only want to plot top ten countries
df_LIFE_EXPECTANCY_TOP_10 = df_1962_and_2007 %>%
  slice(1:10)
#ggplot
LIFE_EXPECTANCY_PLOT = ggplot(data = df_LIFE_EXPECTANCY_TOP_10, aes(x = `Country Name` , y = `Change in life expectancy at birth, total (years)`)) + 
  geom_bar(stat = 'identity') + rotate_x_text(angle = 90) + 
  ylab("Change in Life expectancy between years 1962 to 2007") + 
  xlab("Top 10 countries") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,50))
ggplotly(LIFE_EXPECTANCY_PLOT)
```

But this gives little context to the overall change over time. Maybe a
line graph would be more representative…

``` {r}
df_all_years_TOP_10 = c()
for (i in df_LIFE_EXPECTANCY_TOP_10$`Country Name`) {
  X = gapminder_clean %>%
    filter(`Country Name` == i) %>%
    select(`Country Name`,Year, `Life expectancy at birth, total (years)`)
  df_all_years_TOP_10 = rbind(df_all_years_TOP_10,X)
}
line_difference = ggplot(data = df_all_years_TOP_10, aes(x = Year, y = `Life expectancy at birth, total (years)`, color = `Country Name`)) + 
  geom_line(stat = 'identity') + geom_point()
ggplotly(line_difference)
```

That’s better. We can now see that in terms of timepoint differences
Timor-Leste would have been greatest if we started counting in 1977.
Overall we can see the sequential change in life expectancy over the
years.
