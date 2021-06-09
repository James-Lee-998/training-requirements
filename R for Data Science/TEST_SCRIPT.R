library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(devtools)
library(maditr)
library(ggpubr)
#1. Read in the `gapminder_clean.csv` data as a `tibble` using `read_csv`.
gapminder_clean <- tibble(read_csv("R for Data Science/gapminder_clean.csv"))
df = as.data.frame(gapminder_clean)
gapminder_clean %>%
  filter(Year == '1962') %>%
  ggplot(aes(y = `CO2 emissions (metric tons per capita)`, x = gdpPercap)) + 
  geom_point() + theme_bw()

#2. Filter the data to include only rows where `Year` is `1962` and then make a scatter plot comparing `'CO2 emissions (metric tons per capita)'` and `gdpPercap` for the filtered data.
df_1962 = gapminder_clean %>%
  filter(Year == '1962') %>%
  summarise(cor = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$estimate,
            pval = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$p.value
  )
#4. On the unfiltered data, answer "In what year is the correlation between `'CO2 emissions (metric tons per capita)'` and `gdpPercap` the strongest?" Filter the dataset to that year for the next step...
list_of_correlations = gapminder_clean %>%
  group_by(Year) %>%
  summarise(cor = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$estimate,
            pval = stats::cor.test(`CO2 emissions (metric tons per capita)`,gdpPercap)$p.value
            ) %>%
  ungroup()

#1967 highest correlated year

#5. Using `plotly`, create an interactive scatter plot comparing `'CO2 emissions (metric tons per capita)'` and `gdpPercap`, where the point size is determined by `pop` (population) and the color is determined by the `continent`. You can easily convert any `ggplot` plot to a `plotly` plot using the `ggplotly()` command.
plot_1967 = gapminder_clean %>%
  filter(Year == '1967') %>%
  ggplot( aes(gdpPercap,`CO2 emissions (metric tons per capita)`, size = pop, color = continent )) +
  geom_point() +
  theme_bw()

ggplotly(plot_1967)

################################################################################
################################################################################
#1. What is the relationship between `continent` and `'Energy use (kg of oil equivalent per capita)'`? (stats test needed)
df_continent = df

model_ENERGY_CONTINENT_aov =  df_continent %>%
  aov(`Energy use (kg of oil equivalent per capita)`~continent, data = .)

summary(model_ENERGY_CONTINENT_aov) 
#F = 21.88, p<0.05

shapiro.test(resid(aov(log(`Energy use (kg of oil equivalent per capita)`)~continent, data = df_continent)))
#but residuals are not normally distributed

kruskal.test(`Energy use (kg of oil equivalent per capita)`~continent, data = df_continent)
#yes there is a relationship between continent and Energy use
#x^2 = 339.15, p<0.05

df_continent['continent'][is.na(df_continent['continent'])] = 'Other' # change all NAs for continent to other

df_continent_points = df_continent %>%
  filter(is.na(`Energy use (kg of oil equivalent per capita)`) == FALSE) %>%
  select(`Country Name`,`Energy use (kg of oil equivalent per capita)`, continent) %>%
  group_by(`Country Name`, continent) %>%
  summarize(value = mean(`Energy use (kg of oil equivalent per capita)`)) %>%
  ungroup()

df_continent_countries = df_continent %>%
  filter(is.na(`Energy use (kg of oil equivalent per capita)`) == FALSE) %>%
  select(`Country Name`,`Energy use (kg of oil equivalent per capita)`,continent) %>%
  group_by(continent) %>%
  summarize(mean = mean(`Energy use (kg of oil equivalent per capita)`),
            sd = sd(`Energy use (kg of oil equivalent per capita)`),
            se = sd(`Energy use (kg of oil equivalent per capita)`)/sqrt(length(continent))) %>%
  ungroup()

plot_ENERGY_CONTINENT = df_continent_countries %>%
    ggplot() + geom_bar(aes(y = mean, x = continent, fill = continent), stat = 'identity') +
    geom_errorbar(aes(y = mean, x = continent, ymin = mean-se, ymax = mean+se), width  = 0.75) +
    geom_jitter(data = df_continent_points, aes(x = continent, y = value, fill = continent, label = `Country Name`)) + 
    scale_y_continuous(limits = c(0,21000), expand = c(0,0)) + 
    labs(y = 'Energy use (kg of oil equivalent per capita)') + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplotly(plot_ENERGY_CONTINENT)

####both for q1 and q2 plots add individual point reference to country names not just continents
################################################################################
#2. Is there a significant difference between Europe and Asia with respect to `'Imports of goods and services (% of GDP)'` in the years after 1990? (stats test needed) 

ASIA = df %>%
  filter(Year > 1990) %>%
  filter(continent == 'Asia')

EUROPE = df %>%
  filter(Year > 1990) %>%
  filter(continent == 'Europe')

EUROPE_ASIA_AOV = rbind(ASIA,EUROPE)

ks.test(as.numeric(EUROPE_ASIA_AOV$`Imports of goods and services (% of GDP)`), "pnorm")
#non-normal distribution of Imports therefore use non-parametric or use log
#we use a KS test because we want to allow for some tail skewness

EUROPE_ASIA_new_df = as.data.frame(t(rbind(EUROPE_ASIA_AOV$continent, as.numeric(EUROPE_ASIA_AOV$`Imports of goods and services (% of GDP)`))))

colnames(EUROPE_ASIA_new_df) = c("Continent", "Imports")

EUROPE_ASIA_DF_FINAL_ASIA = EUROPE_ASIA_new_df %>%
  filter(is.na(Imports) == FALSE) %>%
  filter(Continent == 'Asia')

EUROPE_ASIA_DF_FINAL_EUROPE = EUROPE_ASIA_new_df %>%
  filter(is.na(Imports) == FALSE) %>%
  filter(Continent == 'Europe')

wilcox.test(as.numeric(EUROPE_ASIA_DF_FINAL_ASIA$Imports), as.numeric(EUROPE_ASIA_DF_FINAL_EUROPE$Imports))

ggplotly(EUROPE_ASIA_AOV %>%
  ggplot(data = ., aes(x = continent, y = `Imports of goods and services (% of GDP)`, colour = Year)) +
  geom_bar(stat = 'summary', fun = 'mean') + geom_jitter(data = EUROPE_ASIA_AOV, aes(label = `Country Name`)) +
  stat_compare_means(method = "wilcox", label.x = 1.5, label.y = 200) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,250)) +
  theme_bw())

#t = 1.35 df = 137.53 p > 0.05
#no detectable difference in contribution of Imports and services between ASIA and EUROPE
###############################################################################
#3. What is the country (or countries) that has the highest `'Population density (people per sq. km of land area)'` across all years? (i.e., which country has the highest average ranking in this category across each time point in the dataset?)

N = c()
HIGHEST = c()

for (i in levels(as.factor(df$Year))) {
  X = df %>%
    select(Year,`Country Name` ,`Population density (people per sq. km of land area)`) %>%
    filter(Year == i) %>%
    na.omit() %>%
    arrange(desc(`Population density (people per sq. km of land area)`)) %>%
    slice(1:10)
  HIGHEST = rbind(HIGHEST,X[1,2])
  N = rbind(N,X)
}

N$Max = c(rep(HIGHEST, each = 10))

PLOT_HIGHEST_POP = N %>%
  ggplot(data = ., aes(x = Year, y = `Population density (people per sq. km of land area)`, fill = `Country Name`, label = Max)) +
  geom_bar(stat = 'identity') + 
  scale_y_continuous(expand = c(0,0), limits = c(0,60000)) + 
  theme_bw()

ggplotly(PLOT_HIGHEST_POP)
####################################################################
#4. What country (or countries) has shown the greatest increase in `'Life expectancy at birth, total (years)'` since 1962?

df_1962 = as.data.frame(df %>%
  filter(Year == 1962))

df_2007 = as.data.frame(df %>%
  filter(Year == 2007))
          
df_LIFE_EXPECTANCY = as.data.frame(cbind(df_1962$`Country Name`, df_1962$`Life expectancy at birth, total (years)`, df_2007$`Life expectancy at birth, total (years)`))
colnames(df_LIFE_EXPECTANCY) = c('Country','1962','2007')

df_LIFE_EXPECTANCY = df_LIFE_EXPECTANCY %>%
  na.omit()

df_LIFE_EXPECTANCY['Difference'] = (as.numeric(df_LIFE_EXPECTANCY$`2007`) - as.numeric(df_LIFE_EXPECTANCY$`1962`))
df_LIFE_EXPECTANCY$Country[which.max(df_LIFE_EXPECTANCY$Difference)]
#Sierra Leone

df_LIFE_EXPECTANCY_TOP_10 = df_LIFE_EXPECTANCY %>%
  arrange(desc(Difference)) %>%
  slice(1:10)

LIFE_EXPECTANCY_PLOT = ggplot(data = df_LIFE_EXPECTANCY_TOP_10, aes(x = Country, y = Difference)) + 
  geom_bar(stat = 'identity') + rotate_x_text(angle = 90) + 
  ylab("Change in Life expectancy between years 1962 to 2007") + 
  xlab("Top 10 countries") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,50))

ggplotly(LIFE_EXPECTANCY_PLOT)

df_all_years_TOP_10 = c()

for (i in df_LIFE_EXPECTANCY_TOP_10$Country) {
  X = df %>%
    filter(`Country Name` == i) %>%
    select(`Country Name`,Year, `Life expectancy at birth, total (years)`)
  df_all_years_TOP_10 = rbind(df_all_years_TOP_10,X)
}

line_difference = ggplot(data = df_all_years_TOP_10, aes(x = Year, y = `Life expectancy at birth, total (years)`, color = `Country Name`)) + 
  geom_line(stat = 'identity') + geom_point()

ggplotly(line_difference)
######done####
