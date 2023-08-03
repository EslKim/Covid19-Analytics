##### Exploratory Data Analysis and Visualization #####
#### Group Project -  
### Analyzing the Impact of Demographic and Economic Factors on COVID-19 Outcomes:
### A Comparative Analysis of USA and Global Trends in Cases and Deaths
### Group Leader: Esl Kim
### Group Members: Max Li, Bora Kayak, Tomomi Satowaga

### Setting working directory
#setwd("/Users/eslkim/Documents/DataVisual/Dataset/GroupProject")

### Installing Packages ### If you never install any packages, please do so by removing (''')
'''
install.packages(dplyr)
install.packages(ggplot2)
install.packages(ggpubr)
install.packages(gridExtra)
install.packages(heatmaply)
install.packages("hydroTSM")
install.packages(lubridate)
install.packages(mgcv)
install.packages(scales)
install.packages(reshape2)
install.packages(tidyverse)
install.packages(tidyr)
'''
### Loading Packages ###
library("dplyr")
library(ggplot2)
library(ggpubr) # regression line equation
library(gridExtra) # grid.arrange()
library(heatmaply) # heatmap
library("hydroTSM")
library(lubridate)
library(mgcv)
library(scales)
library(reshape2)
library(tidyverse)
library(tidyr)

###############################################################################################################
###### Dataset 1. owid-covid-data.csv #####
#### 1.1 Importing CSV file and Cleaning data frame (Max) ####
# Load the data set
covid_data <- read.csv("owid-covid-data.csv", header = TRUE) # 260567 obs. of 67 variables

# Replacing NA into 0 as all empty are data entry per day
covid_data[is.na(covid_data)] <- 0
summary(covid_data)

# Extract year from the date
year <- year(covid_data$date)

# Change date type from character to date
covid_data$date <- as.Date(covid_data$date)
season <- time2season(covid_data$date, out.fmt = "seasons")

# Creating a data frame and combine the year and season into the dataset
covid <- data.frame(covid_data)
covid <- cbind(covid, year, season) # 260567 obs. of 69 variables

# Reorder the year and season (put their positions at front)
covid <- covid[,c(1,2,3,68,69,4:67)]

#### 1.2 Data Analysis and Visualization (Esl) ####
# Correcting the Spelling error in Autumn
covid$season[covid$season == 'autumm'] <- 'autumn'

# Add one variable to Calculate the days from the January 1st 2020
# Move to the 7th column
n = nrow(covid)
firstdate <- as.Date("2020-01-01")
covid$day[1] <- length(seq(from = firstdate, to = covid$date[1], by = 'day')) - 1
covid <- covid[,c(1:6,70, 7:69)]
for(i in 2:n){covid$day[i] <- length(seq(from = firstdate, to = covid$date[i], by = 'day')) - 1}

# Analyze unique numbers in each variables to remove unnecessary variables:
for(i in colnames(covid)){
  cat("Unique values in", i, ":", n_distinct(covid[,i]), "\n")
}

# Select Variables and re-order the column
covid_anal <- covid[,c(1:7,66,56,8,9,11,12,20,21,23,29,30,35,38:42,51:55,57:65)] # 260567 obs. of 38 variables
unique(covid_anal$location) # 13 unique variables

# Create Seperate dataframes for different filtering condition
## 1.A world dataframe: location is World
world <- covid_anal %>% filter(continent == "") %>% filter(location == "World") # 1133 obs. 38 variables
summary(world)

# Change days variables to Calculate the days from earliest date in the dataframe
n1 = nrow(world)
firstdate <- min(world$date) # earliest date
for(i in 1:n1){world$day[i] <- length(seq(from = firstdate, to = world$date[i], by = 'day')) - 1}

# Create 2 dataframes: 1) column that has only 1 unique variables 2) column that has more than 1 unique variables
world_constant <- world %>% select(where(~n_distinct(.) == 1)) %>% distinct() # 1 obs. 24 variables
world_variant <- world %>% select(where(~n_distinct(.) > 1)) # 1133 obs. 14 variables

# Create Scatter plot for Total Cases, Total Deaths, Total Vaccinations by time-series
WRL_totalcases_day <- ggplot(world_variant, aes(x = day, y = total_cases/1000)) + 
  geom_point(color = "black", size = 0.5) + scale_x_continuous(breaks = seq(0,1132,100)) + 
  scale_y_continuous(breaks = seq(0, 700000, 100000)) +
  labs(x = "Number of Days since January 22, 2020", y = "Number of Total Cases (in thousand)") +
  geom_smooth(linetype = "dashed", color = "red", method='lm', formula = y ~ poly(x,3)) + 
  stat_regline_equation(label.x = 100, label.y = 600000,aes(label = ..eq.label..),formula = y ~ poly(x,3)) + 
  stat_regline_equation(label.x = 100, label.y = 550000, aes(label = ..rr.label..),formula = y ~ poly(x,3)) + 
  theme_bw()

WRL_totaldeaths_day <- ggplot(world_variant, aes(x = day, y = total_deaths/1000)) + ylim(c(0,7000)) + 
  geom_point(color = "blue", size = 0.5) + scale_x_continuous(breaks = seq(0,1132,100)) + 
  labs(x = "Number of Days since January 22, 2020", y = "Number of Total Deaths (in thousand)") + 
  geom_smooth(linetype = "dashed", color = "red", method='lm', formula = y ~ poly(x,3)) + 
  stat_regline_equation(label.x = 100, label.y = 6000,aes(label = ..eq.label..),formula = y ~ poly(x,3)) + 
  stat_regline_equation(label.x = 100, label.y = 5500, aes(label = ..rr.label..),formula = y ~ poly(x,3)) + 
  theme_bw()

WRL_totalvaccine_day <- ggplot(world_variant, aes(x = day, y = total_vaccinations/10000)) + 
  geom_point(color = "gold", size = 0.5) + scale_x_continuous(breaks = seq(0,1132,100)) + 
  scale_y_continuous(breaks = seq(0,134000000, 100000)) + 
  labs(x = "Number of Days since January 22, 2020", y = "Number of Total Vaccinations (in 10 thousand)") +
  geom_smooth(linetype = "dashed", color = "red", method='lm', formula = y ~ poly(x,3), se = TRUE) + 
  stat_regline_equation(label.x = 100, label.y = 1200000,aes(label = ..eq.label..),formula = y ~ poly(x,3)) + 
  stat_regline_equation(label.x = 100, label.y = 1000000, aes(label = ..rr.label..),formula = y ~ poly(x,3)) + 
  theme_bw()

WRL_reproduction_day <- ggplot(world_variant, aes(x = day, y = reproduction_rate)) + 
  geom_point(color = "darkgreen", size = 0.5) + scale_x_continuous(breaks = seq(0,1132,100)) + 
  labs(x = "Number of Days since January 22, 2020", y = "Reproduction Rate") + 
  geom_smooth(linetype = "dashed", color = "red") + 
  theme_bw() # Transmission of COVID-19,  number of people

figure1 <- ggarrange(WRL_totalcases_day, WRL_totaldeaths_day, WRL_totalvaccine_day, WRL_reproduction_day,
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2) 
figure1.1 <- annotate_figure(figure1, top = text_grob("COVID-19 Measures since January 22, 2020"))

## 1.B world_continent dataframe: continent: 7873 obs. 38 variables
world_continent <- covid_anal %>% filter(continent == "") %>% 
  filter(location %in% c("Africa", "Asia", "Europe", "European Union","North America", "Oceania", "South America"))

# Change days variables to Calculate the days from earliest date in the dataframe
n2 = nrow(world_continent)
firstdate <- min(world_continent$date) # earliest date
for(i in 1:n2){world_continent$day[i] <- length(seq(from = firstdate, to = world_continent$date[i], by = 'day')) - 1}

# Create 2 dataframes: 1) column that has only 1 unique variables 2) column that has more than 1 unique variables
world_continent_constant <- world_continent %>% select(where(~n_distinct(.) == 1)) %>% distinct() # 1 obs. 22 variables
world_continent_variant <- world_continent %>% select(where(~n_distinct(.) > 1)) # 7873 obs. 16 variables

# Analyze unique numbers in each variables to remove unnecessary variables:
for(i in colnames(world_continent_variant)){
  cat("Unique values in", i, ":", n_distinct(world_continent_variant[,i]), "\n")
}
continent_day_cases <- ggplot(world_continent_variant, 
                              aes(x = day, y = total_cases/1000, color = location)) + 
  geom_point(size = 0.5) + labs(x = "Number of Days since January 22, 2020", 
                                y = "COVID-19 Cases (in Thousand)") + 
  scale_color_discrete(name = "Continent")

continent_day_deaths <- ggplot(world_continent_variant, 
                               aes(x = day, y = total_deaths/1000, color = location)) + 
  geom_point(size = 0.5) + labs(x = "Number of Days since January 22, 2020", 
                                y = "Total COVID-19 Deaths (in Thousand)") + 
  scale_color_discrete(name = "Continent")


continent_day_vaccine <- ggplot(world_continent_variant, 
                                aes(x = day, y = total_vaccinations/10000, color = location)) + 
  geom_point(size = 0.5) + labs(x = "Number of Days since January 22, 2020", 
                                y = "Total COVID-19 Vaccinations (in 10 Thousand)") + 
  scale_color_discrete(name = "Continent") + 
  scale_y_continuous(breaks = seq(0, max(world_continent_variant$total_vaccination)/10000, 125000))

continent_day_vaccine1 <- ggplot(world_continent_variant, 
                                 aes(x = day, y = new_vaccinations/10000, color = location)) + 
  geom_point(size = 0.5) + labs(x = "Number of Days since January 22, 2020", 
                                y = "COVID-19 Vaccinations (in 10 Thousand)") + 
  scale_color_discrete(name = "Continent")

continent_vaccine_death1 <- ggplot(
  world_continent_variant, aes(x = total_deaths/1000, y = new_vaccinations/10000, color = location)) + 
  geom_point(size = 1) + 
  labs(x = "Total COVID-19 Deaths (in Thousand)", 
       y = "COVID-19 Vaccinations (in 10 Thousand)") + 
  scale_fill_discrete(name = "Continent")

continent_vaccine_death2 <- ggplot(
  world_continent_variant, aes(x = new_deaths/1000, y = new_vaccinations/10000, color = location)) + 
  geom_point(size = 1) + xlim(c(0, 10)) + 
  labs(x = "Total COVID-19 Deaths (in Thousand)", 
       y = "COVID-19 Vaccinations (in 10 Thousand)") + 
  scale_fill_discrete(name = "Continent")

figure2 <- ggarrange(continent_day_cases, continent_day_deaths, continent_day_vaccine,
                     labels = c("A", "B", "C"),
                     ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom") 
figure2.1 <- annotate_figure(figure2, top = text_grob("COVID-19 Measures by Continents since January 22, 2020"))

## 1.C world_income dataframe: 4500 obs. 38 variables
world_income <- covid_anal %>% filter(continent == "") %>% 
  filter(location %in% c("High income", "Low income", "Lower middle income", "Upper middle income"))

# Change days variables to Calculate the days from earliest date in the dataframe
n3 = nrow(world_income)
firstdate <- min(world_income$date) # earliest date
for(i in 1:n3){world_income$day[i] <- length(seq(from = firstdate, to = world_income$date[i], by = 'day')) - 1}

# Create 2 dataframes: 1) column that has only 1 unique variables 2) column that has more than 1 unique variables
world_income_constant <- world_income %>% select(where(~n_distinct(.) == 1)) %>% distinct() # 1 obs. 22 variables
world_income_variant <- world_income %>% select(where(~n_distinct(.) > 1)) # 4500 obs. 16 variables

income_death <- ggplot(world_income_variant, 
                       aes(x = day, y = total_deaths/1000, 
                           color = factor(
                             location, levels = 
                               c("High income", "Upper middle income","Lower middle income", "Low income")))) + 
  geom_point(size = 0.5) + scale_color_discrete(name = "Income Levels") + 
  labs(y = "Number of COVID-19 Deaths (in Thousand)", x = "Number of Days since January 22, 2020")
income_death
income_vaccine <- ggplot(world_income_variant, 
                         aes(x = factor(
                           location, levels = 
                             c("High income", "Upper middle income","Lower middle income", "Low income")), 
                           y = people_fully_vaccinated, 
                           fill = factor(
                             location, levels = 
                               c("High income", "Upper middle income","Lower middle income", "Low income")))) + 
  geom_boxplot() + scale_fill_discrete(name = "Income Levels") + 
  labs(y = "Number of People Fully Vaccinated", x = "Income Levels")
income_vaccine

figure3 <- ggarrange(income_death, income_vaccine,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom") 
figure3.1 <- annotate_figure(figure3, top = text_grob("COVID-19 Measures by Income level since January 22, 2020"))

## 1.D Dataframe for all countries: 245944 obs. 38 variables
country <- covid_anal %>% filter(continent != "")
unique(country$location) # 235 countries
unique(country$continent) # 6 Continents (Not include - European Union)

ggplot(country, aes(x = female_smokers, y = new_deaths, color = median_age)) + geom_point()
ggplot(country, aes(x = median_age, y = hosp_patients, color = female_smokers )) + geom_point()
ggplot(country, aes(x = median_age, y = icu_patients, color = female_smokers)) + geom_point()
ggplot(country, aes(x = female_smokers, y = reproduction_rate, color = median_age)) + geom_point()

# 2 Dataframes for countries 1) Without time line 2) With year and season
country_brief <- country %>% group_by(iso_code, continent, location) %>%
  summarise(population = max(population), gdp_per_capita = max(gdp_per_capita), 
            cases = sum(new_cases), deaths = sum(new_deaths),
            reproduction_rate = sum(reproduction_rate), icu_patients = sum(icu_patients), 
            hosp_patients = sum(hosp_patients), total_vaccination = max(total_vaccinations),
            people_vaccinated = max(people_vaccinated),people_fully_vaccinated = max(people_fully_vaccinated),
            total_boosters = max(total_boosters),vaccination = sum(new_vaccinations),
            stringency_index = max(stringency_index), population_density = max(population_density),
            median_age = max(median_age), aged_65_older = max(aged_65_older), 
            aged_70_older = max(aged_70_older), 
            extreme_poverty = max(extreme_poverty), cardiovasc_death_rate = max(cardiovasc_death_rate),
            diabetes_prevalence = max(diabetes_prevalence), female_smokers = max(female_smokers),
            male_smokers = max(male_smokers), handwashing_facilities = max(handwashing_facilities),
            hospital_beds_per_thousand = max(hospital_beds_per_thousand), 
            life_expectancy = max(life_expectancy), human_development_index = max(human_development_index))

# Create Box plots between variables            
contient_death <- ggplot(country_brief, aes(x = continent, y = deaths, fill = continent)) + geom_boxplot() + ylim(c(0, 200000))
ggplot(country_brief, aes(x = continent, y = cases, fill = continent)) + geom_boxplot()
gdp_2021 <- ggplot(country_brief, aes(x = continent, y = gdp_per_capita, fill = continent)) + geom_boxplot() + 
  theme(legend.position = 'none') + 
  labs(x = "Continent", y = "GDP per capita", title = "GDP per capita by Continent During COVID-19 Pandemic") + 
  theme(plot.title = element_text(hjust = 0.5))


# Create Scatter plots between variables  
female_smoke <- ggplot(country_brief, 
                       aes(x = female_smokers, y = reproduction_rate, color = median_age)) + 
  geom_point() + labs(x = "Share of Female Smokers", y = "Reproduction rate of COVID-19") + 
  scale_color_continuous(name = "Median Age") + xlim(c(1,80)) 

ggplot(country_brief, aes(x = male_smokers, y = deaths, color = median_age)) + geom_point()
ggplot(country_brief, aes(x = male_smokers, y = hosp_patients, color = continent)) + geom_point()

male_smoke <- ggplot(country_brief, 
                     aes(x = male_smokers, y = reproduction_rate, color = median_age)) + 
  geom_point() + labs(x = "Share of Male Smokers", y = "Reproduction rate of COVID-19") + 
  scale_color_continuous(name = "Median Age") + xlim(c(1,80))

ggplot(country_brief, aes(x = median_age, y = deaths)) + geom_point()
ggplot(country_brief, aes(x = median_age, y = hosp_patients)) + geom_point()
ggplot(country_brief, aes(x = median_age, y = icu_patients)) + geom_point()

median_female <- ggplot(country_brief, 
                        aes(x = median_age, y = reproduction_rate, color = female_smokers)) + 
  geom_point() + labs(x = "Median Age", y = "Reproduction rate of COVID-19",
                      color = "Share of Female Smokers") + xlim(c(1,50)) + 
  scale_color_gradient(low = "#56B1F7", high = "#132B43") +  theme_bw() 


median_male <- ggplot(country_brief, 
                      aes(x = median_age, y = reproduction_rate, color = male_smokers)) + 
  geom_point() + labs(x = "Median Age", y = "Reproduction rate of COVID-19", 
                      color = "Share of Male Smokers") + xlim(c(1,50)) + 
  scale_color_gradient(low = "snow3", high = "purple4") + theme_bw()

figure4 <- ggarrange(female_smoke, male_smoke,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1, common.legend = TRUE, legend = "right") 
figure4.1 <- annotate_figure(figure4, top = text_grob("Share of Smokers and Reproduction rate of COVID-19"))

figure5 <- ggarrange(median_female, median_male,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1) 
figure5.1 <- annotate_figure(figure5, top = text_grob("Median Age and Reproduction rate of COVID-19"))

country_season <- country %>% group_by(iso_code, continent, location, year, season) %>%
  summarise( 
    cases = sum(new_cases), deaths = sum(new_deaths),
    reproduction_rate = sum(reproduction_rate), icu_patients = sum(icu_patients), 
    hosp_patients = sum(hosp_patients), total_vaccination = max(total_vaccinations),
    people_vaccinated = max(people_vaccinated),people_fully_vaccinated = max(people_fully_vaccinated),
    total_boosters = max(total_boosters),vaccination = sum(new_vaccinations),
    cardiovasc_death_rate = max(cardiovasc_death_rate),
    diabetes_prevalence = max(diabetes_prevalence), female_smokers = max(female_smokers),
    male_smokers = max(male_smokers), handwashing_facilities = max(handwashing_facilities),
    hospital_beds_per_thousand = max(hospital_beds_per_thousand))
# Season box plots   
ggplot(country_season, 
       aes(x = factor(season, levels = c("spring", "summer", "autumn", "winter")), y = cases)) + geom_boxplot()
ggplot(country_season, 
       aes(x = factor(season, levels = c("spring", "summer", "autumn", "winter")), y = deaths)) + geom_boxplot()
season_reproduction <- ggplot(country_season, 
                              aes(x = factor(season, levels = c("spring", "summer", "autumn", "winter")), 
                                  y = reproduction_rate, 
                                  fill = factor(season, levels = c("spring", "summer", "autumn", "winter")))) + 
  geom_boxplot() + labs(x = "Seasons", y = "Reproduction rate of COVID-19") + scale_fill_discrete(name = "Seasons")

# Year box plots
ggplot(country_season, aes(x = factor(year), y = cases)) + geom_boxplot()
ggplot(country_season, aes(x = factor(year), y = deaths)) + geom_boxplot()
year_reproduction <- reproduction <- ggplot(country_season, aes(x = factor(year), y = reproduction_rate, fill = factor(year))) + 
  geom_boxplot()  + labs(x = "Year", y = "Reproduction rate of COVID-19") + scale_fill_discrete(name = "Year")

###############################################################################################################
#### Put another graphs to visualize the data by dividing year of Omicro vs non-Omicron era

# Continent data summary (3120 obs. 11 variables)
summary_continent <- covid_anal %>% group_by(location, year, season, day) %>% 
  summarise(population = max(population), cases = sum(new_cases), deaths = sum(new_deaths),
            vaccination = sum(new_vaccinations),total_vaccination = max(total_vaccinations),
            people_vaccinated = max(people_vaccinated),people_fully_vaccinated = max(people_fully_vaccinated),
            total_boosters = max(total_boosters))
# Season order by: spring, summer, autumn, winter
season_order <- c("spring","summer", "autumn", "winter")
summary_continent <- summary_continent %>% slice(match(season_order, season))
# Create 1 extra column for year + season:
summary_continent$year_season <- paste(summary_continent$year, summary_continent$season, sep= " ")
summary_continent$year_season1 <- paste(summary_continent$year, summary_continent$season, sep= " ")
summary_continent$year_season1 <-c("2020 spring"= 0,"2020 summer"= 1,"2020 autumn" = 2,"2020 winter" = 3, 
                                   "2021 spring" = 4,"2021 summer" = 5,"2021 autumn" = 6,
                                   "2021 winter" = 7,"2022 spring" = 8, "2022 summer" = 9, 
                                   "2022 autumn" = 10,"2022 winter" = 11,"2023 winter" = 12)[summary_continent$year_season]
summary_continent$year_grp <-summary_continent$year

summary_continent$year_grp[summary_continent$year_grp <= "2021"] <- "Before Omicron(B.1.1.7)"
summary_continent$year_grp[summary_continent$year_grp == "2022"] <- "Original Omicron (BA.5)"
summary_continent$year_grp[summary_continent$year_grp == "2023"] <- "New Omicron subvariant(XBB.1.5)"

summary_continent$year_grp1 <-summary_continent$year_grp
summary_continent$year_grp1[summary_continent$year_grp1 != "Original Omicron (BA.5)"] <- 1
summary_continent$year_grp1[summary_continent$year_grp1 == "Original Omicron (BA.5)"] <- 2
summary(summary_continent)
### Comparison between cases and deaths depends on rise of Omicron
figure6 <- ggplot(summary_continent, aes(x = cases/1000, y = deaths/1000, color = year_grp1)) + geom_point() + 
  labs(x = "Number of COVID-19 Cases (in Thousand)", y = "Number of COVID-19 Deaths (in Thousand)", 
       color = "Year of Rising Variant", title = "Number of COVID-19 Outcomes by Rising of Omicron") + 
  scale_color_discrete(labels = c("Before/After Omicron(2020, 2021, 2023)", "Omicron(2022)")) + theme_bw() + 
  geom_smooth(method = 'lm') + theme(plot.title = element_text(hjust = 0.5)) + 
  stat_regline_equation(label.x = c(2.5,2.5), label.y = c(13, 14),aes(label = ..eq.label..)) + 
  stat_regline_equation(label.x = c(600,600), label.y = c(13, 14), aes(label = ..rr.label..)) + 
  stat_cor(label.x = c(1000,1000), label.y = c(13, 14), 
           aes(label = paste("P-value: ", signif(..p.value.., digits = 4), sep = " ")))

## Mortality rate
summary_continent <- summary_continent %>% filter(cases != 0)
summary_continent$mortality <- summary_continent$deaths/summary_continent$cases

figure7 <- ggplot(summary_continent, 
                  aes(x = factor(year_grp, 
                                 levels = 
                                   c("Before Omicron(B.1.1.7)","Original Omicron (BA.5)","New Omicron subvariant(XBB.1.5)")), 
                                 y = mortality, 
                      fill = factor(year_grp, 
                                    levels = 
                                      c("Before Omicron(B.1.1.7)","Original Omicron (BA.5)","New Omicron subvariant(XBB.1.5)")))) + 
  geom_boxplot() + 
  labs(x = "Time of COVID-19 Variants", y = "Mortality Rate of COVID-19", fill = "Year of Rising Variant") + theme_bw()

figure7 <- figure7 + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) 

figure7.1 <- ggplot(summary_continent, 
                  aes(x = factor(year_grp, 
                                 levels = 
                                   c("Before Omicron(B.1.1.7)","Original Omicron (BA.5)","New Omicron subvariant(XBB.1.5)")), 
                      y = log(mortality), 
                      fill = factor(year_grp, 
                                    levels = 
                                      c("Before Omicron(B.1.1.7)","Original Omicron (BA.5)","New Omicron subvariant(XBB.1.5)")))) + 
  geom_boxplot() + 
  labs(x = "Time of COVID-19 Variants", 
       y = "Log of Mortality Rate of COVID-19", fill = "Year of Rising Variant") + theme_bw()
figure7.1 <- figure7.1 + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) 

figure7.2 <- ggarrange(figure7, figure7.1,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1, common.legend = TRUE, legend = "right") 
###############################################################################################################

##### Dataset 2. total-confirmed-cases-of-covid-19-per-million-people-vs-gdp-per-capita.csv #####
#### 2.1 Importing CSV file and Cleaning data frame (Bora) ####
TotalConfirmedCases <-read.csv("total-confirmed-cases-of-covid-19-per-million-people-vs-gdp-per-capita.csv")

mainDf_gdp <- as.data.frame(TotalConfirmedCases)

#changing the column names for easy access

colnames(mainDf_gdp)[1] <- "Location"
colnames(mainDf_gdp)[2] <- "iso_code"
colnames(mainDf_gdp)[3] <- "Year"
colnames(mainDf_gdp)[7] <- "GDP"

#### Required for Non-Macbook ################################
#changing the chr type date to a regular date format 
#mainDf_gdp$Day <- strptime(as.character(mainDf_gdp$Day), "%d/%m/%Y")

#changing the regular date format to a POSIXct in order to use it later
#(You cant change it directly into a POSIX format thats why we change it to a date format first)
#mainDf_gdp$Day <- as.POSIXct(mainDf_gdp$Day, format = "%Y-%m-%d %H:%M:%S")
##############################################################################

#Dividing months to the seasons in order to group them later
mainDf_gdp$Seasons <- case_when(month(mainDf_gdp$Day) %in% c(12, 1, 2) ~ "Winter",
                            month(mainDf_gdp$Day) %in% c(3, 4, 5) ~ "Spring",
                            month(mainDf_gdp$Day) %in% c(6, 7, 8) ~ "Summer",
                            month(mainDf_gdp$Day) %in% c(9, 10, 11) ~ "Fall")

#Selecting the necessary columns for the final table
mainDf_gdp <- mainDf_gdp %>%
  select(c("iso_code","Continent", "Location","Year","Seasons", "GDP" ))

#check for NA values
sum(is.na(mainDf_gdp)) #we see that there are alot of NA values in the GDP section because of Continent data

#change them to 0
mainDf_gdp[is.na(mainDf_gdp)] <- 0 

#Filtering out the unnecessary columns from the mainDf_gdp
mainDf_gdp <- mainDf_gdp %>%
  filter(Location != "High income") %>%
  filter(Location != "Low income") %>%
  filter(Location != "Upper middle income") %>%
  filter(Location != "World excl. China") %>%
  filter(Location != "Lower middle income")


# Create a finalDf_gdp with appropriate columns from the 
# mainDf_gdp and group by their respective counterparts

finalDf_gdp <- mainDf_gdp %>% group_by(iso_code, Continent, Location, Year, Seasons, ) %>% 
  summarise(GDP_mean=mean(GDP))
#I used "mean" on the GDP because I think that represents the best of that country's GDP around that time period

#### 2.2 Data Analysis and Visualization (Tomomi) ####
# Define is_blank
# is_blank <- function(x) {is.na(x) | x == ""}

# Eliminating if the iso_code = "" and Continent = ""
finalDf_gdp2 <- finalDf_gdp[!finalDf_gdp[,"iso_code"] == "",]
finalDf_gdp2 <- finalDf_gdp2[!finalDf_gdp2[,"Continent"] == "",]
finalDf_gdp2 <- finalDf_gdp2[!finalDf_gdp2[,"GDP_mean"] == 0,]

unique(finalDf_gdp2$Continent)


# Scatter plot
continent_avg_gdp1 <- subset(finalDf_gdp2, select = c(Continent, Year, GDP_mean))
continent_avg_gdp1 %>%
  mutate(Continent2 = case_when(Continent == "NorthAmerica" ~ "NorthAmerica",
                                Continent == "Africa" ~ "Africa",
                                Continent == "Europe" ~ "Europe",
                                Continent == "South America" ~ "South America",
                                Continent == "Oceania" ~ "Oceania"),
         Continent2 = factor(Continent2,
                             levels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),
                             labels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),)) %>%
  ggplot() +
  geom_point(aes(x = Year, y = GDP_mean, color = Continent2)) +
  labs ( x = "Year", y = "GDP_mean", color = "Continent") +
  theme_bw()


# Box Plot
continent_avg_gdp2 <- subset(finalDf_gdp2, select = c(Continent, Year, GDP_mean))
continent_avg_gdp2 %>%
  ggplot() +
  geom_boxplot(aes(x = Continent, y = GDP_mean, fill = Continent),
               show.legend = FALSE) + scale_y_continuous(breaks=seq(0,1500000,30000)) + 
  labs(x = "Continent", y = "GDP per capita", title = "GDP per capita by Continent Before COVID-19 Pandemic") + 
  theme(plot.title = element_text(hjust = 0.5))


# Scatter plot 3

continent_avg_gdp3 <- subset(finalDf_gdp2, select = c(Continent, Seasons, GDP_mean))

continent_avg_gdp3$Seasons[is.na(match(continent_avg_gdp3$Seasons, "Winter")) == FALSE] <- 1
continent_avg_gdp3$Seasons[is.na(match(continent_avg_gdp3$Seasons, "Spring")) == FALSE] <- 2
continent_avg_gdp3$Seasons[is.na(match(continent_avg_gdp3$Seasons, "Summer")) == FALSE] <- 3
continent_avg_gdp3$Seasons[is.na(match(continent_avg_gdp3$Seasons, "Fall")) == FALSE] <- 4

continent_avg_gdp3 %>%
  mutate(Continent3 = case_when(Continent == "NorthAmerica" ~ "NorthAmerica",
                                Continent == "Africa" ~ "Africa",
                                Continent == "Europe" ~ "Europe",
                                Continent == "South America" ~ "South America",
                                Continent == "Oceania" ~ "Oceania"),
         Continent3 = factor(Continent3,
                             levels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),
                             labels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),)) %>%
  ggplot() +
  geom_point(aes(x = Seasons, y = GDP_mean, color = Continent3)) +
  labs ( x = "Seasons", y = "GDP_mean", color = "Continent") +
  theme_bw()

# Scatter plot4
continent_avg_gdp4 <- subset(finalDf_gdp2, select = c(Continent, Seasons, GDP_mean))

continent_avg_gdp4$Seasons[is.na(match(continent_avg_gdp4$Seasons, "Winter")) == FALSE] <- 1
continent_avg_gdp4$Seasons[is.na(match(continent_avg_gdp4$Seasons, "Spring")) == FALSE] <- 2
continent_avg_gdp4$Seasons[is.na(match(continent_avg_gdp4$Seasons, "Summer")) == FALSE] <- 3
continent_avg_gdp4$Seasons[is.na(match(continent_avg_gdp4$Seasons, "Fall")) == FALSE] <- 4

continent_avg_gdp4 %>%
  ggplot() +
  geom_boxplot(aes(x = Seasons, y = GDP_mean, fill = Continent),
               show.legend = TRUE) +
  labs(x = "Seasons", y = "GDP_mean")
###############################################################################################################

##### Dataset 3. Provisional_COVID-19_Deaths_by_Sex_and_Age.csv #####
#### 3.1 Importing CSV file and Cleaning data frame (Bora) ####
covidDatasetSexAndAge <- read.csv("Provisional_COVID-19_Deaths_by_Sex_and_Age.csv") # 118422 obs. with 16 variables
# Deselecting the Footnote column from the dataframe
covidSexAge <- covidDatasetSexAndAge %>%
  select(!c("Footnote"))
# Changing NA Values to 0 as all NA are data points (for 0) for specific time period
covidSexAge[is.na(covidSexAge)] <- 0 
summary(covidSexAge) # 118422 observations with 15 variables

#### 3.2 Data Analysis (Esl) ####
# Calculating the total number of deaths by influenza, pneumonia and Covid19
covidSexAge$Influenza.Pneumonia.COVID.19.Deaths <- 
  rowSums(covidSexAge[,c("COVID.19.Deaths","Pneumonia.Deaths","Influenza.Deaths")])

# Calculating the total number of deaths by influenza and Covid19
covidSexAge$Influenza.and.COVID.19.Deaths <-
  covidSexAge$Influenza.Pneumonia.COVID.19.Deaths - 
  covidSexAge$Pneumonia..Influenza..or.COVID.19.Deaths - 
  covidSexAge$Pneumonia.and.COVID.19.Deaths

summary(covidSexAge) # 118422 observations with 17 variables after calculation

# Reduce/filter Age group as below as their are some overlaps.
# AgeGroup: All Ages, Under 1 year,1-4 years, 5-14 years, 15-24 years, 
# 25-34 years, 35-44 years, 45-54 years, 55-64 years, 65-74 years, 
# 75-84 years,85 years and over

usa_covid19 <- filter(covidSexAge,covidSexAge$Age.Group %in% 
                        c("All Ages", "Under 1 year","1-4 years", "5-14 years", 
                          "15-24 years", "25-34 years", "35-44 years", 
                          "45-54 years", "55-64 years", "65-74 years", 
                          "75-84 years", "85 years and over"))
# Change character value names: 
# All Ages: All # Under 1 year: < 1
# 1-4 years: 1-4.. # 85 years and over: >=85

usa_covid19$Age.Group <- sub("years","",usa_covid19$Age.Group) # remove "years"
usa_covid19["Age.Group"][usa_covid19["Age.Group"] == "All Ages"] <- "All"
usa_covid19["Age.Group"][usa_covid19["Age.Group"] == "Under 1 year"] <- "< 1"
usa_covid19["Age.Group"][usa_covid19["Age.Group"] == "85  and over"] <- ">= 85"
# 83592 observations with 17 variables 
unique(usa_covid19["Age.Group"]) # Total 12 Age Groups

#### 3.3 Data Analysis and Visualization for whole United States(Tomomi) ####
# Making a new data frame by whole united USA
usa_covid19_whole <- usa_covid19[usa_covid19$State == "United States", ]
# 1548 observations with 17 variables

# Changing the format to long format
usa_covid19_whole_l <- gather(usa_covid19_whole, key ="Causes", value = "Cases",
                              COVID.19.Deaths, Pneumonia.and.COVID.19.Deaths,
                              Influenza.and.COVID.19.Deaths)

# Eliminating Age.Group = All
usa_covid19_whole_l <- usa_covid19_whole_l[!usa_covid19_whole_l[,"Age.Group"] == "All",]

# Eliminating Sex = All Sexes
usa_covid19_whole_l <- usa_covid19_whole_l[!usa_covid19_whole_l[,"Sex"] == "All Sexes",]
# 2838 obs. 16 variables

# Comparing Covid19 Deaths / Covid19 + Pneumonia Deaths/ 
# Covid19 + Influenza Deaths by age group
ggplot(data = usa_covid19_whole_l, 
       mapping = aes(x = Age.Group, y = Cases, fill = Causes)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_comma())

# Comparing Covid19 Deaths / Covid19 + Pneumonia Deaths/ 
# Covid19 + Influenza Deaths by gender
ggplot(data = usa_covid19_whole_l, 
       mapping = aes(x = Sex, y = Cases, fill = Causes)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_comma())


# Comments
# By age, 85 or over has the most Covid19 Deaths, while 75-84 has 
# the most Pneumonia and COVID19. 
# Influenza and Covid19 is very few and almost same number.
# By gender, Female has the most Covid19 Deaths, while Male has the most
# Pneumonia Deaths.
# The same as by age, Influenza and Covid19 is very few and almost same number.

# Eliminating inaccurate data
usa_covid19_whole_l <- usa_covid19_whole_l[!(usa_covid19_whole_l$Year == 0 |
                                               usa_covid19_whole_l$Month == 0),] 
# Adding a variable
usa_covid19_whole_l <- mutate(usa_covid19_whole_l, 
                              YearMonth = paste(Year, Month, sep = "/"))

death_bydate <- ggplot(usa_covid19_whole_l, aes(x = YearMonth, y = Cases, color = Causes)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) + 
  scale_y_continuous(labels = label_comma())

# Comments
# Covid19 Deaths and Pneumonia and Covid19 deaths are up and down,
# while Influenza Deaths almost flat.

#### 3.4 Data Analysis and Visualization by States in USA (Esl) ####
# Re-order/transformation for Tomomi's graph with deatil changes
# Name of the plot, ordering of age group, order of causes in the graphs
deathsbydisease_age <- ggplot(
  data = usa_covid19_whole_l, 
  mapping = aes(x = factor(Age.Group,
                           level = c("< 1","1-4 ", "5-14 ", "15-24 ", "25-34 ", "35-44 ", 
                                     "45-54 ", "55-64 ", "65-74 ", "75-84 ", ">= 85")), 
                y = Cases, 
                fill = factor(Causes, 
                              levels = c("Influenza.and.COVID.19.Deaths",
                                         "Pneumonia.and.COVID.19.Deaths",
                                         "COVID.19.Deaths")))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Age Group", y = "Number of Cases", fill = "Deaths of COVID 19 and different diseases") +
  scale_y_continuous(labels = label_comma()) + scale_fill_hue(direction = -1)

deathsbydisease_gender <- ggplot(
  data = usa_covid19_whole_l, 
  mapping = aes(x = Sex, y = Cases, 
                fill = factor(Causes, 
                              levels = c("Influenza.and.COVID.19.Deaths",
                                         "Pneumonia.and.COVID.19.Deaths","COVID.19.Deaths")))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y = "Number of Cases", fill = "Deaths of COVID 19 and different diseases") + 
  scale_y_continuous(labels = label_comma()) + scale_fill_hue(direction = -1)

grid.arrange(deathsbydisease_age, deathsbydisease_gender) # Put Tomomi's graphs together

# Analyze unique numbers in each variables to remove unnecessary variables:
for(i in colnames(usa_covid19)){
  cat("Unique values in", i, ":", n_distinct(usa_covid19[,i]), "\n")
}

usa <- usa_covid19 %>% 
  filter(!State %in% c("United States", "New York City"))
usa_bytotal <- usa %>% filter(Group == "By Total") %>% 
  filter(!Sex == 'All Sexes', !Age.Group =='All')

usa_bytotal$Influenza.and.COVID.19.Deaths <- abs(usa_bytotal$Influenza.and.COVID.19.Deaths)
usa_bytotal <- usa_bytotal %>% select(!c("Data.As.Of","Start.Date","End.Date",
                                         "Group", "Year","Month", "Total.Deaths")) # 1144 obs. 10 variables
# Normalize and Create Heatmap for different deaths of diseases
usa_normalize <- normalize(usa_bytotal)
heatmaply(usa_normalize[,-c(1,2,3)],row_side_colors = usa_normalize[, 1:3])

### Dataset 3a. 2020-2023_Temperature_by_state.csv
usa_state <- usa_covid19 %>% filter(!usa_covid19$State %in% 
                                      c("United States","Hawaii", 
                                        "New York City", "Puerto Rico")) 
usa_Temp_bymonth <- read.csv("2020-2023_Temperature_by_state.csv")
unique(usa_Temp_bymonth$StateName)
usa_state_all <- usa_state %>% filter(usa_state$Group == 'By Month', 
                                      usa_state$Sex == 'All Sexes',
                                      usa_state$Age.Group =='All')
usa_state_all$Date1 <- paste(usa_state_all$Year, usa_state_all$Month,
                             sep = "")
usa_state_all <- usa_state_all %>% filter(!usa_state_all$Date1 == 20232)
unique(usa_state$State)
usa_statewtemp <- cbind(usa_state_all,usa_Temp_bymonth)
usa_statewtemp <- usa_statewtemp %>% select(!c("StateName","Data.As.Of","Start.Date",
                                               "End.Date","Date1"))
usa_statewtemp$Influenza.and.COVID.19.Deaths <- abs(usa_statewtemp$Influenza.and.COVID.19.Deaths)

# Create Scatter Plot
usa_covid19_scatter <- ggplot(usa_statewtemp, 
                              aes(x=factor(Date/100), y = (COVID.19.Deaths/1000), 
                                  group = 1, color = State)) + 
  geom_point() + theme_bw() + 
  ggtitle("COVID 19 Deaths in the USA between 2020.Jan and 2023.Jan") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Date(YY.MM)", y = "Number of COVID Deaths in Thousand", color = "State") + 
  geom_smooth(method = loess) 
usa_covid19_scatter

# Comment: 
# There is no correlation between days and deaths of COVID 19 
# Through the graph, we may observe the slight increases in the number of COVID 19
# between end of 2020 to mid 2021 and it decreases as time towards today. 

usa_covid19_scatter1 <- ggplot(usa_statewtemp, aes(x=Avg.Temp..F., y = (COVID.19.Deaths/1000), 
                                                   group = 1, color = State)) + 
  geom_point() + theme_bw() + 
  ggtitle("COVID 19 Deaths in the USA depends on Temperature") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Temperature(in Fahrenheit)", y = "Number of COVID Deaths in Thousand", color = "State") + 
  geom_smooth() + ylim(c(0,10)) 

usa_covid19_scatter1
# Comment: 
# There is no correlation between temperature and deaths of COVID 19 
# There is no significance between temperature and deaths of COVID 19

# Boxplot for deaths by state (20,000 max deaths)
usa_covid19_boxplot <- ggplot(
  usa_statewtemp, aes(x= State, y = (COVID.19.Deaths/1000), fill = State)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0))

# Remove most outliers; Deaths 5,000 max , delete legend
usa_covid19_boxplot1 <- ggplot(
  usa_statewtemp, aes(x= State, y = (COVID.19.Deaths/1000), fill = State)) + 
  geom_boxplot() + ylim(c(0,5)) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0), 
        legend.position = "none") + labs(y = "COVID 19 Deaths (per Thousand)")

# Comment: 
# Higher the population state, higher the number of death due to COVID-19






