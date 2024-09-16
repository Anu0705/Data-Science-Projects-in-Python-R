library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)


## Get current Data in the four files
# they all begin the same way
url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
file_names <- c("confirmed_global.csv",
                "deaths_global.csv",
                "confirmed_US.csv",
                "deaths_US.csv",
                "recovered_global.csv")
urls <- str_c(url_in,file_names)

# Let's read in the data and see what we have.

global_cases <- read_csv(urls[1], show_col_types = FALSE)
global_deaths <- read_csv(urls[2], show_col_types = FALSE) 

# Unpivoting dates and values of cases

global_cases <- global_cases %>%
  pivot_longer(cols = -c(`Province/State`, 
                         `Country/Region`, Lat, Long), 
               names_to = "date", 
               values_to = "cases") %>%
  select(-c(Lat,Long))

# unpivoting dates and values of deaths

global_deaths <- global_deaths %>%
  pivot_longer(cols = -c(`Province/State`,
                         `Country/Region`, Lat, Long), 
               names_to = "date", 
               values_to = "deaths") %>%
  select(-c(Lat, Long))

#full  joining global cases and global deaths based on the key Joining, by = c("Province/State", "Country/Region", "date")

global <- global_cases %>% 
  full_join(global_deaths) %>%
  rename(Country_Region = `Country/Region`, 
         Province_State = `Province/State`) %>%
  mutate(date = mdy(date))

## Joining, by = c("Province/State", "Country/Region", "date")

summary(global)

# filtering global dataset which have cases

global <- global %>% filter(cases > 0)

global <- global %>% 
  unite("Combined_Key", 
        c(Province_State, Country_Region), 
        sep = ", ", 
        na.rm = TRUE, 
        remove = FALSE)

# Let's add in recoveries data to the global dataset.

 # Your Analysis

# To answer the question that how has number of recoveries or recovery rate changed over the last few months
# Also with respect to the no. of deaths should lower as recoveries increase and vice versa

# Let us look at the cases recovered data

recovered_cases <- read_csv(urls[5], show_col_types = FALSE)

recovered_cases <- recovered_cases %>%
  pivot_longer(cols = -c(`Province/State`, 
                         `Country/Region`, Lat, Long), 
               names_to = "date", 
               values_to = "recoveries") %>%
  select(-c(Lat,Long))

summary(recovered_cases)
str(recovered_cases)

recovered_cases <- recovered_cases %>% 
    rename(Country_Region = `Country/Region`, 
         Province_State = `Province/State`) %>%
    filter(recoveries > 0) %>%
  mutate(date = mdy(date), year = year(date))

# Let us add recoveries data to the global dataset for further analysis

global <- global %>% 
  full_join(recovered_cases, by = c("Province_State", "Country_Region", "date")) %>%
    select(Province_State, Country_Region, date,
         cases, deaths, recoveries,
         Combined_Key)
global

global["year"] <- lubridate::year(date)

# Visualize the data 

global %>% 
  filter(cases > 0) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = recoveries, color = "recoveries")) +
  geom_point(aes(y = recoveries, color = "recoveries")) +
  scale_x_date(limits = as.Date(c('2020-01-26','2021-07-31'))) +
  scale_y_log10() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(title = "COVID19 Global", y= NULL)


# Visualize number of recoveries in Australia
Australia <- global %>% filter(Country_Region == "Australia")

Australia %>% 
  filter(cases > 0) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  geom_line(aes(y = recoveries, color = "recoveries")) +
  geom_point(aes(y = recoveries, color = "recoveries")) +
  scale_x_date(limits = as.Date(c('2020-01-26','2021-07-31'))) +
scale_y_log10() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(title = "COVID19 Aus", y= NULL)


# Filtering for Australia
# As Australia had revealed in an announcement that there are minimal cases and more recoveries in comparison with other countries


#Victoria, Australia
# Consolidating totals of number of cases, deaths and recoveries

Aus_totals <- Australia %>%
  group_by(Country_Region, date) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), 
             Recoveries = sum(recoveries, na.rm = TRUE)) %>%
  select(Country_Region, date,
         cases, deaths, Recoveries) %>%
  ungroup()

Aus_totals %>% 
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_bar(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  geom_bar(aes(y = Recoveries, color = "Recoveries")) +
  geom_point(aes(y = Recoveries, color = "Recoveries")) +
  scale_x_date(limits = as.Date(c('2020-01-26','2021-07-31')))
  scale_y_log10() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(title = "COVID19 Aus", y= NULL)

# Comparison with a state

Aus_totals_by_state <- Australia %>%
  group_by(Province_State, date) %>%
  summarize(cases = sum(cases), deaths = sum(deaths), 
             Recoveries = sum(recoveries, na.rm = TRUE)) %>%
  select(Province_State,date,
         cases, deaths, Recoveries) %>%
  ungroup()

state <- "Victoria"
Aus_totals_by_state %>% 
  filter(Province_State == state) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  geom_line(aes(y = Recoveries, color = "Recoveries")) +
  geom_point(aes(y = Recoveries, color = "Recoveries")) +
  scale_y_log10() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID19 ", state), y= NULL)

# Analyze the data
# cumulative sum over the time shows that the number of recoveries 
# have increased with decrease in number of deaths over the time

#  This measure is also called running total that sums up the day wise totals going forward with each day

Aus_totals_by_state <- Aus_totals_by_state %>%
  mutate(cases_cum = cumsum(cases), 
         deaths_cum = cumsum(deaths),
         rec_cum = cumsum(Recoveries))


# if required, plot the same for AUS totals

# Grouping the provinces of Australia to find out the mean, median , IQR, min and max of the metrics
Aus_totals_by_state %>% 
  summarize(mean = mean(Recoveries, na.rm=TRUE), 
            median = median(Recoveries, na.rm=TRUE))
# add IQR and max

Aus_totals_by_state %>% 
  summarize(mean = mean(deaths, na.rm=TRUE), 
            median = median(deaths, na.rm=TRUE))

Aus_by_state <- 
  Aus_totals_by_state %>%
  group_by(Province_State) %>%
  summarise(deaths = max(deaths), cases = max(cases),
            recoveries = max(Recoveries))

# All the data points show that the number of deaths are significantly lesser than the number of recoveries
# As the cases have gone up, number of deaths has lowered with increase in number of recoveries

# Model the Data

mod <- lm(deaths ~ recoveries, data = Aus_by_state)
summary(mod)

Aus_by_state %>% slice_min(deaths)
Aus_by_state %>% slice_max(deaths)

x_grid <- seq(1, 100)
new_df <- tibble(recoveries = x_grid)
Aus_by_state %>% mutate(pred = predict(mod))

Aus_tot_w_pred <- Aus_by_state %>% mutate(pred = predict(mod))
Aus_tot_w_pred %>% ggplot() +
  scale_y_log10() +
  scale_x_log10() +
  geom_point(aes(x = recoveries, y = deaths), color = "blue") +
  geom_point(aes(x = recoveries, y = pred), color = "red")

# Conclusion

# The number of deaths have come down as the number of recoveries has increased