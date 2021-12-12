library(tidyverse)
library(readxl)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
#Load the main WHO data
who_url = "https://apps.who.int//nha//database//Home//IndicatorsDownload//en"
who_download<- download.file(url = who_url, destfile = "data_Who.xlsx", mode="wb")
master_data <- read_excel("data_Who.xlsx")
#Keep the relevant data
newdata <- master_data %>% select(country=country,
                                  year=year,
                                  income_group=`income group (2018)`,
                                  region = `region (WHO)`,phc_che = phc_che,
                                  phc_usd_pc = phc_usd_pc,
                                  gdp = gdp,
                                  pop = pop,
                                  che_gdp = che_gdp,
                                  che_pc_usd = che_pc_usd,
                                  vhi_che = vhi_che,
                                  oops_che = oops_che)
newdata
# Financial impact of such schemes on individuals
master_df <- filter(newdata, income_group == "Low" | income_group == "Low-Mid")

# trends and observations in the out of pocket expenses by an individual


master_df_region <- master_df %>%
  group_by(region) %>%
  summarise(mean_vhi = mean(vhi_che, na.omit = TRUE),
            mean_oops = mean(oops_che, na.omit = TRUE))

View(master_df1)
# Low income and low mid - Europe
# Clearly individuals in European countries have more out of pocket expenses, and so they are not making use of the voluntary health insurance
# they are not only offered to the individuals under govt jobs but also are additional benefits other than health expenses provided by the govt

master_df_country <- master_df %>%
                  group_by(country) %>%
                  summarise(mean_vhi = mean(vhi_che, na.omit = TRUE),
                  mean_oops = mean(oops_che, na.omit = TRUE))

# it is visible also at the country level

# Visuals to cross verify the numbers

voluntary_expense_trend <- ggplot(data = master_df, aes(x=year, y=vhi_che))+
  geom_bar(stat="identity",(aes(fill=region))) +
  ggtitle("Year over Year trend of the Voluntary Health Expenditure") +
  xlab("year") +
  ylab("VHI in % as Current Expenditure")
voluntary_expense_trend

out_of_expense_trend <- ggplot(data = master_df, aes(x=year, y=oops_che))+
  geom_bar(stat="identity",(aes(fill=region))) +
  ggtitle("Year over Year trend of the Out-of-Pocket Expenditure") +
  xlab("year") +
  ylab("OOPS in % as Current Expenditure")
out_of_expense_trend

ggplot(master_df)+
  geom_point(aes(x=year, y = oops_che) +
               geom_point(aes(color=Province_State))


q <- ggplot(newdata, aes(x=factor(income_group),
                              y=oops_che,color=factor(income_group)))
q + geom_boxplot() +
  labs(title=" Comparison out of pocket by income group",
       x="income group", y="Out of Pocket Expenditure") 

p <- ggplot(newdata, aes(x=factor(income_group),
                           y=oops_che,color=factor(income_group)))
p + geom_boxplot() +
  labs(title=" boxplots to compare out based on income group",
       x="income group", y="Out of Pocket Expenditure") 