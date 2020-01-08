library(tidyverse)
library(DBI)
library(dplyr)
library(ggplot2)
library(jtools)
library(scales)

path.to.db <- paste0("C:\\Users\\Nikki\\Documents\\UM Fall 2019\\",
                     "BMKT 670 - Applied Data Analytics\\applied-data-analytics\\")

# Creating the connection to the DB
con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(path.to.db,"northstar_athletics.db"))

# Listing the tables
dbListTables(con)

# Creating connections to the table 
## this one is for year_month
cust.ym <- tbl(con, "cust_year_month")
## this one is for total 
cust.t <- tbl(con, "cust_year_month")


cust.t %>%
  group_by(Customer)

# Preparing data for histogram of total spending by customers
for.hist <- cust.t %>%
  group_by(Customer) %>%
  summarize(Sales = sum(Sales)) %>%
  filter(Sales > 0, Sales < 3000) %>%
  collect
                               
# Creating the histogram
hist(for.hist$Sales, main = "Histogram of Total Customer Spending < $3,000", xlab = "Spend ($)", col = "gray", breaks = (seq(0,3000,100)),xaxt="n",yaxt="n")

#Formatting the axes
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

axis(side=2, at=axTicks(2), 
     labels=formatC(axTicks(2), format="d", big.mark=',')) 

summary(for.hist$Sales)

# Preparing data for regression analysis
cust.ym %>% 
  group_by(Customer,Year) %>% 
  summarize(Months=n_distinct(Month)) %>%
  ungroup

for.model <- cust.ym %>% 
  group_by(Customer,Year) %>% 
  summarize(Sales = sum(Sales)) %>%
  collect

for.model <- 
  spread(for.model,key=Year,value=Sales) %>%
  rename(year_2008 = `2008`, 
         year_2009 = `2009`,
         year_2010 = `2010`,
         year_2011 = `2011`,
         year_2012 = `2012`,
         year_2013 = `2013`,
         year_2014 = `2014`,
         year_2015 = `2015`,
         year_2016 = `2016`,
         year_2017 = `2017`,
         year_2018 = `2018`)

for.model <-  for.model %>% 
  replace(is.na(.), 0)

#### removed avg_2015_2008 from model because it was not significant
for.model <- for.model %>%
  mutate(avg_2015_2008 = mean(c(year_2008,year_2009,year_2010,year_2011,year_2012,year_2013,year_2014,year_2015),na.rm=T))
####

# Create model for 2018 spend
lm <- 
  lm(year_2018 ~ year_2017 + year_2016,
     data=for.model)


summary(lm)

#Filter outliers for scatterplots
for.plot <- for.model %>%
  filter(Customer != "EBAY -", Customer != "RICK -")

#Set the theme for the scatterplots
set_theme(
  geom.outline.color = "antiquewhite4", 
  base = theme_bw(),
  title.size = 1.5, 
)

# Confirm correct coefficients
lm$coefficients

# Create scatterplot for 2016 vs 2018 spend
ggplot(for.plot, aes(x = year_2016, y = year_2018)) + 
  geom_point(alpha = 0.1) +
  geom_abline(slope = lm$coefficients[3], intercept = lm$coefficients[1], col = "darkblue") +  
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title= "Customer Spend - 2016 vs 2018", x = "2016 Spend", y = "2018 Spend")


# Create scatterplot for 2017 vs 2018 spend
ggplot(for.plot, aes(x = year_2017, y = year_2018)) + 
  geom_point(alpha = 0.1) +
  geom_abline(slope = lm$coefficients[2], intercept = lm$coefficients[1], col = "darkblue") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title= "Customer Spend - 2017 vs 2018", x = "2017 Spend", y = "2018 Spend")



# Plot coefficients and their error bars
plot_coefs(lm, scale = TRUE) + 
  scale_y_discrete(labels = c('2016','2017')) +
  labs(x = "Estimated Coefficient") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Coefficients from Spend 2018 Model")



