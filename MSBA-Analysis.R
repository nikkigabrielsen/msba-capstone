
## ------------------------------------------------------------- ##

                        # Library Calls #  
  
## ------------------------------------------------------------- ##
  
library(tidyverse)
library(DBI)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
  #group_rows is masked from dyplr#
library(dotwhisker)
#----#
library(jtools)
library(scales)

## ------------------------------------------------------------- ##

                            # Setup #

## ------------------------------------------------------------- ##

path.to.db <- paste0("C:\\Users\\Nikki\\Documents\\UM Spring 2021\\",
                     "Capstone\\msba-capstone\\")

# Creating the connection to the DB
con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(path.to.db,"northstar_athletics.db"))

# Listing the tables
dbListTables(con)

## ------------------------------------------------------------ ##

                     # Capstone Code #

## ------------------------------------------------------------ ##

# Creating connection to the tables

trans <- tbl(con, "transactions")
sku_f <- tbl(con, "sku_2021")
master <- tbl(con, "master")
vend_ym <- tbl(con, "vend_year_month")
cat_ym <- tbl(con, "cat_year_month")

dat <- master %>% 
  collect


dat %>% 
  mutate(quarter = quarter(dat$date)) %>% 
  filter(vendor != "FREQ" &  
           vendor != "KAHU" &
           vendor != "KHOM" &
           vendor != "LEAD" &
           vendor !=  "MBT" &
           vendor != "ONFI" &
           vendor != "SLID" &
           vendor != "SMIT" &
           vendor != "TAPO" &
           vendor != "UNDE" &
           vendor_desc != "NOT FOUND") %>% 
  group_by(quarter,year) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  ggplot() +
  theme_minimal() +
  geom_col(aes(x = year, y = total_sold, fill = quarter, width = 0.7)) +
  scale_y_continuous(name="Total Items Sold", labels = scales::comma) +
  xlab("Year") +
  ggtitle("Total Item Sales \n 2008-2020") +
  theme(plot.title = element_text(hjust=0.5)) +
  guides(fill = guide_legend(title = "Quarter"))
  
??ggtitle  

# Transactions descriptive stats

a <- dat %>% 
  select(ticket_num) %>% 
  group_by(ticket_num) 

n_distinct(aa)

# Vendor descriptive stats


b <- dat %>% 
  filter(vendor != "FREQ") %>% 
  select(vendor) %>% 
  group_by(vendor)

c <- dat %>% 
  filter(vendor != "FREQ" &  
           vendor != "KAHU" &
           vendor != "KHOM" &
           vendor != "LEAD" &
           vendor !=  "MBT" &
           vendor != "ONFI" &
           vendor != "SLID" &
           vendor != "SMIT" &
           vendor != "TAPO" &
           vendor != "UNDE" &
           vendor_desc != "NOT FOUND") %>% 
  select(vendor_desc) %>% 
  group_by(vendor_desc)


# Category descriptive statistics

c <- dat %>% 
  select(category) %>% 
  group_by(category)

n_distinct(c)


# Number of tickets

dat %>% 
  select(ticket_num) %>% 
  count()

# Number of SKUs

sku_f %>% 
  select(sku) %>% 
  count() 

# Histogram - Number of Items per Customer
# for.hist <- dat %>% 
#   group_by(cust) %>% 
#   summarize(Quan = sum(quan_sold), Sales = sum(net_sale)) %>% 
#   filter(Sales > 0, Sales < 3000) %>% 
#   collect

#Histogram - Number of Items per Ticket
for.hist2 <- dat %>% 
  group_by(ticket_num) %>% 
  summarize(Quan = sum(quan_sold)) %>%
  collect

for.hist2 %>% 
  as_tibble()

mean(for.hist2$Quan)
sd(for.hist2$Quan)

median(for.hist2$Quan)

#Display Tables
table(for.hist$Quan)
table(for.hist2$Quan)

#Display Histograms
hist(for.hist$Quan, 
     main = "Histogram of Total Items per Customer",
     xlab = "Items", 
     col = "hot pink")

hist(for.hist2$Quan, 
     main = "Histogram of Total Items per Ticket", 
     xlab = "Number of Items", 
     col = "hot pink")

#Descriptive Stats

for.stat <- dat %>% 
  group_by(ticket_num) %>% 
  summarize(Quan = sum(quan_sold)) %>% 
  collect

summary(for.stat)


### ------------------------------------------------------------ ###

                  ### Top & Bottom 10 ###

### ------------------------------------------------------------ ###


# Top 10 Categories

c_top10 <- dat %>% 
  select(category, category_desc, vendor, vendor_desc, quan_sold, quan_returned) %>% 
  group_by(category_desc)  %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  arrange(desc(total_sold)) %>% 
  head(10) 

c_top10 %>% 
  kable()

# Top 10 Vendors

V_top10 <- dat %>% 
  select(vendor, vendor_desc, quan_sold, quan_returned) %>% 
  group_by(vendor) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  arrange(desc(total_sold)) %>% 
  head(10)

v_top10 %>% 
  kable()

# Pass column values to vector 

top10 <- as.vector(V_top10$vendor)
ctop10 <- as.vector(c_top10$category_desc)

# Top 10 
dat %>% 
  select(vendor, vendor_desc, category_desc, quan_sold, quan_returned) %>%
  filter(vendor %in% top10, 
         category_desc %in% ctop10) %>% 
  group_by(category_desc, vendor_desc) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  ggplot() +
  theme_bw() +
  geom_col(aes(x = category_desc, y = total_sold, fill = vendor_desc, width = 0.7)) +
  scale_y_continuous(name="Total Items Sold", labels = scales::comma) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  xlab("Category") +
  ggtitle("[Insert Title]") +
  theme(plot.title = element_text(hjust=0.5)) +
  guides(fill = guide_legend(title = "Vendor"))

# Bottom 10


dat %>% 
  filter(vendor != "FREQ" &  
           vendor != "KAHU" &
           vendor != "KHOM" &
           vendor != "LEAD" &
           vendor !=  "MBT" &
           vendor != "ONFI" &
           vendor != "SLID" &
           vendor != "SMIT" &
           vendor != "TAPO" &
           vendor != "UNDE" &
           vendor_desc != "NOT FOUND") %>%
  group_by(vendor_desc) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  arrange(total_sold) %>% 
  head(10) %>% 
  kable()

bot10 <- dat %>% 
  filter(vendor != "FREQ" &  
           vendor != "KAHU" &
           vendor != "KHOM" &
           vendor != "LEAD" &
           vendor !=  "MBT" &
           vendor != "ONFI" &
           vendor != "SLID" &
           vendor != "SMIT" &
           vendor != "TAPO" &
           vendor != "UNDE" &
           vendor_desc != "NOT FOUND") %>%
  group_by(vendor_desc) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  arrange(total_sold) %>% 
  head(10) 


  # Left off attempting to determine top and bottom 10 items by vendor for each category #

dat %>% 
  select(vendor, vendor_desc, category, category_desc, quan_sold, quan_returned) %>% 
  filter(vendor != "FREQ" &  
           vendor != "KAHU" &
           vendor != "KHOM" &
           vendor != "LEAD" &
           vendor !=  "MBT" &
           vendor != "ONFI" &
           vendor != "SLID" &
           vendor != "SMIT" &
           vendor != "TAPO" &
           vendor != "UNDE" &
           vendor_desc != "NOT FOUND") %>% 
  group_by(category_desc, vendor_desc) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  arrange(total_sold) %>% 
  head(10) %>% 
  kable()


### ------------------------------------------------------------ ###

                   ### Split into periods ###

### ------------------------------------------------------------ ###

first <- dat %>% 
  filter(month %in% c("01","02","03"))

second <- dat %>% 
  filter(month %in% c("04","05","06"))

third <- dat %>% 
  filter(month %in% c("07","08","09"))

fourth <- dat %>% 
  filter(month %in% c("10","11","12"))


## Attempt at descriptive stats by period

dat %>% 
  group_by(date) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  ggplot(aes(x = date, y = total_sold)) +
  geom_line()

## Attempt at a linear model
## Top 10 vendors


lm_dat <- dat %>%
  select(date, month, vendor, vendor_desc, quan_sold, quan_returned) %>% 
  mutate(quarter = case_when(month %in% c("01","02","03") ~ "quarter_1",
                             month %in% c("04","05","06") ~ "quarter_2",
                             month %in% c("07","08","09") ~ "quarter_3",
                             month %in% c("10","11","12") ~ "quarter_4")) %>% 
  #this will be vendor %in% top10 ideally
  filter(dat$year %in% c("2016","2017","2018","2019","2020"), 
         vendor == "ADID" |
           vendor == "DC" |
           vendor == "KEEN" |
           vendor == "NIKE" |
           vendor == "ASIC" |
           vendor == "ZEPH" |
           vendor == "REEB" |
           vendor == "MIZU" |
           vendor == "NEWE" |
           vendor == "TENS") %>%
  group_by(date,quarter,vendor) %>% 
  summarise(total_sold = sum(quan_sold) - sum(quan_returned)) %>% 
  ungroup()
  
  
  
lm_1 <- lm(formula = total_sold ~ quarter * vendor, data = lm_dat)

lm_2 <- lm(formula = quan_sold ~ (quarter + vendor)^2, data = lm_dat)


summary(lm_1) 
summary(lm_2)

lm_2 %>% 
  ggplot()

## Attempt at ANOVA

anova(lm_2, test="Chisq")

arm::display(lm_1)

two.way <- aov(total_sold ~ quarter * vendor, data = lm_dat)

summary(two.way)

mydata.cor <- lm_dat %>% 
  cor(quarter, vendor_desc)

?cor()
