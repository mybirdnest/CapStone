
library(tidyverse)
library(caret)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

# Read the data from ./DATA folder
#sales_win_loss <- read_csv("/repos/CapStone/DATA/WA_Fn-UseC_-Sales-Win-Loss.csv")

sales_win_loss <- read_csv("DATA/WA_Fn-UseC_-Sales-Win-Loss.csv")


# Review the data in general
glimpse(sales_win_loss, give.attr = FALSE)

head(sales_win_loss[, 1:6])
head(sales_win_loss[, 7:13])
head(sales_win_loss[, 14:19])

# Check for missing values
map_dbl(sales_win_loss, ~sum(is.na(.)))

# Conclusion: No Missing values are identified from the data

# Rename the long columns name to make it easier
colnames(sales_win_loss)<- c("ID","SuppliesSubgroup","SuppliesGroup","Region", "Route",
                             "ElapsedDays", "Result","SalesStageCount",
                             "TotalDaysClosing","TotalDaysQualified",
                             "Opportunity","ClientSizeRev","ClientSizeCount",
                             "Revenue","Competitor","RDaysIdentified",
                             "RDaysValidated","RDaysQualified",
                             "DealSize")

# Adding additional columns to translate category column into meaningful column for data visualization
sales_win_loss <- sales_win_loss %>% 
  mutate(ClientSizeRev2 = case_when(
    ClientSizeRev == 1 ~ "ClientRev<$1M",
    ClientSizeRev == 2 ~ "$1M<=ClientRev<$10M",
    ClientSizeRev == 3 ~ "$10M<=ClientRev<$50M",
    ClientSizeRev == 4 ~ "$50M<=ClientRev<$100M",
    ClientSizeRev == 5 ~ "ClientRev>=$100M"))

sales_win_loss <- sales_win_loss %>% 
  mutate(ClientSizeCount2 = case_when(
    ClientSizeCount == 1 ~ "Count<1K",
    ClientSizeCount == 2 ~ "1K<=Count<5K",
    ClientSizeCount == 3 ~ "5K<=Count<10K",
    ClientSizeCount == 4 ~ "10K<=Count<30K",
    ClientSizeCount == 5 ~ "Count>=30K"))

sales_win_loss <- sales_win_loss %>% 
  mutate(Revenue2 = case_when(
    Revenue == 0 ~ "Rev=$0",
    Revenue == 1 ~ "$1<=Rev<$50K",
    Revenue == 2 ~ "$50K<=Rev<$400K",
    Revenue == 3 ~ "$400K<=Rev<$1.5M",
    Revenue == 4 ~ "Rev>=$1.5M"))

# Cluster bar chart: Revenue vs Result last two years
position <- c("Rev>=$1.5M","$400K<=Rev<$1.5M","$50K<=Rev<$400K","$1<=Rev<$50K","Rev=$0")
ggplot(sales_win_loss) + geom_bar(aes(x = Revenue2, fill = Result), position = "fill") +
  scale_x_discrete(limits = position) + scale_y_continuous(labels = scales::percent_format()) + 
  coord_flip() + ggtitle("Revenue vs Result last two years") + xlab("Revenue") + ylab("Percent(%)") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: I can see that the probability of loss opportunity is higher if customer didn't buy anything 
# in the last two years. If client purchase in the last two years, the chance of win decreases as sales deals rises


# Cluster bar chart: Opportunity amount compare by Region and Result

sales_win_loss %>% 
  group_by(Region, Result) %>% 
  summarise(SumResult = Sum(Opportunity))

ggplot(sales_win_loss) + geom_bar(aes(x = Region, Y = SumResult, fill = Result), stat = "identity")




# Cluster bar chart: Opportunity amount compare by Supplies Group and Result
ggplot(sales_win_loss) + geom_bar(aes(x = SuppliesGroup, Y = Opportunity, fill = Result), stat = "identity")


# Cluster bar chart: Opportunity amount compare by Region and Result
ggplot(sales_win_loss) + geom_bar(aes(x = Region, fill = Result), stat = "identity")




#x <- ggpairs(sales_win_loss)

# pick one convention - camel case or snake case
# try to make them smaller 
# look at function rename in dplyr
# look at function glimpse - nicer str
# look at all the independent variables, color by dependent variables, change something change color first
# look at tool dependable variables 
# try to come up with one recommendation from the EDA 
# Use RMarkdown
# Add some comments on the script or analysis. 
# Add comment for each chart you show


glimpse(sales_win_loss2)

sales_win_loss2 <-
  sales_win_loss2 %>%
  mutate(loss = if_else(Opportunity_Result == "Loss",1,0))

sales_loss <- mean(sales_win_loss2$loss)
sales_loss



