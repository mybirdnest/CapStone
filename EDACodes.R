
library(tidyverse)
library(caret)
library(broom)

# Read the data from ./DATA folder
sales_win_loss <- read_csv("/repos/CapStone/DATA/WA_Fn-UseC_-Sales-Win-Loss.csv")

#sales_win_loss <- read_csv("DATA/WA_Fn-UseC_-Sales-Win-Loss.csv")

# Review the data in general
glimpse(sales_win_loss, give.attr = FALSE)

head(sales_win_loss[, 1:6])
head(sales_win_loss[, 7:13])
head(sales_win_loss[, 14:19])

# Check for missing values
map_dbl(sales_win_loss, ~sum(is.na(.)))

# Conclusion: No Missing values are identified from the data

# Set Standard chart theme
theme_set(theme_minimal() + theme(legend.position = "bottom"))

# Rename the long columns name to make it easier
colnames(sales_win_loss)<- c("ID","SuppliesSubgroup","SuppliesGroup","Region", "Route",
                             "ElapsedDays", "Result","SalesStageCount",
                             "TotalDaysClosing","TotalDaysQualified",
                             "Opportunity","ClientSizeRev","ClientSizeCount",
                             "Revenue","Competitor","RDaysIdentified",
                             "RDaysValidated","RDaysQualified",
                             "DealSize")


# Data Dictionary
var_descriptions <- c(
  "A random number assigned to the opportunity",
  "Supplies Subgroup",
  "Supplies Group",
  "Region",
  "Route to market",
  "The number of days between the change in sales stages",
  "A closed opportunity. Values is either won or loss",
  "A count of number of times an opportunity changes sales stages",
  "Total days from Identified to Gained Agreement/closing",
  "Total days from Identified to Qualified Agreement",
  "Sum of line item revenue estimates",
  "Client size based on annual revenue",
  "Client size based on number of employees",
  "Revenue from client the past two years",
  "An indicator whether or not competitor has been identified",
  "Ratio of Identified/Validating over total days",
  "Ratio of Qualified/Gaining Agreement over total days",
  "Ratio of Validated/Qualifying over total days",
  "Categorical grouping of the opportunity amount"
)

var <- colnames(sales_win_loss)
var_type <- unlist(map(sales_win_loss, class))
as_data_frame(cbind(c(1:length(var)), var, var_type, var_descriptions))


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


# 1. Stacked bar chart: Result vs Number of sales leads last two years
position <- c("Rev=$0", "$1<=Rev<$50K", "$50K<=Rev<$400K", "$400K<=Rev<$1.5M", "Rev>=$1.5M")
ggplot(sales_win_loss, aes(x = Revenue2, fill = Result)) + 
  geom_bar() + 
  scale_x_discrete(limits = position) + 
  xlab("Revenue") + 
  ylab("Number of records") + 
  ggtitle("Result vs Number of sales leads for the last two years") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: A lot sales leads opportunity that resulted in $0 revenue for the last two years that
# the sales team can learn or put more effort


# 2. Cluster bar chart: Revenue vs Result last two years
position <- c("Rev>=$1.5M","$400K<=Rev<$1.5M","$50K<=Rev<$400K","$1<=Rev<$50K","Rev=$0")
ggplot(sales_win_loss) + 
  geom_bar(aes(x = Revenue2, fill = Result), position = "fill") +
  scale_x_discrete(limits = position) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  coord_flip() + 
  ggtitle("Revenue vs Result last two years") + 
  xlab("Revenue") + 
  ylab("Percent(%)") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: I can see that the probability of loss opportunity is higher if customer didn't buy anything 
# in the last two years. If client purchase in the last two years, the chance of win decreases as sales deals rises


# 3. Bar chart: follow up from no 2, Compare by Region, Route, and Result 
sales_win_loss %>% 
  group_by(Region, Route) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Region, y = SumResult, fill = Route)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Market Opportunity by Region and Route last two years") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# sales opportunity across all regions


# 4. Bar chart: Opportunity amount compare by Region and Result
sales_win_loss %>% 
  group_by(Region, Result) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Region, y = SumResult, fill = Result)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$"))

# reorder the chart based on max SumResult, Y axis label make it $100M - $2B??
# Conclusion: Plenty of loss $ opportunity across all regions especially in Midwest and Pacific region.
# Loss $ opportunity in Midwest and Pacific is ~1.3B each.  These two regions also contribute the biggest $ win
# in the range of $300M - $350M.
# Other region also show loss $ opportunity in the range of $500M - $700M.  
# The sales team on each region need to get busy and understand the loss oppportunity. The following charts
# will explore loss opportunity for each region by route


# 5. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Mid-Atlantic and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Mid-Atlantic") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Mid-Atlantic by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# $ Opportunity loss. In the Mid-Atlantic region, in field sales, "Shelters & RV" and "Batteries & 
# Accessories", and "Exterior Accessories" are the main areas to focus.  
# The sales team in this region should focus their sales effort on these three buckets.


# 6. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Midwest and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Midwest") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Midwest by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 


# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# $ Opportunity loss. In the Midwest region, in field sales, "Shelters & RV" and "Batteries & 
# Accessories", and "Exterior Accessories" are the main areas to focus.  
# The sales team in this region should focus their sales effort on these three buckets.


# 7. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Northeast and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Northeast") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Northeast by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# $ Opportunity loss. In the Northeast region, in field sales, "Shelters & RV" and "Batteries & 
# Accessories", and "Exterior Accessories" are the main areas to focus.  
# The sales team in this region should focus their sales effort on these three buckets.


# 8. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Northwest and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Northwest") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Northwest by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# $ Opportunity loss. In the Northwest region, in field sales, "Shelters & RV" and "Batteries & 
# Accessories", and "Exterior Accessories" are the main areas to focus.  
# The sales team in this region should focus their sales effort on these three buckets.


# 9. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Pacific and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Pacific") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Pacific by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Quite similar to Pacific, Field sales and reseller are the biggest sales chanel that 
# contribute to a lot of $ Opprotunity loss. However, the "Other" bucket is also something to pay attention to
# We didn't see this in the other regions as an issue. 
# Looking at the SuppliesSubgroup, in field sales, "Shelters & RV", "Batteries & Accessories", and
# "Exterior Accessories" are the three big main areas to focus.  


# 10. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Southeast and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Southeast") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Southeast by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# $ Opportunity loss. In the Southeast region, in field sales, "Shelters & RV" and "Batteries & 
# Accessories", and "Exterior Accessories" are the main areas to focus.  
# The sales team in this region should focus their sales effort on these three buckets.


# 11. Bar chart: follow up from no 2, Compare by Route and Result 
# where Region = Southwest and Result = Loss
sales_win_loss %>% filter(Result == "Loss" & Region == "Southwest") %>%
  group_by(Route, SuppliesSubgroup) %>% 
  summarise(SumResult = sum(Opportunity)) %>%
  ggplot(aes(x = Route, y = SumResult, fill = SuppliesSubgroup)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,1e+11, 1e+08), labels = scales::dollar_format(prefix = "$")) +
  ggtitle("Opportunity Loss in Southwest by SuppliesSubgroup and Route") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: Field sales and reseller are the two biggest sales chanel that contribute to a lot of
# $ Opportunity loss. In the Southwest region, in field sales, "Shelters & RV" and "Batteries & 
# Accessories", and "Exterior Accessories" are the main areas to focus.  
# The sales team in this region should focus their sales effort on these three buckets.


# 12. Scater chart: Avg Total Days Qualified vs Avg Opp for SuppliesSubgroup
sales_win_loss %>% 
  group_by(Result, SuppliesSubgroup) %>% 
  summarise(AvgOpp = mean(Opportunity), AvgQual = mean(TotalDaysQualified )) %>%
  ggplot(aes(x = AvgQual, y = AvgOpp, shape = factor(Result), label = SuppliesSubgroup)) + 
  geom_point(aes(colour = factor(Result)), size = 4) +
  geom_point(colour = "grey90", size = 1.5) + xlab("AvgTotalDaysQualified") + ylab("AvgOpp(US$)") +
  scale_x_continuous(breaks = seq(0, 30, 5)) + 
  scale_y_continuous(breaks = seq(40000, 175000, 12500), labels = scales::dollar_format(prefix = "$")) +
  geom_text(aes(label = SuppliesSubgroup,  color = Result), size = 3) + 
  stat_ellipse(aes(color = Result), type = "t") + 
  ggtitle("Avg Total Days Qualified vs Avg Opp for SuppliesSubgroup") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Conclusion: The longer the lead stays in the pipeline longer than 12 days, the higher the probability
# the company will lose the deal.


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

