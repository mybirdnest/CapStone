
library(tidyverse)
library(caret)
library(broom)

# Read the data from ./DATA folder
sales_win_loss <- read_csv("/repos/CapStone/DATA/WA_Fn-UseC_-Sales-Win-Loss.csv")


# Rename the long columns name to make it easier
colnames(sales_win_loss) <- c("ID","SuppliesSubgroup","SuppliesGroup","Region", "Route",
                              "ElapsedDays", "Result","SalesStageCount",
                              "TotalDaysClosing","TotalDaysQualified",
                              "Opportunity","ClientSizeRev","ClientSizeCount",
                              "Revenue","Competitor","RDaysIdentified",
                              "RDaysValidated","RDaysQualified",
                              "DealSize")

# Check for missing values
map_dbl(sales_win_loss, ~sum(is.na(.)))

# Note: No Missing values are identified from the data

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
as_tibble(cbind(c(var, var_type, var_descriptions)))
as_data_frame(cbind(c(1:length(var)), var, var_type, var_descriptions))
as_tibble(sales_win_loss)


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

sales_win_loss <- sales_win_loss %>%
  mutate(Result2 = case_when(
    Result == "Won" ~ 1,
    Result == "Loss" ~ 0))

glimpse(sales_win_loss)


# Create modeling data
# Select the data to be used in modeling and assign to ModelData, then convert 
# the categorical information to dummy variables to help with the modelling. 


set.seed(3456)
trainIndex <- caret::createDataPartition(sales_win_loss$Result, p = .75, 
                                         list = FALSE, 
                                         times = 1)
head(trainIndex)

training <- sales_win_loss[ trainIndex,]
testing  <- sales_win_loss[-trainIndex,]

training <- data.frame(training)
testing <- data.frame(testing)

# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummy_model <- caret::dummyVars(Result ~ Region + Route + SuppliesGroup + TotalDaysQualified + Competitor, data = training)

# Create the dummy variables using predict. The Y variable (Result) will not be present in trainData_mat.
trainData_mat <- predict(dummy_model, newdata = training)

# # Convert to dataframe
trainData <- data.frame(trainData_mat)

# # See the structure of the new dataset
str(training)

# As can be seen from the new training dataset, the categorical variables are now translated to numbers


# Set up model parameters
control <- caret::trainControl(method = "cv", number = 2, classProbs = TRUE)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- 3
tunegrid <- expand.grid(.mtry = mtry)


# Logistic Model
glm_model <- caret::train(
  Result ~ Region + Route + SuppliesGroup + TotalDaysQualified + Competitor,
  data = training,
  method = "glm",
  trControl = control
)

summary(glm_model)

pred_glm <- predict(glm_model, newdata = testing, type = "prob")

summary(pred_glm)

tapply(pred_glm, training$Result, mean)


conf_mat_glm <- caret::confusionMatrix(pred_glm, testing$Result, positive = "Won")

conf_mat_glm
conf_mat_glm$byClass["F1"]




