
library(tidyverse)
library(caret)
library(broom)

# Read the data from ./DATA folder
sales_win_loss <- read_csv("DATA/WA_Fn-UseC_-Sales-Win-Loss.csv")


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

# saveRDS

# Create modeling data
# Select the data to be used in modeling. Set Training, Validation, and Testing data

set.seed(3456)
TrainingValidationIndex <- caret::createDataPartition(sales_win_loss$Result, p = .80, 
                                                      list = FALSE, 
                                                      times = 1)
str(TrainingValidationIndex)


TrainingValidation <- sales_win_loss[ TrainingValidationIndex,]
TrainingIndex <- caret::createDataPartition(TrainingValidation$Result, p = .75, 
                                            list = FALSE, 
                                            times = 1)

Training <- TrainingValidation[TrainingIndex,]
Validation <- TrainingValidation[-TrainingIndex,]
Testing  <- sales_win_loss[-TrainingValidationIndex,]

glimpse(Training)

str(Training)


# Set up model parameters
control <- caret::trainControl(method = "cv", number = 2, classProbs = TRUE)
seed <- 7
metric <- "Accuracy"
set.seed(seed)


# Training Logistic model using Training dataset
GLMModel <- caret::train(
  Result ~ .,
  data = Training,
  method = "glm",
  trControl = control
)

summary(GLMModel)


# Training Random Forest model using Training dataset
RFModel <- caret::train(
  Result ~ .,
  data = Training,
  method = "rf",
  trControl = control
)

glimpse(RFModel)


# Prediction model using validation dataset and GLMModel
PredGLM <- predict(GLMModel, Validation)

ConfMatGLM <- caret::confusionMatrix(
  PredGLM, factor(Validation$Result), positive = "Won", mode = "everything")

ConfMatGLM


# Prediction model using validation dataset and RFModel
PredRF <- predict(RFModel, Validation)

ConfMatRF <- caret:: confusionMatrix(PredRF, factor(Validation$Result), positive = "Won", mode = "everything")

ConfMatRF



# Final_prediction model using GLMMOdel
FinalPredGLM <- predict(GLMModel, Testing)

ConfMatFinalGLM <- caret::confusionMatrix(FinalPredGLM, factor(Testing$Result), positive = "Won", mode = "everything")

ConfMatFinalGLM



# Final_prediction model using RFMOdel
FinalPredRF <- predict(RFModel, Testing)

ConfMatFinalRF <- caret::confusionMatrix(FinalPredRF, factor(Testing$Result), positive = "Won", mode = "everything")

ConfMatFinalRF



