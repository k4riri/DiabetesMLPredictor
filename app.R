#IMPORTING THE DATA
diabetes_data <- read.csv("DATASET.csv")
head(diabetes_data)
attach(diabetes_data)

#LOADING NECESSARY LIBRARIES 
library(caret)
library(glmnet)
library(randomForest)
library(pROC)
library(e1071)
library(dplyr)
library(ROSE)
library(kernlab)

#SAMPLING THE DATA
set.seed(123)
sample_size <- 500
sample_indices <- sample(nrow(diabetes_data), sample_size, replace = FALSE)
sampled_data <- diabetes_data[sample_indices, ]
head(sampled_data)
attach(sampled_data)

data_upsampled <- ROSE::ovun.sample(diabetes ~ ., data = sampled_data, method = "over", seed = 123)$data
table(data_upsampled$diabetes)
attach(data_upsampled)
head(data_upsampled)

#RECURSIVE FEATURE ELIMINATION
dataA<-data.frame(age,bmi,HbA1c_level,blood_glucose_level)
head(dataA)
total_observations <- nrow(dataA)
print(total_observations)

minmaxnorm<-function(x){(x-min(x))/(max(x)-min(x))}
datanorm<-as.data.frame(lapply(dataA[1:4],minmaxnorm))
head(datanorm)

dataY<-data.frame(diabetes,datanorm,smoking_history,hypertension,heart_disease)
head(dataY)
nrow(dataY)

dataY$smoking_history <- as.factor(dataY$smoking_history)
dataY$hypertension <- as.factor(dataY$hypertension)
dataY$heart_disease <- as.factor(dataY$heart_disease)
dataY$diabetes <- as.factor(dataY$diabetes)	

predictors <- c("age", "bmi", "HbA1c_level", "blood_glucose_level", "smoking_history", "hypertension", "heart_disease")
target <- "diabetes"

formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 25)

rfe_result <- rfe(x = dataY[, predictors], y = dataY$diabetes, sizes = 1:(length(predictors) - 1), rfeControl = ctrl)
print(rfe_result)

#SVM MODEL 
splitIndex <- createDataPartition(data_upsampled$diabetes, p = 0.7, list = FALSE)
head(splitIndex)
nrow(splitIndex)

training_data <- data_upsampled[splitIndex, ]
head(training_data)
nrow(training_data)
attach(training_data)



training_data1<-data.frame(age,bmi,HbA1c_level,blood_glucose_level)
nrow(training_data1)


minmaxnorm2<-function(x){(x-min(x))/(max(x)-min(x))}
training_datanorm<-as.data.frame(lapply(training_data1[1:4],minmaxnorm2))
nrow(training_datanorm)
head(training_datanorm)

TRAINING_DATA_NORM<-data.frame(diabetes,training_datanorm)
nrow(TRAINING_DATA_NORM)
TRAINING_DATA_NORM$diabetes <- as.factor(TRAINING_DATA_NORM$diabetes)	



test_data <- data_upsampled[-splitIndex, ]
head(test_data)
nrow(test_data)
attach(test_data)

test_data1<-data.frame(age,bmi,HbA1c_level,blood_glucose_level)
nrow(test_data1)


minmaxnorm2<-function(x){(x-min(x))/(max(x)-min(x))}
test_datanorm<-as.data.frame(lapply(test_data1[1:4],minmaxnorm2))
nrow(test_datanorm)
head(test_datanorm)


TEST_DATA_NORM<-data.frame(diabetes,test_datanorm)
nrow(TEST_DATA_NORM)
TEST_DATA_NORM$diabetes <- as.factor(TEST_DATA_NORM$diabetes)	


svm_model <- svm(diabetes ~ ., data = TRAINING_DATA_NORM, kernel = "radial")
print(svm_model)

predictions <- predict(svm_model, newdata = TEST_DATA_NORM)


conf_matrix <- confusionMatrix(predictions, TEST_DATA_NORM$diabetes)
print(conf_matrix)


svm_tuned <- train(diabetes ~ ., data = TRAINING_DATA_NORM, method = "svmRadial",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(C = c(0.1, 1, 10), sigma = c(0.1, 1, 10)))


print(svm_tuned$bestTune)
tuned_predictions <- predict(svm_tuned, newdata = TEST_DATA_NORM)
tuned_conf_matrix <- confusionMatrix(tuned_predictions, TEST_DATA_NORM$diabetes)
print(tuned_conf_matrix)



#SAVING THE TUNED SVM MODEL
saveRDS(svm_tuned, "tuned_svm_model.rds")


#LOGISTIC REGRESSION MODEL
logistic_model <- train(diabetes ~ ., data = TRAINING_DATA_NORM, method = "glm", family = "binomial")
predictions <- predict(logistic_model, newdata = TEST_DATA_NORM)
conf_matrix <- confusionMatrix(predictions, TEST_DATA_NORM$diabetes)
print(conf_matrix)



#TESTING THE PREDICTION OFF ONE PERSONS DATA
new_data <- data.frame(
  age = 78,
  bmi = 17.74,
  HbA1c_level = 8.8,
  blood_glucose_level = 159
)



# Normalize each variable separately using its specific min and max values
normalized_age <- (new_data$age - 0.64) / (80- 0.64)
normalized_bmi <- (new_data$bmi - 17.30) / (63.48 - 17.30)
normalized_HbA1c_level <- (new_data$HbA1c_level - 3.50) / (9.00 - 3.50)
normalized_blood_glucose_level <- (new_data$blood_glucose_level - 80.00) / (300 - 80)

# Combine the normalized values into a new data frame
normalized_new_dataA <- data.frame(
  age = normalized_age,
  bmi = normalized_bmi,
  HbA1c_level = normalized_HbA1c_level,
  blood_glucose_level = normalized_blood_glucose_level
)

print(normalized_new_dataA)


loaded_svm_model <- readRDS("tuned_svm_model.rds")
new_predictions <- predict(loaded_svm_model, normalized_new_dataA)
print(new_predictions)


library(shiny)
# Define the min and max values for normalization
min_age <- 0
max_age <- 100
min_bmi <- 10 
max_bmi <- 40 
min_HbA1c <- 4 
max_HbA1c <- 10 
min_glucose <- 70 
max_glucose <- 200 

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #f7f7f7; 
        color: #333333; 
      }
      headerPanel {
        text-align: center;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
      .sidebar {
        background-color: #f8f9fa;
      }
      .main-panel {
        background-color: #ffffff;
      }
      h2, h3 {
        color: #ff6600;
      }
      .logo {
        width: 150px; /* Set the width */
        height: auto; /* Maintain aspect ratio */
        margin-bottom: 20px;
        text-align: center;
      }
    "))
  ),
  pageWithSidebar(
    headerPanel('Welcome to Support Vector Machine Model DiaDetect'),
    sidebarPanel(
      tags$img(src = "https://res.cloudinary.com/dpoqx18xw/image/upload/v1700733775/diabetes_nsikcz.png", class = "logo"),
      tags$label(h1('DIA DETECT')),
      tags$br(),
      tags$label(h3('Input Information')),
      numericInput("age", "Age", value = 0, min = 0),
      numericInput("bmi", "BMI", value = 0, min = 0),
      numericInput("HbA1c_level", "HbA1c Level", value = 0, min = 0),
      numericInput("blood_glucose_level", "Blood Glucose Level", value = 0, min = 0),
      actionButton("submitbutton", "Submit", class = "btn btn-primary")
    ),
    mainPanel(
      tags$label(h2('Status/Output')),
      verbatimTextOutput('contents'),
      tableOutput('tabledata')
    )
  )
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    df <- data.frame(
      age = as.numeric(input$age),
      bmi = as.numeric(input$bmi),
      HbA1c_level = as.numeric(input$HbA1c_level),
      blood_glucose_level = as.numeric(input$blood_glucose_level)
    )
    
    # Ensure normalization parameters are correctly set based on your dataset
    normalized_df <- data.frame(
      age = (df$age - min_age) / (max_age - min_age),
      bmi = (df$bmi - min_bmi) / (max_bmi - min_bmi),
      HbA1c_level = (df$HbA1c_level - min_HbA1c) / (max_HbA1c - min_HbA1c),
      blood_glucose_level = (df$blood_glucose_level - min_glucose) / (max_glucose - min_glucose)
    )
    
    predict(loaded_svm_model, normalized_df)
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      result <- isolate(datasetInput())
      if (result == 0) {
        "Low probability of having diabetes."
      } else if (result == 1) {
        "High probability of having diabetes."
      } else {
        "Invalid prediction value."
      }
    } else {
      "Server is ready for calculation."
    }
  })
  
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
}

shinyApp(ui = ui, server = server)
