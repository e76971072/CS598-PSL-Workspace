# install necessary libraries for the entire document
library(glmnet)
library(xgboost)
library(dplyr)
library(caret)

DATA_TYPE.TRAIN = "train"
DATA_TYPE.TEST = "test"

#' @func: preprocessing_data
#' @param: data: The data frame to be preprocessed.
#' @notes: This function removes unwanted variables, winsorizes specific numeric columns,
#'           and transforms categorical variables in the data.
#' @returns: A preprocessed data frame.
preprocessing_data = function (data, data_type) {

  data_after_remove = variables_to_remove(data)
  data_after_transform = categorical_variables_transform(data_after_remove, data_type)
  dataset = winsorize(data_after_transform)
  return (dataset)
}

#' @func clean_training_data
#' @param : unfilter train_data
#' @Note: data.x should return without "PID" and "Sale_Price"
#' @return: train data
#' sample R code provided by Prof. Liang on campuswire #327
categorical_variables_transform = function(data, data_type) {
  data.x = data

  # Remove Sale_Price from features, but keep PID
  if (data_type == DATA_TYPE.TRAIN && 'Sale_Price' %in% colnames(data.x)) {
    data.x = data.x %>% select(-Sale_Price)
  }

  # Log-transform Sale_Price and store original values for training data only
  if (data_type == DATA_TYPE.TRAIN) {
    data.y = ifelse(!is.na(data$Sale_Price), log(data$Sale_Price), NA)
    original_y = ifelse(!is.na(data$Sale_Price), data$Sale_Price, NA)
  } else {
    data.y = NULL
    original_y = NULL
  }

  # Replace missing values with zero for Garage_Yr_Blt
  data.x$Garage_Yr_Blt[is.na(data.x$Garage_Yr_Blt)] = 0

  # Identify categorical variables
  categorical.vars = colnames(data.x)[
    sapply(data.x, function(x) mode(x) == "character")
  ]

  # Exclude categorical columns from data.x for the initial matrix
  data.matrix = data.x[, !colnames(data.x) %in% categorical.vars, drop = FALSE]

  # One-hot encoding for categorical variables
  for (var in categorical.vars) {
    mylevels = sort(unique(data.x[[var]]))
    m = length(mylevels)

    if (m > 0) {
      tmp.train = matrix(0, nrow(data.matrix), m)

      for (j in seq_along(mylevels)) {
        tmp.train[data.x[[var]] == mylevels[j], j] = 1
      }
      colnames(tmp.train) = paste(var, mylevels, sep = '_')
      data.matrix = cbind(data.matrix, tmp.train)
    }
  }

  # Combine the PID column back into the final data frame
  final_data = as.data.frame(data.matrix)

  # Add the PID column
  final_data$PID = data.x$PID

  # Optionally add the original Sale_Price
  final_data$original_y = original_y

  return(final_data)
}


#' @func get_train_data
#' @param : from_folder: idataset folder
#' @return: unfilter train data
get_train_data  = function(from_folder = ".") {
  data_file   =file.path(from_folder, "train.csv")
  tryCatch({
    data  = read.csv(data_file, header = TRUE)
    return(data)
  }, error = function(e) {
    message("Error reading data:", e$message)
    return(NULL)
  })
}

#' @func get_test_data
#' @param : from_folder: dataset folder
#' @return: unfilter test data
get_test_data  = function(from_folder = ".") {
  data_file   =file.path(from_folder, "test.csv")
  tryCatch({
    data  = read.csv(data_file, header = TRUE)
    return(data)
  }, error = function(e) {
    message("Error reading data:", e$message)
    return(NULL)
  })
}

#' @func variables_to_remove
#' @param : data
#' @param : cols_to_remove columns need to be removed
#' @return: filtered data

variables_to_remove = function(data, cols_to_remove = c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 'Heating',
                                                        'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area',
                                                        'Longitude', 'Latitude')) {
  data_result = data %>% select(-all_of(cols_to_remove))
  return(data_result)
}


#' @func: winsorize
#' @params : df: The data frame containing the columns to be winsorized.
#' @params :  cols: A vector of column names (default: several numeric columns) to apply the winsorization to.
#' @params  quantile_thresh: The quantile threshold above which values will be capped (default: 0.95, i.e., the 95th percentile).

winsorize = function(df, cols = c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2",
                                  "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", "First_Flr_SF",
                                  "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF",
                                  "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val"),
                     quantile_thresh = 0.95) {

  if (!is.data.frame(df) || ncol(df) < 1) {
    stop("Input 'df' is not a valid data frame or it has no columns.")
  }
  # Clean column names
  colnames(df) = trimws(colnames(df))

  # Identify columns that are actually present in the data frame
  cols_to_winsorize = intersect(cols, colnames(df))

  if (length(cols_to_winsorize) == 0) {
    warning("No columns to winsorize found in the data frame.")
    return(df)
  }

  for (column in cols_to_winsorize) {
    upper_bound  = quantile(df[[column]], probs = quantile_thresh, na.rm = TRUE)
    #print(paste("Upper bound for", column, ":", upper_bound))  # Debugging output

    # Limit values above the upper bound
    df[[column]] = pmin(df[[column]], upper_bound)
  }
  return(df)
}




#' @func: full_join_data
#' @params: data1: The first data frame.
#' @params: data2: The second data frame to be joined.
#' @params: col: The column name (default 'PID') on which to perform the full join.
#' @notes: This function performs a full join of two data frames based on the given column,
#'           keeping all rows from the first data frame.
#' @returns: A data frame containing the full join of data1 and data2.
full_join_data = function (data1, data2, col= 'PID' ) {
  merge(data1, data2, by = col , all.x = TRUE)
}


#' @func: load_and_fetch_data_type
#' @params: from_folder: The directory where the data is stored.
#' @params: data_type: The type of data to be loaded (either 'test' or other types).
#' @notes: This function loads and preprocesses data depending on the specified data type.
#'           If the type is 'test', it will load the test data and labels, join them, and preprocess.
#'           Otherwise, it loads the training data and preprocesses it.
#' @returns: A preprocessed data frame based on the specified data type.
load_and_fetch_data_type = function (from_folder, data_type ) {

  if (tolower(data_type) == tolower(DATA_TYPE.TEST)) {
    original_test_data = get_test_data(from_folder)
    if (is.null(original_test_data)) {
      stop("Test data not found or could not be loaded.")
    }
    #original_test_label = get_label_data(from_folder) # No access!
    #test_data = full_join_data(original_test_data, original_test_label)
    # Preprocess test data
    test_data = preprocessing_data(data = original_test_data, data_type = data_type)
    return (test_data)
  } else {
    original_train_data = get_train_data(from_folder)
    if (is.null(original_train_data)) {
      stop("Train data not found or could not be loaded.")
    }
    train_data = preprocessing_data(data = original_train_data, data_type = data_type)  # Pass data_type here
    return (train_data)
  }
}
align_features = function(train_data, test_data) {
  # Convert matrices to data frames to add missing features
  train_data_df = as.data.frame(train_data)
  test_data_df = as.data.frame(test_data)

  # Get the feature names of the training data
  train_feature_names = colnames(train_data_df)

  # Get the feature names of the test data
  test_feature_names = colnames(test_data_df)

  # Find missing features in the test data
  missing_features = setdiff(train_feature_names, test_feature_names)

  # Add the missing features to the test data and fill them with zeros
  for (feature in missing_features) {
    test_data_df[[feature]] = 0
    #cat(paste("Added missing feature to test data:", feature, "\n"))
  }

  # Ensure the columns in test data are in the same order as in training data
  test_data_df = test_data_df[, train_feature_names, drop = FALSE]

  # Convert the data frame back to a matrix
  test_data_aligned = as.matrix(test_data_df)

  return(test_data_aligned)
}

# narrow down parameters with Lasso
lasso_feature_selection = function(inputs, response) {
  # Fit the Lasso model
  lasso_model = glmnet(inputs, response, alpha = 1)

  # Cross-validate to find the optimal lambda
  cv_model = cv.glmnet(inputs, response, alpha = 1)
  best_lambda = cv_model$lambda.min

  # Extract coefficients using the best lambda
  lasso_coefs = coef(cv_model, s = "lambda.min")

  # Convert lasso_coefs to a vector
  lasso_coefs_vector = as.vector(lasso_coefs)

  # Identify selected features (non-zero coefficients)
  selected_features = rownames(lasso_coefs)[lasso_coefs_vector != 0]

  # Remove the intercept if present
  selected_features = selected_features[selected_features != "(Intercept)"]

  # Return the selected features
  return(selected_features)
}

train_ridge_model = function(X, y) {
  # Define a sequence of lambda values for cross-validation
  myridge_lambda_seq = exp(seq(-10, 1, length.out = 100))

  # Perform cross-validation to find the best lambda for Ridge regression
  cv_out = cv.glmnet(X, y, alpha = 0, lambda = myridge_lambda_seq)

  # Return the fitted model and the best lambda
  return(list(model = cv_out, best_lambda = cv_out$lambda.min))
}

predict_ridge = function(model, X_new) {
  # Make predictions on the new data using the trained model
  predictions = predict(model$model, s = model$best_lambda, newx = X_new)

  # Return the first column of predictions
  return(predictions[, 1])
}

# Train XGBoost model using caret with a training flag
train_xgboost_model_caret = function(train_matrix, train_y, train_flag = TRUE) {

  # Define the training control parameters
  train_control <- trainControl(
    method = "cv",  # Cross-validation method
    number = 10,    # Number of folds
    verboseIter = FALSE  # Disable verbose output for cleaner execution
  )

  if (train_flag) {
    #cat("Training mode: Hyperparameter tuning with varying parameters.\n")

    # Define a grid of hyperparameter values for XGBoost
    nrounds_values = c(50,100,250,500,1000,2500)
    eta_values = c(0.075, 0.1, 0.125)   # Learning rates
    max_depth_values = c(3, 6, 9)       # Maximum tree depths
    subsample_values = c(0.375, 0.5, 0.625)  # Subsample ratios
    colsample_bytree_values = c(0.7, 1)     # Column subsample ratios
    gamma_values <- 0  # Gamma set to 0 as without Gamma performed better
    min_child_weight_values <- c(1, 2, 3)  # Minimum child weights

    # Create a grid of parameters using expand.grid
    xgb_grid <- expand.grid(
      nrounds = nrounds_values,
      max_depth = max_depth_values,
      eta = eta_values,
      gamma = gamma_values,
      colsample_bytree = colsample_bytree_values,
      min_child_weight = min_child_weight_values,
      subsample = subsample_values
    )

  } else {
    #cat("Training mode: Using best known parameters without hyperparameter tuning.\n")

    # Define the best parameters as a data frame with a single row
    best_params = data.frame(
      nrounds = 1000,
      max_depth = 3,
      eta = 0.1,
      gamma = 0,
      colsample_bytree = 0.7,
      min_child_weight = 2,
      subsample = 0.5
    )

    # Use the best parameters grid
    xgb_grid <- best_params
  }

  # Display the parameter grid
  #print(xgb_grid)

  # Train the XGBoost model using caret with the defined hyperparameter grid
  xgb_model_caret = tryCatch({
    train(
      x = train_matrix,            # Training features (X_selected matrix)
      y = train_y,                 # Target variable (log-transformed y)
      method = "xgbTree",          # Specify XGBoost as the modeling method
      trControl = train_control,   # Training control parameters
      tuneGrid = xgb_grid,         # Hyperparameter grid
      metric = "RMSE",             # Evaluation metric to optimize
      verbose = FALSE              # Suppress verbose output for cleaner execution
    )
  }, error = function(e) {
    stop("Error during model training: ", e$message)  # Stop execution if an error occurs
  })

  return(xgb_model_caret)
}

set.seed(123)  # For reproducibility
# Define whether to perform hyperparameter tuning for XGBoost or use best-known parameters
trainXGModel <- FALSE  # Set to FALSE to use predefined best parameters

# load train data
train_data_processed = load_and_fetch_data_type(from_folder = ".", data_type = DATA_TYPE.TRAIN)

train_X = as.matrix(train_data_processed[, !names(train_data_processed) %in% "original_y"])
train_true_y = train_data_processed$original_y # true y in original form

train_log_y = log(train_true_y) #log y


# Check dimensions
# print(dim(train_X))
# print(length(train_log_y))

# # use Lasso to narrow down feature selection
selected_features = lasso_feature_selection(train_X, train_log_y)
num_selected_features = length(selected_features)
total_features = ncol(train_X)
num_selected_features = length(selected_features)
num_zeroed_out_features = total_features - num_selected_features

# cat("Total features:", total_features, "\n")
# cat("Selected features:", num_selected_features, "\n")
# cat("Zeroed-out features:", num_zeroed_out_features, "\n")

X_selected = train_X[, selected_features, drop = FALSE]
ridge_model  = train_ridge_model(X_selected, train_log_y)

# Train XGBoost model with caret (uses trainXGModel flag)
xgb_model_caret = train_xgboost_model_caret(train_X, train_log_y, trainXGModel)

test_data_processed = load_and_fetch_data_type(from_folder = ".", data_type = DATA_TYPE.TEST)

test_pid = test_data_processed$PID
test_X = as.matrix(test_data_processed[, !names(test_data_processed) %in% "original_y"])

##### try to fill in missing features
X_test_selected = data.frame(matrix(0, nrow = nrow(test_X), ncol = length(selected_features)))

# Step 2: Assign column names
colnames(X_test_selected) = selected_features

# Step 3: Fill in values for the features that exist in test_X
common_features = selected_features[selected_features %in% colnames(test_X)]
X_test_selected[, common_features] = test_X[, common_features]
X_test_selected_matrix = as.matrix(X_test_selected)

##### XGBoost Predictions
# Align test_X with train_X for XGBoost
#cat("Aligning test data features with training data features for XGBoost...\n")
aligned_test_X = align_features(train_X, test_X)
# cat("Feature alignment for XGBoost completed.\n")


# Predicting on testing data
ridge_predictions_log = predict_ridge(ridge_model, X_test_selected_matrix)
ridge_predictions = round(exp(ridge_predictions_log))  # Convert back from log scale

# Predict XGBoost model
xgb_predictions_log = tryCatch({
  predict(xgb_model_caret, newdata = aligned_test_X)
}, error = function(e) {
  stop("Error during XGBoost prediction: ", e$message)  # Stop execution if an error occurs
})

# Convert log predictions back to original scale
xgb_predictions = round(exp(xgb_predictions_log))
xgb_predictions = round(exp(xgb_predictions_log))  # Convert back from log scale

# Save the predictions as required
ridge_submission = data.frame(PID = test_pid, Sale_Price = ridge_predictions)
xgb_submission = data.frame(PID = test_pid, Sale_Price = xgb_predictions)


# Save Ridge predictions
write.table(ridge_submission, file = "mysubmission1.txt", row.names = FALSE, col.names = TRUE, sep = ",")

# Save XGBoost predictions
write.table(xgb_submission, file = "mysubmission2.txt", row.names = FALSE, col.names = TRUE, sep = ",")

# cat("Predictions saved to mysubmission1.txt (Ridge) and mysubmission2.txt (XGBoost).\n")
