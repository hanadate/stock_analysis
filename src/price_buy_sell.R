# Load necessary libraries
library(keras)
library(tensorflow)
library(tidyquant)

# 
target.symbol <- new.env()
getSymbols("SPY", env=target.symbol)

# Load and preprocess data
features <- target.symbol$SPY[,c("SPY.Close","SPY.Volume","SPY.Open","SPY.High","SPY.Low")]
scaled_features <- scale(features)

# Create sequences for LSTM input
time_steps <- 60
X <- NULL
y <- NULL
for (i in time_steps:(nrow(scaled_features) - 1)) {
  X <- abind::abind(X, scaled_features[(i - time_steps + 1):i, ], along = 3)
  y <- c(y, scaled_features[i + 1, "SPY.Close"]) # Predict next closing price
}

# Reshape data for LSTM input
X <- array(X, dim = c(dim(X)[1], time_steps, ncol(features)))

# Build LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(time_steps, ncol(features))) %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(units = 50) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error"
)

# Train the model
history <- model %>% fit(
  X, y,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 10))
)

# Save the model
save_model_hdf5(model, "multi_feature_lstm_model.h5")
