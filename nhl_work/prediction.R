library(data.table)
library(ggplot2)
library(magrittr)
library(glue)
library(purrr)
library(caret)

data <- fread("nhl_data/clean_nhl.csv")
data$V1 <- NULL
data <- data[!(Pos == "RW/LW")]
data <- data[!(Pos == "LW/RW")]
data <- data[!(Pos == "RW/C")]
data <- data[!(Pos == "C/LW")]
data <- data[!(Pos == "LW/C")]
data <- data[(Age > 0)]

# data[, .N, by = Season]

ggplot(data, aes(G, Age)) + geom_point() + theme_minimal()

slm <- lm(G ~ Age + TOI + GP, data = data)
slm$coefficients

# temp_data <- data
# temp_data$Tm <- NULL
# temp_data$Player <- NULL
# temp_data$Pos <- NULL
# cor(temp_data)

residuals <- data.table(residuals = slm$residuals, fitted = slm$fitted)
ggplot(residuals, aes(fitted, residuals)) + geom_point(alpha = .3) +
  theme_minimal()

################
set.seed(1234)
n_data <- data[, c("Tm","G", "TOI", "Age", "GP")]

formula = as.formula("G ~ Tm + TOI + Age + GP")

#V1
n = n_data[,.N]
fold <- 100
cv_split <- split(sample(1:n), 1:fold)
cv_errors <- imap(cv_split, ~{
  model <- lm(formula = formula, data = data[-.x,])
  p <- predict(model, newdata = data[.x,])
  mean((p - data[.x, G])^2)
})

mean(unlist(cv_errors))

#V2
train_control <- trainControl(method = "cv", number = 100)
model <- train(form = formula, data = n_data, trControl = train_control, method = "lm")
model


model$resample
model$results$RMSE**2
