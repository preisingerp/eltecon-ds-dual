library(data.table)
library(ggplot2)
library(magrittr)
library(glue)
library(purrr)

data <- fread("nhl_data/clean_nhl.csv")
data$V1 <- NULL
data <- data[!(Pos == "RW/LW")]
data <- data[!(Pos == "LW/RW")]
data <- data[!(Pos == "RW/C")]
data <- data[!(Pos == "C/LW")]
data <- data[!(Pos == "LW/C")]
# need to delete two rows where age = NA

# data[, .N, by = Season]

ggplot(data, aes(G, Age)) + geom_point() + theme_minimal()

slm <- lm(G ~ . - Player - Tm, data = data)
slm$coefficients

temp_data <- data
temp_data$Tm <- NULL
temp_data$Player <- NULL
temp_data$Pos <- NULL
cor(temp_data)
