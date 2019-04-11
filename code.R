# Exercise 1:
# The equation to generate data


# Instructions to run this code: to get the same graph in the assignment document, you need to load the workspace included
# in the folder


# These are the functions used to create the dummy data
# error <- rnorm(999, 150, 50)
# x_var <- runif(999, 100, 300)
y_var <- 1.5*x_var + error

# Create the original data set
data_set <- data.frame(x_var, y_var)

# Create the data_set with a single outlier
outlier_data <- rbind(data_set, c(9000, -9000))
names(data_set) <- c('input', 'output')
names(outlier_data) <- c('input', 'output')

# Create two linear models
orig <- lm(output ~ input, data = data_set)
outlier <- lm(output ~ input, data = outlier_data)

summary(orig)
summary(outlier)

# Plot the two regression lines
plot(data_set, main = 'Regression lines of two data sets', xlab='Independent variable', ylab ='Dependent variable', ylim = c(150, 800))
abline(coef= summary(orig)$coef, col = 'blue', lwd = 3)
abline(coef= summary(outlier)$coef, col ='red', lwd = 3)
legend("topright", legend=c("Original data set", "With extra outlier")
       ,col = c('black', 'red'), lty = 1:1, cex=0.8)


#Exercise 2:
library(arm)
library(Matching)
data(lalonde)

# Extract control group
control_gr <- lalonde[which(lalonde$treat == 0),]

# Make the model
model_lalonde <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(age*age) + I(re74*re75),
                    data = control_gr)
set.seed(42)

expected_val <- function(educ, re74, re75) {
  result <- data.frame(age=integer(),low_bound=numeric(), high_bound=numeric())
  sim_result <- sim(model_lalonde, n.sims = 10000)
  for (i in 17:55) {
    coefs <- sim_result@coef
    predicts <- rep(0, 10000)
    vectors_of_chars <- c(1, i, educ, re74, re75, educ * re74, educ * re75, i * re74, i * re75, i*i, re74 * re75)
    for (x in 1:10000) {
      prediction <- sum(coefs[x,]*vectors_of_chars)
      predicts[x] <- prediction
    }
    range <- c(i, quantile(predicts, probs=c(0.025, 0.975)))
    result <- rbind(result, range)
  }
  names(result) <- c('age', '2.5%', '97.5%')
  return(result)
}

predicted_val <- function(educ, re74, re75) {
  result <- data.frame(age=integer(),low_bound=numeric(), high_bound=numeric())
  sim_result <- sim(model_lalonde, n.sims = 10000)
  for (i in 17:55) {
    coefs <- sim_result@coef
    predicts <- rep(0, 10000)
    vectors_of_chars <- c(1, i, educ, re74, re75, educ * re74, educ * re75, i * re74, i * re75, i*i, re74 * re75)
    for (x in 1:10000) {
      error <- rnorm(1, 0, sim_result@sigma[x])
      prediction <- sum(coefs[x,]*vectors_of_chars) + error
      predicts[x] <- prediction
    }
    range <- c(i, quantile(predicts, probs=c(0.025, 0.975)))
    result <- rbind(result, range)
  }
  names(result) <- c('age', '2.5%', '97.5%')
  return(result)
}

exp_median <- expected_val(median(control_gr$educ), median(control_gr$re74), median(control_gr$re75))
exp_quantile <- expected_val(quantile(control_gr$educ, probs=0.75), quantile(control_gr$re74, probs=0.75),
                             quantile(control_gr$re75, probs=0.75))

pred_median <- predicted_val(median(control_gr$educ), median(control_gr$re74), median(control_gr$re75))
pred_quantile <- predicted_val(quantile(control_gr$educ, probs=0.75), quantile(control_gr$re74, probs=0.75),
                               quantile(control_gr$re75, probs=0.75))

# Get the results into a single data frame
final_table_exp<- cbind(exp_median, exp_quantile[,2:3])
names(final_table_exp) <- c("Age", "2.5% (Median)", "97.5% (Median)", "2.5% (Quantile)", "97.5% (Quantile)")

final_table_pred<- cbind(pred_median, pred_quantile[,2:3])
names(final_table_pred) <- c("Age", "2.5% (Median, with sigma)", "97.5% (Median, with sigma)", "2.5% (Quantile, with sigma)", "97.5% (Quantile, with sigma)")

final_table <- cbind(final_table_exp, final_table_pred[,2:ncol(final_table_pred)])
for (x in 1: nrow(final_table)){
  for (y in 2: ncol(final_table)){
    final_table[x, y] <- round(final_table[x, y], 2)
  }
}


# Plot for expected values
plot(x = c(1: 100), y = c(1:100), type='n', xlim = c(17,55), ylim = c(0,14000), xlab = 'Age (years)', ylab ='Re78 (dollars)',
     main = "Expected real income in 1978")
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = exp_median[age - 16, 2],
    x1 = age,
    y1 = exp_median[age - 16, 3],
    lwd = 4,col = 'red')
}
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = exp_quantile[age - 16, 2],
    x1 = age,
    y1 = exp_quantile[age - 16, 3],
    lwd = 2,col = 'blue', lty = 'dashed')
}
legend("topleft", legend=c('educ, re74, re75 held at median', 'educ, re74, re75 held at 75% quantile')
       ,col = c('red', 'blue'), lty = 1:2, lwd = 2, cex=0.8)


# Plot for predicted values
plot(x = c(1: 100), y = c(1:100), type='n', xlim = c(17,55), ylim = c(-10000,25000), xlab = 'Age (years)', ylab ='Re78 (dollars)',
     main = "Predicted real income in 1978")
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred_median[age - 16, 2],
    x1 = age,
    y1 = pred_median[age - 16, 3],
    lwd = 3,col = 'red')
}
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred_quantile[age - 16, 2],
    x1 = age,
    y1 = pred_quantile[age - 16, 3],
    lwd = 2,col = 'blue', lty = 4)
}

legend("topleft", legend=c('educ, re74, re75 held at median', 'educ, re74, re75 held at 75% quantile')
       ,col = c('red', 'blue'), lty = 1:2, lwd = 2, cex=0.8)


#Exercise 3:

# Get original data set
data("PlantGrowth")

# Remove obs with trt2, add a column for a binary variable
plant_data <- PlantGrowth[-which(PlantGrowth$group == "trt2"),]
binary = rep(0, length(plant_data))
for (i in 1:length(plant_data$group)) {
  if (plant_data$group[i] == 'ctrl'){
    binary[i] <- 0
  }else {
    binary[i] <- 1
  }
}
plant_data <- cbind(plant_data, binary)

# Make the model
model_plant <- lm(weight ~ binary, data = plant_data)

# Get the analytical confint from the model
conf_treat <- confint(model_plant)[2,]

# Bootstrap the coefficient for 'binary'
boot_result <- rep(0, 10000)
set.seed(45)
for (i in 1:10000){
  # n = 10000 bootstrap
  sample_idx <- sample(1:nrow(plant_data), size=nrow(plant_data), replace = T)  # Sampling with replacement
  boot_model <- lm(weight ~ binary, data = plant_data, subset=sample_idx)  # Make a model for each sample
  boot_result[i] <- boot_model$coefficients[2]  # Extract the coefficient, save in vector
}

# Get the bootstrapped confint
boot_conf_treat <- quantile(boot_result, probs = c(0.025, 0.975))
coef_table <- rbind(conf_treat, boot_conf_treat)

# Create the table
row.names(coef_table) <- c('Analytical', 'Bootstrapped')
coef_table

# Make the histogram
hist(boot_result, xlab = "Coefficient value", main = "Frequency of bootstrap-sample results", ylim = c(0,3000))

# Analytical value  of the coefficient
abline(v = summary(model_plant)$coef[2], col ='red', lty = 2, lwd = 2)

legend("topleft", legend=c("Model's value of the coefficient")
       ,col = c('red'), lty = 2, lwd = 2, cex=0.8)


#Exercise 4:
rsq <- function(data, prediction){
  mean_data <- mean(data)
  total_sos <- sum((data - mean_data)^2)
  residual_sos <- sum((data - prediction)^2)
  return(1-(residual_sos/total_sos))
}

# Reuse the model from exercise 3.
plant_predict <- predict(model_plant)

# The results from the function and from the model output are the same
rsq(plant_data$weight, plant_predict)
summary(model_plant)


#Exercise 5:
library(foreign)
nsw <- read.dta("nsw.dta")
nsw <- nsw[, -1]
ctrl <- which(nsw$treat == 0)

# Separate the data into a control and a treatment group
nsw_ctrl <- nsw[ctrl,]
nsw_treat <- nsw[-ctrl,]

# Use the entire data, make the logit model
logit_fit <- glm(treat ~. -re78, data = nsw, family=binomial)

# Calculate probability predictions for the groups
ctrl_probs <- predict(logit_fit, nsw_ctrl, type = 'response')
treat_probs <- predict(logit_fit, nsw_treat, type = 'response')

# Histogram for control group
hist(ctrl_probs, breaks = 25, main = 'Distribution of predictions for control and treatment group',
     xlab = "Chance of being in the treatment group", col ='dodgerblue1', ylim = c(0, 110))

# Histogram for treatment group
hist(treat_probs,breaks = 25,
     xlab = "Chance of being in the treatment group", col =rgb(255, 0, 0, max = 255, alpha = 180), ylim = c(0, 110), add = T)

legend("topright", legend=c("Control group", "Treatment group")
       ,col = c('dodgerblue1', 'red'), lty = 1, lwd = 4, cex=0.8)