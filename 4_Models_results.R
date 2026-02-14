theme_set(theme_classic()) # This sets the default ggplot theme
options(width = 65, digits = 5, show.signif.stars = TRUE)

# Create data for the hypothese -----------------------------------------------------------
df_long <- data %>%
  dplyr::select(id, Reliability, Block1, Block2, Block3, Block4, Block5, Block6, Alter, Geschlecht,Bildung, `VR-Nutzung`,AIC , PT , TSE) %>%
  pivot_longer(cols = starts_with("Block"), 
               names_to = "Exposure", 
               values_to = "Verification")
df_longb <- data %>%
  dplyr::select(Trust.1, Trust.2, Trust.3, Trust.4, Trust.5, Trust.6,) %>%
  pivot_longer(cols = starts_with("Trust"), 
               values_to = "Trust")

# Data Exploration --------------------------------------------------------
data_x <- cbind(df_long, df_longb)

final_data <- data_x %>%
  dplyr::select(-c(name, "id...12"))

names(final_data)[names(final_data) == "id...1"] <- "id"


#Normal distribution of predictors#
numeric_columns <- c("AIC", "PT", "TSE", "Verification", "Trust")
final_data$Exposure <- as.factor(final_data$Exposure) # Exposure is categorical variable
exploredf <- final_data
par(mfrow = c(3, 1))
# Loop through each numeric column in final_data
for (col in numeric_columns) {
  hist_data <- hist(final_data[[col]], breaks = 10, prob = TRUE, main = paste("Histogram of", col),
                    xlab = col, col = "lightblue", border = "black")
lines(density(final_data[[col]], na.rm = TRUE), col = "red", lwd = 2)  # Density of the data
x_fit <- seq(min(final_data[[col]], na.rm = TRUE), max(final_data[[col]], na.rm = TRUE), length = 100)
y_fit <- dnorm(x_fit, mean = mean(final_data[[col]], na.rm = TRUE), sd = sd(final_data[[col]], na.rm = TRUE))
lines(x_fit, y_fit, col = "blue", lwd = 2, lty = 2)  # Normal distribution
}
# Reset plot layout
par(mfrow = c(1, 1))  # Optional, reset to single plot layout
####Fragen: normal distribution der Predictors? significant nicht normally distributed: what to do?
###data34 <- data_x %>%
  #dplyr::select(-c(name, AIC, PT, TSE))

# Outliers of raw y -------------------------------------------------------

# Function to calculate outliers based on 2.5 SD
find_outliers <- function(data, column_name, threshold = 2.5) {
  column_values <- data[[column_name]]
  
  # Calculate mean and standard deviation
  column_mean <- mean(column_values, na.rm = TRUE)
  column_sd <- sd(column_values, na.rm = TRUE)
  
  # Define upper and lower bounds
  lower_bound <- column_mean - (threshold * column_sd)
  upper_bound <- column_mean + (threshold * column_sd)
  
  # Find indices of outliers
  outliers <- which(column_values < lower_bound | column_values > upper_bound)
  return(outliers)
}

# Apply function to Trust and Verification columns
trust_outliers <- find_outliers(final_data, "Trust")
verification_outliers <- find_outliers(final_data, "Verification")

# Print outliers
cat("Outlier indices in Trust:", trust_outliers, "\n")
cat("Outlier indices in Verification:", verification_outliers, "\n")

# Optionally, subset the data without outliers
final_data_no_outliers <- final_data[-unique(c(trust_outliers, verification_outliers)), ]

# Display the cleaned data
cat("Number of rows in cleaned data:", nrow(final_data_no_outliers), "\n")

# Model formula -----------------------------------------------------------
H1a <- formula(Trust ~ AIC+PT) 
H1b <- formula(Verification ~ AIC+PT) 

H2a <- formula(Trust ~ AIC+PT+TSE) 
H2b <- formula(Verification ~ AIC+PT+TSE) 

H3a <- formula(Trust ~ Reliability +Exposure) 
H3b <- formula(Verification ~ Reliability +Exposure) 

H4a <- formula(Trust~ Reliability * Exposure) 
H4b <- formula(Verification ~ Reliability * Exposure) 
# Models  preperation ----------------------------------------------------------

control <- lmeControl(maxIter = 100, msMaxIter = 100, niterEM = 50, msMaxEval = 200)
final_data$Exposure <- as.factor(final_data$Exposure)
contrasts(final_data$Exposure) <- contr.sum(6)

font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")  # Adjust path as needed
showtext_auto()
par(family = "serif", cex.main = 2.8, cex.lab = 1.8, cex.axis = 1.5) # the default of R

print("Final data are ready and models are built up")