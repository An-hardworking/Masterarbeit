control <- lmeControl(maxIter = 100, msMaxIter = 100, niterEM = 50, msMaxEval = 200)


#simple model
model2a  <- lme(H2a, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude)

#complex model: add variance or correlation structure to the simple model

#variance structure: 
model2a_varIdent  <- lme(H2a, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude,weights=varIdent(form = ~1 | Reliability))
#correlation structure:
model2a_AR1 <- lme(H2a, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude,correlation = corAR1())
model2a_corCompSymm  <- lme(H2a, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude, correlation = corCompSymm())
model2a_corSymm <- lme(H2a, data = final_data, random = ~1 | id / Reliability, 
                       method = "ML", na.action = na.exclude, correlation = corSymm(), 
                       control = control)



# Get QQ plot for all models -----------------------------------------------------

models <- list(
  model2a,
  model2a_varIdent,
  model2a_AR1,
  model2a_corCompSymm,
  model2a_corSymm
)

model_names <- c(
  "model2a simple",
  "model2a with varIdent",
  "model2a with AR1",
  "model2a with corCompSymm",
  "model2a with corSymm"
)
name <- as.data.frame(model_names,colnames="names")
# Define the layout matrix
layout(mat = matrix(c(0,1,1,0,
                      2,2,3,3,
                      4,4,5,5), nrow = 3, byrow = TRUE))

#layout(mat = matrix(c(1,1,2,2,3,3,
 #                     0,4,4,5,5,0), nrow = 2, byrow = TRUE))

# Adjust margins for better spacing
par(mar = c(4, 4, 2, 1)) # Adjust plot margins

#par(mfrow = c(2, ((length(models)+1)/2))) # 2 rows and x columns

# Generate QQ plots for all models
for (i in seq_along(models)) {
  qqnorm(residuals(models[[i]]), main = paste("QQ Plot for", model_names[i]))
  #family = "Calibri"
  qqline(residuals(models[[i]]))
}
'for (i in seq_along(models)) {
  qqPlot(residuals(models[[i]], type = "pearson"),
  layout=c(4, 4, 2, 1),
  envelope = TRUE, main = paste("QQ Plot for",model_names[i]))
}
'
# Reset layout
par(mfrow = c(1, 1))
# Model evaluation: --------------------------------------------------------

anova_results_model2a <- anova(
  model2a,
  model2a_varIdent,
  model2a_AR1,
  model2a_corCompSymm,
  model2a_corSymm
)
a1 <- anova(model2a,model2a_corSymm)
a2 <- anova(model2a_varIdent,model2a_corSymm)
a3 <- anova(model2a_AR1,model2a_corSymm)
a4 <- anova(model2a_corCompSymm,model2a_corSymm)

best_model_table_2a <- rbind(a1,a2,a3,a4)

compare_performance(  model2a,
                      model2a_varIdent,
                      model2a_AR1,
                      model2a_corCompSymm,
                      model2a_corSymm,
                      verbose = FALSE)
#Perf Criteria in one table ---------------------------------------------------

results <- data.frame(
  AIC = numeric(),
  BIC = numeric(),
  RMSE = numeric(),
  R2_Conditional = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each model and calculate metrics
for (i in seq_along(models)) {
  model <- models[[i]]            # Get the model
  a <- AIC(model)                 # Calculate AIC
  b <- BIC(model)                 # Calculate BIC
  rmse <- performance::rmse(model)  # Calculate RMSE
  r2 <- r2(model)$R2_conditional     # Conditional R²
  
  # Add results to the data frame
  results <- rbind(results, data.frame(
    AIC = a,
    BIC = b,
    RMSE = rmse,
    R2_Conditional = r2
  ))
}

# Print the results table
criteria_model2a <- cbind(model_names,as.data.frame(results))
rownames(criteria_model2a) <- model_names
print(criteria_model2a)


# Export tables -----------------------------------------------------------

current_date <- Sys.Date()  # Get today's date
filename1 <- paste0( current_date, "anova 2a", ".xlsx")  # Dynamic filename
filename2 <- paste0(current_date, "Criteria 2a",  ".xlsx")  # Dynamic filename
filename3 <- paste0(current_date, "Super_anova_superior_model_2a",  ".xlsx")  # Dynamic filename


# Combine location and filename
file_path1 <- file.path(save_location, filename1)
file_path2 <- file.path(save_location, filename2)
file_path3 <- file.path(save_location, filename3)

# Export the results to the specified location
write_xlsx(criteria_model2a, file_path2)
write_xlsx(anova_results_model2a, file_path1)
write_xlsx(best_model_table_2a, file_path3)

#
filename <- paste0( current_date,"anova_part", ".xlsx") 
file_path <- file.path(save_location, filename)
anova_table <- anova(model2a, model2a_varIdent)
write_xlsx(anova_table, file_path)

# Remove 5% outliers Residual_Pearson --------------------------------------------------------
model <- model2a_corSymm
summary(model)
threshold <- 0.05     #threshold 5% threshold   
#id <- armd$subject #id

outliers.idx <- 
  within(na.exclude(final_data),
         {
           residP <- resid(model, type = "pearson", na.action = na.exclude)  # Get the Pearson residuals
           idx <- abs(residP) > -qnorm(threshold/2)
         })

outliers <- subset(outliers.idx,idx)

unique_outliers <- unique(outliers$id)
length(unique(outliers$id))
#length(unique(final_data_clean $id))
final_data_clean <- final_data[!(final_data$id %in% unique_outliers ), ]

model2a_corSymm_new<- lme(H2a, data = final_data_clean, random = ~1 | id / Reliability, 
                          method = "ML", na.action = na.exclude, correlation = corSymm(), 
                          control = control)
new_model <- model2a_corSymm_new
qqnorm(residuals(new_model))
qqline(residuals(new_model))
summary(new_model)

# RE:EBULP
eblup_df <- as.data.frame(ranef(model, level = 2)) 
eblup_df$ID <- rownames(eblup_df)

# Extract Reliability Level from ID names
eblup_df <- eblup_df %>%
  mutate(Reliability = ifelse(grepl("/High", ID), "High", "Low"),
         ID = gsub("/(High|Low)", "", ID))  # Clean up ID names
# Plot distribution of EBLUPs
ggplot(eblup_df, aes(x = Reliability, y = `(Intercept)`, fill = Reliability)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Distribution of EBLUPs by Reliability Level",
       x = "Reliability Level",
       y = "EBLUP (Random Intercept Estimate)")

sd_eblup <- eblup_df %>%
  group_by(Reliability) %>%
  summarise(SD_EBLUP = sd(`(Intercept)`))

# Get CI  ------------------------------------------------

##### Get confidence intervals before ####
ci <- intervals(model, level = 0.95, which = "fixed")

# Extract fixed effects confidence intervals
fixed_ci <- data.frame(
  Parameter = rownames(ci$fixed),
  Estimate = ci$fixed[, "est."],
  Lower = ci$fixed[, "lower"],
  Upper = ci$fixed[, "upper"]
)

# Print or save the table
print(fixed_ci) 
filename_CI <- paste0( current_date, "CI95_model2a_before",".xlsx") 
file_path_CI <- file.path(save_location, filename_CI)
write_xlsx(fixed_ci, file_path_CI)


####Get confidence intervals after####
ci <- intervals(new_model, 
                level = 0.95, 
                which = "fixed"
)

# Extract fixed effects confidence intervals
new_fixed_ci <- data.frame(
  Parameter = rownames(ci$fixed),
  Estimate = ci$fixed[, "est."],
  Lower = ci$fixed[, "lower"],
  Upper = ci$fixed[, "upper"]
)

# Print or save the table
print(new_fixed_ci) 
new_filename_CI <- paste0(current_date, "CI95_model2a_after",  ".xlsx") 
new_file_path_CI <- file.path(save_location, new_filename_CI)
write_xlsx(new_fixed_ci, new_file_path_CI)


# Get FE_before and after -------------------------------------------------

####GetFE before ####
# Function to extract fixed effects table from summary(model)
extract_fixed_effects <- function(model_summary) {
  # Extract the fixed effects table
  fixed_effects <- as.data.frame(model_summary$tTable)
  
  # Add row names as a column for parameter names
  fixed_effects$Parameter <- rownames(fixed_effects)
  rownames(fixed_effects) <- NULL
  
  # Reorder columns
  fixed_effects <- fixed_effects[, c("Parameter", "Value", "Std.Error", "DF", "t-value", "p-value")]
  
  return(fixed_effects)
}

# Example usage
# Get the fixed effects table
model_summary <- summary(model)
fixed_effects_table <- extract_fixed_effects(model_summary)

# Print the table
print(fixed_effects_table)
# Print or save the table
filename_FE <- paste0(current_date, "fixed_effects_table_model2a", ".xlsx") 
file_path_FE <- file.path(save_location, filename_FE)
write_xlsx(fixed_effects_table, file_path_FE)

####Get FE after####
new_model_summary <- summary(new_model)

# Get the fixed effects table
new_fixed_effects_table <- extract_fixed_effects(new_model_summary)

# Print the table
print(fixed_effects_table)
# Print or save the table
new_filename_FE <- paste0(current_date, "new_fixed_effects_table_model2a", ".xlsx") 
new_file_path_FE <- file.path(save_location, new_filename_FE)
write_xlsx(new_fixed_effects_table, new_file_path_FE)



# Comparison model vs Hypothesis 1:  --------------------------------------

anova(model1a_corSymm,model2a_corSymm) 

print("see in Results_tables folder to see the excel tables of fixed effects results of RQ2a")