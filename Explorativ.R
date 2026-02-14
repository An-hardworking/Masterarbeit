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

summary(lm(Trust ~ TSE, data=final_data))
summary(lm(Verification ~ TSE, data=final_data))
H5a <- formula(Trust ~ TSE) 
model5a  <- lme(H5a, data = final_data , random = ~1 | id / Reliability,method = "ML", 
                na.action = na.exclude,correlation = corCompSymm())
summary(model5a)

H5b <- formula(Verification ~ TSE) 
model5b <- lme(H5b, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude, correlation = corCompSymm())
summary(model5b)

(correl <- cor(final_data$Trust, final_data$Verification, use = "complete.obs", method = "spearman"))
(cor.test(final_data$Trust, final_data$Verification, use = "complete.obs", method = "spearman"))

H6a <- formula(Trust ~ PT*Reliability) 
model6a  <- lme(H6a, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude)
summary(model6a)
H6b <- formula(Verification ~ AIC*Reliability) 
model6b  <- lme(H6b, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude)
summary(model6b)

H7a <- formula(Trust ~ PT+Exposure) 
model7a  <- lme(H7a, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude)
summary(model7a)
H7b <- formula(Verification ~ AIC+Exposure) 
model7b  <- lme(H7b, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude)
summary(model7b)

colnames(U_data)[ncol(U_data)] <- "id"
explor_data <- U_data %>%
  select(id, Alter, Geschlecht,Bildung, `VR-Nutzung`)# %>%  # Keep only necessary columns
# 
# #explor_data <- explor_data %>%
#   left_join(final_data, by = "id")
  
  colnames(final_data)[colnames(final_data) == "VR-Nutzung"] <- "VR"
  
  Hx <- formula(Verification ~ VR) 
  modelx  <- lme(Hx, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude)
  summary(modelx)
  
exploredf
colnames(exploredf)[colnames(exploredf) == "VR-Nutzung"] <- "VR"
exploredf$Bildung <- as.factor(exploredf$Bildung)
#contrasts(exploredf$Bildung) <- contr.sum(5)

H8a <- formula(Trust ~ Reliability+AIC) 
model8a  <- lme(H8a, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model8a)
H8b <- formula(Verification ~ Reliability+AIC) 
model8b  <- lme(H8b, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model8b)

H9a <- formula(Trust ~ Exposure+PT) 
model9a  <- lme(H8a, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model9a)
H9b <- formula(Verification ~ Exposure+PT) 
model9b  <- lme(H9a, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model9b)

H9b <- formula(Verification ~ Exposure+AIC) 
model9b  <- lme(H9a, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model9b)

H8a <- formula(Trust ~ Reliability+PT) 
model8a  <- lme(H8a, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model8a)
H8b <- formula(Verification ~ Reliability+PT) 
model8b  <- lme(H8b, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(model8b)




contrasts(exploredf$Exposure) <- contr.sum(6)
######a,b######
modelRelAICTrust <- formula(Trust ~ AIC+Reliability) 
(modelRelAICTrust  <- lme(modelRelAICTrust, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
summary(modelRelAICTrust)
modelRelAICVeri <- formula(Verification ~ AIC+Reliability) 
modelRelAICVeri  <- lme(modelRelAICVeri, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(modelRelAICVeri)
modelExpAICTrust <- formula(Trust ~ AIC+Exposure) 
(modelExpAICTrust  <- lme(modelExpAICTrust, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
modelExpAICVeri <- formula(Verification ~ AIC+Exposure) 
(modelExpAICVeri  <- lme(modelExpAICVeri, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
summary(modelExpAICTrust)
summary(modelExpAICVeri)
#####c,d######
modelRelPTTrust <- formula(Trust ~ PT+Reliability) 
(modelRelPTTrust  <- lme(modelRelPTTrust, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
modelRelPTVeri <- formula(Verification ~ PT+Reliability) 
modelRelPTVeri  <- lme(modelRelPTVeri, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude)
summary(modelRelPTTrust)
summary(modelRelPTVeri)
modelExpPTTrust <- formula(Trust ~ PT+Exposure) 
(modelExpPTTrust  <- lme(modelExpPTTrust, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
modelExpPTVeri <- formula(Verification ~ PT+Exposure) 
(modelExpPTVeri  <- lme(modelExpPTVeri, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
summary(modelExpPTTrust)
summary(modelExpPTVeri)
####e####
modelagegenderTrust <- formula(Trust ~ Alter+Geschlecht) 
(modelagegenderTrust  <- lme(modelagegenderTrust, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
modelagegenderVeri <- formula(Verification ~ Alter+Geschlecht) 
(modelagegenderVeri  <- lme(modelagegenderVeri, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
summary(modelagegenderTrust)
summary(modelagegenderVeri)
####f####
colnames(exploredf)[colnames(exploredf) == "VR-Nutzung"] <- "VR"

modelVRTrust <- formula(Trust ~ VR) 
(modelVRTrust  <- lme(modelVRTrust, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
modelVRVeri <- formula(Verification ~ VR) 
(modelVRVeri  <- lme(modelVRVeri, data = exploredf , random = ~1 | id / Reliability, method = "ML", na.action = na.exclude))
summary(modelVRTrust)
summary(modelVRVeri)
#####TSE only####
H5a <- formula(Trust ~ TSE) 
model5a  <- lme(H5a, data = final_data , random = ~1 | id / Reliability,method = "ML", 
                na.action = na.exclude,correlation = corCompSymm())
summary(model5a)

H5b <- formula(Verification ~ TSE) 
model5b <- lme(H5b, data = final_data , random = ~1 | id / Reliability,method = "ML", na.action = na.exclude, correlation = corCompSymm())
summary(model5b)

# # Get the tables of FE only
# model <-modelExpAICVeri
# modelname <- deparse(substitute(modelExpAICVeri))
# # Get the fixed effects table
# model_summary <- summary(model)
# fixed_effects_table <- extract_fixed_effects(model_summary)
# 
# # Print or save the table
# current_date <- Sys.Date()  # Get today's date
# write.csv(
#   fixed_effects_table, 
#   file = paste0("fixed_effects_table_", modelname, ".csv"),
#   row.names = TRUE
# )
# 
# #Get the CI
# ci <- intervals(model, level = 0.95, which = "fixed")
# 
# # Extract fixed effects confidence intervals
# fixed_ci <- data.frame(
#   Parameter = rownames(ci$fixed),
#   Estimate = ci$fixed[, "est."],
#   Lower = ci$fixed[, "lower"],
#   Upper = ci$fixed[, "upper"]
# )
# 
# write.csv(
#   fixed_ci, 
#   file = paste0("fixed_CI_table_", modelname, ".csv"),
#   row.names = TRUE
# )

print("End of explorativ findings")

