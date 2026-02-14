# Umfrage data: Ziele
    #1: die Werte der Individual Differences Faktoren
    #2: Demographische Daten in der Tabelle: N, Age(SD), Gender: female, male, non-binary, VR Nutzung

#we omitted the participants from the excel that are excluded from the analysis ("VP_2B", "VP_4B",  "VP_16A", "VP_34A", "VP_54A"))

U_data<- read_excel("R_script_of_ME/data_vr-interaktion_preprocessed.xlsx") 
U_data <- U_data %>% 
  mutate(Reliability = substr(VPID, nchar(VPID), nchar(VPID))) 
U_data$Reliability[U_data$Reliability == "A"] <- "High" 
U_data$Reliability[U_data$Reliability == "B"] <- "Low"
# Check for rows with any NA values
rows_with_na <- U_data[!complete.cases(U_data), ]
# Print the number of rows with NA values
cat("Number of rows with NA values:", nrow(rows_with_na), "\n")
# Optionally, print the rows that contain NA values
if (nrow(rows_with_na) > 0) {
  cat("Rows with NA values:\n")
  print(rows_with_na)
} else {
  print("no NA in Umfrage_data")
}

# Age_descriptiv ----------------------------------------------------------


U_data$Alter <- as.numeric(U_data$Alter)
U_data$Geschlecht <- factor(U_data$Geschlecht, levels=c(1,2,3))
levels(U_data$Geschlecht) <- c("Female", "Male", "Diverse")
average_age <- mean(U_data$Alter)
std_dev_age <- sd(U_data$Alter)

# Print average age and standard deviation
cat("Average Age:", average_age, "\n")
cat("Standard Deviation of Age:", std_dev_age, "\n")

# Create a table for gender counts
(gender_table <- table(U_data$Geschlecht))

# Calculate the percentage distribution
(gender_percentage <- prop.table(gender_table) * 100)


# Ensure VR-Nutzung is treated as a factor with the specified levels and labels
U_data$`VR-Nutzung` <- factor(U_data$`VR-Nutzung`, 
                              levels = c(5, 1, 2, 3, 4),
                              labels = c("Never used before",
                                         "Less than 15 minutes",
                                         "Between 15 minutes and 3 hours",
                                         "Between 3 and 12 hours",
                                         "More than 12 hours"))

# Create a summary table for VR-Nutzung
(vr_summary <- table(U_data$`VR-Nutzung`))

# Ensure Assistenznutzung is treated as a factor with the specified levels and labels
U_data$Assistenznutzung <- factor(U_data$Assistenznutzung, 
                                  levels = c(8,7,6,5,4,3,2,1),
                                  labels = c("Never",
                                             "Less often",
                                             "Once a month",
                                             "Several times a month",
                                             "Once a week",
                                             "Several times a week",
                                             "Daily",
                                             "Several times a day" 
                                             ))

# Create a summary table for Assistenznutzung
assistenz_summary <- table(U_data$Assistenznutzung)

# Summarize the demographic information by 'Reliability' group with counts and percentages in ratio of each group
demographic_table_by_reliability <- U_data %>%
  group_by(Reliability) %>%
  summarise(
    N = n(), #N=40
    `Age Mean` = round(mean(Alter), 2),
    `Age SD` = round(sd(Alter), 2),
    
    # Gender distribution (count and percentage)
    Female = sum(Geschlecht == "Female"),
    `Female %` = round((Female / N) * 100, 2),
    
    Male = sum(Geschlecht == "Male"),
    `Male %` = round((Male / N) * 100, 2),
    
    `Non-binary` = sum(Geschlecht == "Non-binary"),
    `Non-binary %` = round((`Non-binary` / N) * 100, 2),
    
    # VR-Nutzung categories (count and percentage)
    `VR Never used` = sum(`VR-Nutzung` == "Never used before"),
    `VR Never used %` = round((`VR Never used` / N) * 100, 2),
    
    `VR Less than 15 min` = sum(`VR-Nutzung` == "Less than 15 minutes"),
    `VR Less than 15 min %` = round((`VR Less than 15 min` / N) * 100, 2),
    
    `VR 15 min - 3 hours` = sum(`VR-Nutzung` == "Between 15 minutes and 3 hours"),
    `VR 15 min - 3 hours %` = round((`VR 15 min - 3 hours` / N) * 100, 2),
    
    `VR 3 - 12 hours` = sum(`VR-Nutzung` == "Between 3 and 12 hours"),
    `VR 3 - 12 hours %` = round((`VR 3 - 12 hours` / N) * 100,2),
    
    `More than 12 hours` = sum(`VR-Nutzung` == "More than 12 hours"),
    `More than 12 hours %` = round((`More than 12 hours` / N) * 100, 2),
    # Assistenznutzung categories (count and percentage)
    `Assistenz Several times a day` = sum(Assistenznutzung == "Several times a day"),
    `Assistenz Several times a day %` = round((`Assistenz Several times a day` / N) * 100, 2),
    
    `Daily` = sum(Assistenznutzung == "Daily"),
    `Daily %` = round((`Daily` / N) * 100, 1),
    
    `Assistenz Several times a week` = sum(Assistenznutzung == "Several times a week"),
    `Assistenz Several times a week %` = round((`Assistenz Several times a week` / N) * 100, 2),
    
    `Once a week` = sum(Assistenznutzung == "Once a week"),
    `Once a week %` = round((`Once a week` / N) * 100, 2),
    
    `Assistenz Several times a month` = sum(Assistenznutzung == "Several times a month"),
    `Assistenz Several times a month %` = round((`Assistenz Several times a month` / N) * 100, 2),
    
    `Once a month` = sum(Assistenznutzung == "Once a month"),
    `Once a month %` = round((`Once a month` / N) * 100, 2),
    
    `Less often` = sum(Assistenznutzung == "Less often"),
    `Less often %` = round((`Less often` / N) * 100, 2),
    
    `Assistenz Never used` = sum(Assistenznutzung == "Never"),
    `Assistenz Never used %` = round((`Assistenz Never used` / N) * 100, 2)
  )

# View the summary table by Reliability
print(demographic_table_by_reliability)

U_data$AIC <- rowMeans(U_data[, grep("^AC", colnames(U_data))])
U_data$PT <- rowMeans(U_data[, grep("^PT", colnames(U_data))])
U_data$TSE <- rowMeans(U_data[, grep("^SE", colnames(U_data))])

# Create the new data frame with ID and the three sum columns
ind_dif_df <- U_data[, c("VPID", "Alter", "Geschlecht","Bildung", "VR-Nutzung","AIC", "PT", "TSE", "Reliability")]

# Rename the ID column to something more descriptive if needed
colnames(ind_dif_df)[1] <- "id"

# Print the new data frame
print(ind_dif_df)
 ###########Checkif there is any VP who always gives the same answer: definitely no

print("End of importing questionnaire data")