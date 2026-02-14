setwd("CSV_Data/Vollständige")

# Read raw data from csv --------------------------------------------------

for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
data_list <- mget(temp)
combined_data <- do.call(rbind, data_list)
combined_data <- as.data.frame(combined_data)

compact_data <- cbind(combined_data$id, combined_data$taskname, combined_data$value, combined_data$height, combined_data$blockname, combined_data$status)
compact_data <- as.data.frame(compact_data)
colnames(compact_data) <- c("id","taskname","value","height","blockname","status")
head(compact_data)
####sort as ID and Blockname
sorted_data <- compact_data[order(compact_data$id, compact_data$blockname), ]

#write.csv(sorted_data, file = "sorted.data", row.names = TRUE)
sorted_data <- sorted_data %>%
  mutate(blockname = str_extract(blockname, "\\d+$")) ## it should show us the shorted version of raw data
sorted_data$value <- as.numeric(sorted_data$value)

# What are the values of trust to each block?
Singletrust <- sorted_data %>%
  filter(taskname == "TrustSingleItem") %>%  # Filter for TabFinishedMeasure
  group_by(id, blockname) %>%    #
  mutate(TrustSingleItem = value) %>% 
  arrange(id, blockname)
  # Group by ID and height
Singletrust <- subset(Singletrust, select = -c(taskname, status, height, value))


# Count for each trial ------------------------------------------------------------
heights <- as.character(c(54, 44, 67, 68, 69, 
                          41, 58, 48, 40, 70, 
                          69, 62, 47, 48, 58, 
                          68, 53, 41, 67, 56, 
                          41, 66, 41, 69, 60, 
                          67, 53, 42, 61, 50))

blocks <- as.character(rep(1:6, each = 5))


# Each participant goes through the 30 heights
heights_repeated <- rep(heights, times = length(unique(sorted_data$id)))

#Each participant goes through the 6 blocks
block <- rep(blocks, times = length(unique(sorted_data$id)))
# Repeat each participant 30 times
participants_repeated <- rep(unique(sorted_data$id), each = length(heights))

# Combine into a data frame with all trials in all blocks
all_heights <- data.frame(id = participants_repeated, blockname=blocks, height = heights_repeated)

#### now lets count how many TabFinishedMeasure columns are there in each block to each heights in Tab_finisched_data?
Tab_finish_data <- sorted_data %>%
  filter(taskname == "TabFinishedMeasure" & status== "InCapsule") %>%  # Filter for TabFinishedMeasure
  group_by(id, height, blockname) %>%                      # Group by ID and height
  summarize(count = n_distinct(value), .groups = "drop")  %>% 
  arrange(id, blockname)
#Tab_finish_data, however, has some empty spaces, which are the trials that the VPs
#decided not to verify any parameters, therefore,
#we need to join the Tab_finish_data with all_heights to make sure that all trials
#will be counted

df_full_heights <- all_heights %>%
  left_join(Tab_finish_data, by = c("id", "blockname", "height")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

df_full_heights[c("id","blockname")] <- lapply(df_full_heights[c("id","blockname")], as.factor)

#View(df_full_heights)
df_full_heights$trials <- as.factor(rep(1:30, length(unique(df_full_heights$id))))

# Specify the trials with false diagnosis in both groups
trials_to_remove <- as.factor(c(2, 5, 6, 8, 9, 12, 13, 18, 20, 22, 25, 26, 28))

# Remove the rows with the trials with false diagnosis
true_diagnosis_only <- df_full_heights[!df_full_heights$trials %in% trials_to_remove, ]

#get the mean of verification value in
block_means <- true_diagnosis_only %>%
  group_by(id, blockname) %>% # Assuming 'Exposure' refers to blocks
  summarise(
    Verification = mean(count, na.rm = TRUE)
  )

# Outdated_code_deletable -------------------------------------------------

#summary of verified parameters to each block
'df <- Tab_finish_data %>%
  group_by(id, blockname) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%  
  arrange(id, blockname)
#Expand a grid to contain the data
all_blocks <- expand.grid(
  id = unique(sorted_data$id),
  blockname = 1:6
)
all_blocks$blockname <- as.character(all_blocks$blockname)
# Join with the original dataframe
df_complete <- all_blocks %>%
  left_join(df, by = c("id", "blockname")) %>%
  mutate(total_count = ifelse(is.na(total_count), 0, total_count))'

#the code above will not be used, as we now have to get the mean in each block with true 
#diagnosis only, not for every block; the code above is for true and false diagnosis both

##Transpose the datasets in wide form and name it "virtras"
#verifiedparameter
df_wide <- block_means %>%
  pivot_wider(
    names_from = blockname,
    values_from = Verification,
    names_prefix = "Block",
  )
#subjtrust
trust_wide <- Singletrust %>%
  pivot_wider(
    names_from = blockname,
    values_from = TrustSingleItem,
    names_prefix = "Trust.",
  )
# well because trust_wide is grouped df, we need to ungroup the dataset to set it free 
trust_wide <- dplyr::ungroup(trust_wide)

# Combine: Verification+Trust+FB ------------------------------------------
virtras <- df_wide %>%
  left_join(trust_wide, by="id")

#virtras <- rbind(virtras,unvoll_virtras) #if we want the incompletete participants as well, but now we do not

data <- virtras %>%
  left_join(ind_dif_df, by="id")
data[c("id","Reliability")] <- lapply(data[c("id","Reliability")], as.factor)
head(data)
# Descriptive Statistics --------------------------------------------------

# Alter: SD, mean, von bis, Geschlechtverteilung, Erfahrung mit VR. KANN GRUPPIEREN UND WEG LASSEN,  
descriptive_stats <- data %>%
  group_by(Reliability) %>%
  summarise(
    Block1_mean = mean(Block1), Block1_sd = sd(Block1),
    Block2_mean = mean(Block2), Block2_sd = sd(Block2),
    Block3_mean = mean(Block3), Block3_sd = sd(Block3),
    Block4_mean = mean(Block4), Block4_sd = sd(Block4),
    Block5_mean = mean(Block5), Block5_sd = sd(Block5),
    Block6_mean = mean(Block6), Block6_sd = sd(Block6),
    Trust1_mean = mean(Trust.1), Trust1_sd = sd(Trust.1),
    Trust2_mean = mean(Trust.2), Trust2_sd = sd(Trust.2),
    Trust3_mean = mean(Trust.3), Trust3_sd = sd(Trust.3),
    Trust4_mean = mean(Trust.4), Trust4_sd = sd(Trust.4),
    Trust5_mean = mean(Trust.5), Trust5_sd = sd(Trust.5),
    Trust6_mean = mean(Trust.6), Trust6_sd = sd(Trust.6)
  )

setwd("../../") 

print("End of extracting trust and behavior data from CSV")