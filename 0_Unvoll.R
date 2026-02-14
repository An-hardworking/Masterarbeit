setwd(r"(G:\Meine Ablage\Masterarbeit\Data Analysis\CSV_Data\Unvollständige)")
file_list <- list.files(pattern = "\\.txt$")
temp1 <-file_list

for (i in 1:length(temp1)) assign(temp1[i], read.csv(temp1[i]))
data_list <- mget(temp1)
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


#### now lets count how many TabFinishedMeasure columns are there in each block to each heights in Tab_finisched_data?

Tab_finish_data <- sorted_data %>%
  filter(taskname == "TabFinishedMeasure" & status== "InCapsule") %>%  # Filter for TabFinishedMeasure
  group_by(id, height, blockname) %>%                      # Group by ID and height
  summarize(count = n_distinct(value), .groups = "drop")  %>% 
  arrange(id, blockname)

# What are the values of trust to each block?
Singletrust <- sorted_data %>%
  filter(taskname == "TrustSingleItem") %>%  # Filter for TabFinishedMeasure
  group_by(id, blockname) %>%    #
  mutate(TrustSingleItem = value) %>% 
  arrange(id, blockname)
# Group by ID and height
Singletrust <- subset(Singletrust, select = -c(taskname, status, height, value))

########df for verification behavior##############

#summary of verified parameters to each block
df <- Tab_finish_data %>%
  group_by(id, blockname) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%  
  arrange(id, blockname)
#this data has a nachteil, if the VP did not verifiy any parameters, value=0 will not 
#be recorded in the data

## let s create a dataset to see, in each trial, how was the number of verified parameters?##

#Expand a grid to contain the data
all_blocks <- expand.grid(
  id = unique(sorted_data$id),
  blockname = 1:6
)

all_blocks$blockname <- as.character(all_blocks$blockname)
# Join with the original dataframe
unvoll_data <- all_blocks %>%
  left_join(df, by = c("id", "blockname")) # %>%
  #mutate(total_count = ifelse(is.na(total_count), 0, total_count))

# The result will have all participants with 6 blocks, missing blocks filled with 0
#print(df_complete)

##Transpose the datasets in wide form and name it "virtras"
#verifiedparameter
unvoll_df_wide <- unvoll_data %>%
  pivot_wider(
    names_from = blockname,
    values_from = total_count,
    names_prefix = "Block",
  )

# Create a full grid of id and blockname (1 to 6)
expanded_blocks <- expand.grid(
  id = unique(Singletrust$id),
  blockname = as.character(1:6)
)

# Left join with original data to ensure all combinations are included
unvoll_trust_wide <- expanded_blocks %>%
  left_join(Singletrust, by = c("id", "blockname")) %>%
  pivot_wider(
    names_from = blockname,
    values_from = TrustSingleItem,
    names_prefix = "Trust.",
    values_fill = list(TrustSingleItem = NA)
  )

# View the resulting data frame
unvoll_trust_wide

# well because trust_wide is grouped df, we need to ungroup the dataset to set it free 
unvoll_trust_wide <- dplyr::ungroup(unvoll_trust_wide)
unvoll_virtras <- unvoll_df_wide %>%
  left_join(unvoll_trust_wide, by="id")

library(dplyr)
library(tidyr)

# Assuming `unvoll_virtras` is the data in wide format, where columns like Block1 to Block6 and Trust.1 to Trust.6 exist.

# Iterate through each Trust and corresponding Block column to apply the rules.
unvoll_virtras <- unvoll_virtras %>%
  mutate(
    Block1 = ifelse(is.na(Trust.1), NA, replace_na(Block1, 0)),
    Block2 = ifelse(is.na(Trust.2), NA, replace_na(Block2, 0)),
    Block3 = ifelse(is.na(Trust.3), NA, replace_na(Block3, 0)),
    Block4 = ifelse(is.na(Trust.4), NA, replace_na(Block4, 0)),
    Block5 = ifelse(is.na(Trust.5), NA, replace_na(Block5, 0)),
    Block6 = ifelse(is.na(Trust.6), NA, replace_na(Block6, 0))
  )

# View the modified dataset
print(unvoll_virtras)
