####data cleaning####
setwd("CSV_Data/Vollständige")

##Tutorial and Practice trial###


(file_list <- list.files(pattern = "\\.txt$"))
extract_data_from_file <- function(file) { 
  # code to extract data from file 
  data <- readLines(file) 
  # perform data extraction operations here 
  # e.g. strsplit(data, " ") 
  return(data) 
} 

#files that does not have the VP
# List all .txt files in the directory
file_list <- list.files(pattern = "\\.txt$")

# Filter files that do not contain "VP"
files_without_vp <- file_list[!grepl("VP", file_list)]

# Display the result
files_without_vp
print("delete these files in the folder because it is just test data, if no files are listed then ignore this warnung")



####now lets reload the code####
##it should be empty file list now:"
files_without_vp

# List all .txt files in the directory
file_list <- list.files(pattern = "\\.txt$")

# Extract the Block and VP information from each filename
file_df <- data.frame(filename = file_list)
file_df <- file_df %>%
  mutate(
    block = sub("^(Block\\d+)_.*", "\\1", filename),
    vp = sub(".*_VP_([0-9A-Za-z]+)_.*", "\\1", filename)
  )

# Count the number of files each VP has in each Block
vp_counts_by_block <- file_df %>%
  group_by(block, vp) %>%
  summarise(file_count = n(), .groups = 'drop')

# Display the result
vp_counts_by_block

# Extract the B

file_df <- file_df %>%
  mutate(
    block = sub("^(Block\\d+)_.*", "\\1", filename),
    vp = sub(".*_VP_([0-9A-Za-z]+)_.*", "\\1", filename)
  )

# Count the number of files each VP has in each Block and filter for counts > 1
double_files_by_block <- file_df %>%
  group_by(block, vp) %>%
  summarise(file_count = n(), .groups = 'drop') %>%
  filter(file_count > 1)

# Display the result
double_files_by_block

# Check if all file counts are 1
all(vp_counts_by_block$file_count == 1)
#### it should be true, if not, delete the files to make sure#
####now lets reload the code####
if (all(vp_counts_by_block$file_count == 1) == TRUE) {
  temp <- file_list
} else{
  print("double files in block")
  double_files_by_block
}

setwd("../../") 
print("End of part CSV cleaning")

