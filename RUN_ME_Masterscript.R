# ─────────────────────────────────────────────────────────────
# Master Script for Master's Thesis
# Title: Divergences in Trust and Dependence Behavior
# von Thi An Nguyen Phan (Matrikelnr. 484567)
# Date: 2025-04-26
# ─────────────────────────────────────────────────────────────
####################################READ ME#####################################################
# The scripts are divided into several scripts with different purposes, 
#load the scripts one by one in the numerated order
#################################### START #####################################################

# 1. Set wd ------------------------------------------------------------------

#replace the folderpath from your computer
folderpath <- r"(G:\Meine Ablage\Masterarbeit\Data Analysis\DataAnalysis_Phan_484567)"
setwd(folderpath)  # update to your folder

####load packages####
source("R_script_of_ME/Load_packages.R")

# 2. Power Analysis ----------------------------------------------------------
#As we simulated 10 thousand times, it may take a while to load, 
#so you can load this script at the end or seperately
#source("R_script_of_ME/Poweranalysis/Re__Power_Analysis_Datensatz/PowerSimulation.R")

# 3. Clean up CSV Data
#the CSV folder should only contain data from test blocks, tutorial and practice data are not in CSV Data

#if we want to include data of participants who did not complete the experiment till the end
# but in this experiment, we have enough data of participants who completed the experiment
#till the end so we can ignore this.
#source("R_script_of_ME/0_Unvoll.R)")
source("R_script_of_ME/1_DataCleaning.R")

# 4. Questionnaire data ---------------------------------------------------
#we still have demographic data, TSE, AICP, PTT rating in from questionnaire,
#as the questionnaire also contains data from another study, so we had to filter out what we really need.

#we omitted the participants from the excel that are excluded from the analysis ("VP_2B", "VP_4B",  "VP_16A", "VP_34A", "VP_54A"))
source("R_script_of_ME/2_Questionnaire_data.R")

# 5. Translate CSV Data into numbers of verified parameters ---------------
source("R_script_of_ME/3_Assemble.R")

# 6. Data combination -----------------------------------------------------
#now let s combine csv data and questionnaire data for the models later on 
source("R_script_of_ME/4_Models_results.R")


# 7. Data analysis --------------------------------------------------------
# as there are tables exported during the code running, the folder Results_tables should contain the exported tables
save_location <- (r"(R_script_of_ME\Results_tables)")  # Replace with your desired folder path

  #Hypothesis1a
source("R_script_of_ME/model1a.R")
  #Hypothesis1b
source("R_script_of_ME/model1b.R")
  #Research question2a
source("R_script_of_ME/model2a.R")
  #Research question2b
source("R_script_of_ME/model2b.R")
  #Hypothesis3a
source("R_script_of_ME/model3a.R")
  #Hypothesis3b
source("R_script_of_ME/model3b.R")
  #Hypothesis4a
source("R_script_of_ME/model4a.R")
  #Hypothesis4b
source("R_script_of_ME/model4b.R")

# 8. Descriptive Plots ----------------------------------------------------
#This is how the descriptive plots were created
source("R_script_of_ME/8_Descriptiveplots.R")

# 9. Exploratory Findings -------------------------------------------------
#this is the script for explorativ findings, not CI tables are automatically created
source("R_script_of_ME/Explorativ.R")

#################################### END #####################################################

