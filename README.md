# Master's Thesis — R Analysis Code
## Explaining Divergences in Trust and Dependence Behavior 
## in Human-Automation Interaction
**Thi An Nguyen Phan**  
Human Factors M.Sc., Technische Universität Berlin  
Institute of Psychology and Ergonomics  
Supervised by: Prof. Dr. Linda Onnasch & Steffen Hösterey  
Submitted in April 2025
---

## Overview

This repository contains all R scripts used for data preparation, 
model fitting, and visualization in the master's thesis. The study 
investigates individual differences (AICP, PTT, TSE), automation 
reliability, and exposure as predictors of trust attitude and 
dependence behavior in a VR-based human-automation interaction 
paradigm (VIRTRAS).

---

## Repository Structure

| File | Description |
|------|-------------|
| `RUN_ME_Masterscript.R` | Master script — runs the full 
analysis pipeline in order |
| `Load_packages.R` | Loads all required R packages |
| `0_Unvoll.R` | Initial/incomplete preprocessing steps (not included in the data analysis)|
| `1_DataCleaning.R` | Data cleaning and exclusion criteria |
| `2_Questionnaire_data.R` | Processing of questionnaire data 
(AICP, PTT, TSE, demographics) |
| `3_Assemble.R` | Merges behavioral and questionnaire 
datasets |
| `4_Models_results.R` | Main model fitting and hypothesis 
testing |
| `8_Descriptiveplots.R` | Descriptive statistics and 
visualization (violin plots etc.) |
| `Explorativ.R` | Exploratory analyses (VR experience, 
sensitivity analyses) |
| `model1a.R` | H1a — PTT + AICP predicting trust |
| `model1b.R` | H1b — PTT + AICP predicting dependence 
behavior |
| `model2a.R` | RQ2a — TSE as additional predictor of trust |
| `model2b.R` | RQ2b — TSE as additional predictor of 
dependence behavior |
| `model3a.R` | H3a — Reliability + Exposure predicting trust |
| `model3b.R` | H3b — Reliability + Exposure predicting 
dependence behavior |
| `model4a.R` | H4a — Reliability × Exposure interaction 
on trust |
| `model4b.R` | H4b — Reliability × Exposure interaction 
on dependence behavior |
| `CSV_Data` | Preprocessed extracted data (CSV format) |
| `data_vr-interaktion_preprocessed.xlsx` | Preprocessed raw 
data from VIRTRAS |

---

## How to Run

1. Open `RUN_ME_Masterscript.R`
2. Set your working directory to the repository root:
```r
setwd("your/path/here")
```
3. Run the master script — it calls all other scripts 
in the correct order.

Alternatively, run individual scripts in this order:
