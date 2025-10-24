# --- portability & data preamble ---
# Keep paths portable and fail with clear messages if data is missing.

# Packages
if (!requireNamespace("here", quietly = TRUE))  install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")

library(here)

message("Project root: ", here::here())

# Helper: build a data/ path & (optionally) assert existence
data_path <- function(..., must_exist = TRUE) {
  p <- here::here("data", ...)
  if (must_exist && !file.exists(p)) {
    stop("❌ Missing data file: ", normalizePath(p, winslash = "/"), call. = FALSE)
  }
  p
}

# If you’d rather warn (and continue) instead of stop, flip this flag:
.strict_data <- TRUE  # set FALSE to only warn

# Expected files (add more if needed)
DATA_FILES <- c(
  "BD_Original.xlsx",
  "DB_alphas_omegas_tasks.xlsx"
)

# Check presence up front (nice UX)
missing <- DATA_FILES[!file.exists(here::here("data", DATA_FILES))]
if (length(missing)) {
  msg <- paste0("The following required files are missing in data/: ",
                paste(missing, collapse = ", "))
  if (.strict_data) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
}

# Canonical paths you can reuse below
path_original <- data_path("BD_Original.xlsx", must_exist = !(!.strict_data && "BD_Original.xlsx" %in% missing))
path_alphas   <- data_path("DB_alphas_omegas_tasks.xlsx", must_exist = !(!.strict_data && "DB_alphas_omegas_tasks.xlsx" %in% missing))

# Optional: make View() safe in non-interactive renders (Quarto/Jupyter)
if (isTRUE(getOption("knitr.in.progress"))) {
  View <- function(...) { print(...); invisible(NULL) }
}


####################################################################################################
# Name: XXXXXXXXXX
# Author: Fabio Monteiro
# Purpose: Build clean, scored datasets for Working Memory (WM) and Fluid intelligence (Gf) tasks,
#          apply inclusion criteria, exclude low-quality participants, and handle univariate outliers.
#
# Notes:
# - This script reads multiple sheets from 'BD_Original.xlsx', computes task scores, merges, filters,
#   and exports cleaned data.
# - For reproducibility in a project, consider replacing setwd() with here::here().
####################################################################################################
#####Libraries used in this analysis
library(purrr)
library(semPlot)
library(dplyr)
library(semTools)
library(MVN)
library(mice)
library(readxl)
library(tibble)
library(openxlsx)
library(simsem)
library(psych)
library(sjmisc)
library(tidyr)
library(knitr)

###################################################################################################################################################################
#
#
#
#
###################################################################################################################################################################
#setwd("C:\\Users\\fabio\\OneDrive\\Área de Trabalho\\For_Github\\Data_Cleaning")

###################################################################################################################################################################
## ------------------------------------------------------------------------------------------------
## 1 - Reading the original data (each task is a separate sheet in the Excel file)
## ------------------------------------------------------------------------------------------------

## Working Memory (WM) tasks
# Each dataframe corresponds to a different WM task
df_RS <- readxl::read_excel(path_original, sheet = 'Reading_Span')
df_NB <- readxl::read_excel(path_original, sheet = 'N_Back')
df_OS <- readxl::read_excel(path_original, sheet = 'Operation_Span')
df_BT <- readxl::read_excel(path_original, sheet = 'Binding_Task')
df_MS <- readxl::read_excel(path_original, sheet = 'Multimodal_Span')
df_SS <- readxl::read_excel(path_original, sheet = 'Symmetry Span')
df_UT <- readxl::read_excel(path_original, sheet = 'WMU Task')

## Gf (Fluid Intelligence) tasks
df_NS <- readxl::read_excel(path_original, sheet = 'Number_Series')
df_RAPM <- readxl::read_excel(path_original, sheet = 'RAPM')
df_LS <- readxl::read_excel(path_original, sheet = 'Letter_Series')

#### ------------------------------------------------------------------------------------------------
# 2 - Creating dataframes containing sociodemographic info and accuracy scores (proportion correct) per participant in each WM and Gf task
#### ------------------------------------------------------------------------------------------------

#### WM tasks

# Reading Span
df_RS_scores <- df_RS %>%
  group_by(subject_nr) %>%
  slice_max(score_reading_span, n = 1, with_ties = FALSE)
df_RS_scores <- df_RS_scores["score_reading_span"]
df_RS_scores$score_reading_span <- round(df_RS_scores$score_reading_span, 2)

# N-back
df_NB_scores <- df_NB %>%
  group_by(subject_nr) %>%
  slice_max(TotalCorrectTwoBack, n = 1, with_ties = FALSE)
df_NB_scores <- df_NB_scores["TotalCorrectTwoBack"]/12
df_NB_scores$TotalCorrectTwoBack <- round(df_NB_scores$TotalCorrectTwoBack, 2)

# Operation Span
df_OS_scores <- df_OS %>%
  group_by(subject_nr) %>%
  slice_max(score_operation_span, n = 1, with_ties = FALSE)
df_OS_scores <- df_OS_scores["score_operation_span"]
df_OS_scores$score_operation_span <- round(df_OS_scores$score_operation_span, 2)

# Binding Task
df_BT_scores <- df_BT %>%
  group_by(subject_nr) %>%
  slice_max(BindingRawScore, n = 1, with_ties = FALSE)
df_BT_scores <- df_BT_scores["BindingRawScore"]/16
df_BT_scores$BindingRawScore <- round(df_BT_scores$BindingRawScore, 2)

# Multimodal Span
df_MS_filtered <- df_MS %>%
  filter(practice != "yes")
df_MS_scores <- df_MS_filtered %>%
  group_by(subject_nr) %>%
  slice_max(MultimodalSpanScore, n = 1, with_ties = FALSE)
df_MS_scores <- df_MS_scores["MultimodalSpanScore"]/11
df_MS_scores$MultimodalSpanScore <- round(df_MS_scores$MultimodalSpanScore, 2)

#Symmetry Span
df_SS_scores <- df_SS %>%
  group_by(subject_nr) %>%
  slice_max(score_symmetry_span, n = 1, with_ties = FALSE)
df_SS_scores <- df_SS_scores["score_symmetry_span"]
df_SS_scores$score_symmetry_span <- round(df_SS_scores$score_symmetry_span, 2)

# Updating Task
df_UT_scores <- df_UT %>%
  group_by(subject_nr) %>%
  slice_max(WMUExperimentalScore, n = 1, with_ties = FALSE)
df_UT_scores <- df_UT_scores["WMUExperimentalScore"]/36
df_UT_scores$WMUExperimentalScore <- round(df_UT_scores$WMUExperimentalScore, 2)

########################################
## Create dataframe combining sociodemographic variables and WM task scores
########################################

# Extract unique sociodemographic info
df_sociodemographic <- distinct(df_RS[c(1:10)])

# Merge sociodemographics with WM task scores
df_WM_Tasks <- cbind(df_sociodemographic, df_RS_scores,df_NB_scores,df_OS_scores,df_BT_scores,df_MS_scores,df_SS_scores,df_UT_scores)

# Rename columns for clarity
colnames(df_WM_Tasks)[c(11:17)] <- c("Reading_Span", "NBack", "Operation_Span","Binding_Task", "Multimodal_Span", "Symmetry_Span","WM_Updating_Task")
dplyr::glimpse(df_WM_Tasks) 
knitr::kable(head(df_WM_Tasks, 10))

####------------------------------------------------------------------------------------------------
#### 3 - Creating dataframes containing sociodemographic info and accuracy scores (proportion correct) per participant in each WM and Gf task
###------------------------------------------------------------------------------------------------

#Number Series
df_NS_scores <- df_NS %>%
  group_by(subject_nr) %>%
  slice_max(ExpermentalNumberSeriesScore, n = 1, with_ties = FALSE)
df_NS_scores <- df_NS_scores["ExpermentalNumberSeriesScore"]/15
df_NS_scores$ExpermentalNumberSeriesScore <- round(df_NS_scores$ExpermentalNumberSeriesScore, 2)

#RAPM
df_RAPM_scores <- df_RAPM %>%
  group_by(subject_nr) %>%
  slice_max(ExperimentalRAPMScore, n = 1, with_ties = FALSE)
df_RAPM_scores <- df_RAPM_scores["ExperimentalRAPMScore"]/18
df_RAPM_scores$ExperimentalRAPMScore <- round(df_RAPM_scores$ExperimentalRAPMScore, 2)

#Letter Series
df_LS_scores <- df_LS %>%
  group_by(subject_nr) %>%
  slice_max(ExperimentalLetterSeriesScore, n = 1, with_ties = FALSE)
df_LS_scores <- df_LS_scores["ExperimentalLetterSeriesScore"]/15
df_LS_scores$ExperimentalLetterSeriesScore <- round(df_LS_scores$ExperimentalLetterSeriesScore, 2)

# Combine sociodemographics with Gf task scores
df_Gf_Tasks <- cbind(df_sociodemographic, df_NS_scores,df_RAPM_scores,df_LS_scores)
colnames(df_Gf_Tasks)[c(11:13)] <- c("Number_Series", "RAPM", "Letter_Series")
dplyr::glimpse(df_Gf_Tasks)
knitr::kable(head(df_Gf_Tasks, 10))

##############################################################################################################################################################################
#### 4 - Exclusion of participants based on inclusion criteria
#################################################################

## 4.1 - Portuguese citizenship. Only participants with Portuguese nationality were kept.
### WM tasks
df_WM_Tasks <- df_WM_Tasks[df_WM_Tasks$nacionalidade %in% c("portuguesa", "Portuguesa", "portugues","Portugues","PORTUGUESA","PORTUGUES","pt","PT","Pt","portuguese","Portuguese",
                                                            "Portugal","portugal","PORTUGAL","prtuguesa","Portugu\\êsa"), ]

### Gf tasks
df_Gf_Tasks <- df_Gf_Tasks[df_Gf_Tasks$nacionalidade %in% c("portuguesa", "Portuguesa", "portugues","Portugues","PORTUGUESA","PORTUGUES","pt","PT","Pt","portuguese","Portuguese",
                                                            "Portugal","portugal","PORTUGAL","prtuguesa","Portugu\\êsa"), ]

##########################################################################
## 4.2 Age range. Only participants aged 18–35 were kept.
### WM tasks
df_WM_Tasks <- df_WM_Tasks[df_WM_Tasks$idade >= 18 & df_WM_Tasks$idade <= 35, ]

### Gf tasks
df_Gf_Tasks <- df_Gf_Tasks[df_Gf_Tasks$idade >= 18 & df_Gf_Tasks$idade <= 35, ]

##########################################################################
## 4.3 - Education. Excludes those who did not complete at least secondary education.
### WM tasks
df_WM_Tasks <- df_WM_Tasks %>%
  filter(!(freqEnsSup == "no" & HabNaoEstudante == "Nenhuma das opções anteriores se aplica"))

### Gf tasks
df_Gf_Tasks <- df_Gf_Tasks %>%
  filter(!(freqEnsSup == "no" & HabNaoEstudante == "Nenhuma das opções anteriores se aplica"))

##############################################################################################################################################################################
#### 5 - Exclusion of participants with 0 in more than one task
#################################################################

# Removes columns with sociodemographic info from the databases containing dta form the WM and Gf tasks
df_WM_Tasks <- df_WM_Tasks[c(1,11:17)]
df_Gf_Tasks <- df_Gf_Tasks[c(1,11:13)]

# Combine WM and Gf data
df_concat_WM_Gf <- cbind(df_WM_Tasks,df_Gf_Tasks[-c(1)])

# Identifies subjects with a score of zero in more than one task
subjects_with_multiple_zeros <- df_concat_WM_Gf %>%
  mutate(num_zeros = rowSums(across(-subject_nr, ~ . == 0))) %>%
  filter(num_zeros > 1) %>%
  distinct(subject_nr)

# Remove those subjects from WM and Gf datasets
### WM tasks
df_WM_Tasks <- df_WM_Tasks %>%
  filter(!subject_nr %in% subjects_with_multiple_zeros$subject_nr)

### Gf tasks
df_Gf_Tasks <- df_Gf_Tasks %>%
  filter(!subject_nr %in% subjects_with_multiple_zeros$subject_nr)

##############################################################################################################################################################################
#### 6 - Detects and removes univariate outliers (defined as values beyond the mean ± 3 standard deviations).
#################################################################

# Convert zeros scores to missing values
df_WM_Tasks[df_WM_Tasks == 0] <- NA

# Replace values beyond ±3 SD of the mean by NA
df_WM_Tasks <- df_WM_Tasks %>%
  mutate(across(
    -subject_nr,
    ~ ifelse(. < (mean(., na.rm = TRUE) - 3 * sd(., na.rm = TRUE)) |
               . > (mean(., na.rm = TRUE) + 3 * sd(., na.rm = TRUE)),
             NA, .)
  ))

dplyr::glimpse(df_WM_Tasks) 
knitr::kable(head(df_WM_Tasks, 10))

# Apply same procedure to Gf data
df_Gf_Tasks[df_Gf_Tasks == 0] <- NA
df_Gf_Tasks <- df_Gf_Tasks %>%
  mutate(across(
    -subject_nr,
    ~ ifelse(. < (mean(., na.rm = TRUE) - 3 * sd(., na.rm = TRUE)) |
               . > (mean(., na.rm = TRUE) + 3 * sd(., na.rm = TRUE)),
             NA, .)
  ))

dplyr::glimpse(df_Gf_Tasks)
knitr::kable(head(df_Gf_Tasks, 10))

###2 - Replacement of missing values through multiple imputation.
###Creates the predictor matrix that will be used to impute the missing values of each WM task. 
##The predictor matrix specifies which variables are used in the linear regression of each of the imputation models. 
##A value of 1 specifies that the variable given in the column name is used in the model to impute the variable given in the row name. 
##A value of  0 specifies that this variable is not used in this model.
imp_gen_df_WM_Tasks <- mice(df_WM_Tasks, print = FALSE)
pred1 <- imp_gen_df_WM_Tasks$predictorMatrix 
pred1[,"subject_nr"] <- 0
pred1["NBack","Multimodal_Span"] <- 0

##Creates the predictor matrix that will be used to impute the missing values of each Gf task. 
##The predictor matrix specifies which variables are used in the linear regression of each of the imputation models. 
##A value of 1 specifies that the variable given in the column name is used in the model to impute the variable given in the row name. 
##A value of  0 specifies that this variable is not used in this model.
imp_gen_df_Gf_Tasks <- mice(df_Gf_Tasks, print = FALSE)
pred2 <- imp_gen_df_Gf_Tasks$predictorMatrix 
pred2[,"subject_nr"] <- 0

###This section of the script uses multiple imputation to generate 20 databases in which the missing values of the WM tasks will be replaced by plausible values.
###The missing values replaced through multiple imputation will be slightly different each time you run this function. 
###If you are looking to replicate the statistical analysis presented in the article by Monteiro et al. (2024) you should use R's 'readxl::read_excel' function 
###to read the DB entitled "BDs_imput2_2nd.xlsx" (see line 247 of this file). This file contains the DBs  (generated through multiple imputation)  that were used 
###to compute all the analitical procedures described in the article.
imp_gen_df_WM_Tasks <- mice(data=df_WM_Tasks,
                     method = 'pmm',
                     m=20,
                     maxit=10,
                     predictorMatrix = pred1)

###This section of the script uses multiple imputation to generate 20 databases in which the missing values of the Gf tasks will be replaced by plausible values.
###The missing values replaced through multiple imputation will be slightly different each time you run this function. 
###If you are looking to replicate the statistical analysis presented in the article by Monteiro et al. (2024) you should use R's 'readxl::read_excel' function 
###to read the DB entitled "BDs_imput2_2nd.xlsx" (see line 247 of this file). This file contains the DBs  (generated through multiple imputation)  that were used 
###to compute all the analitical procedures described in the article.
imp_gen_df_Gf_Tasks <- mice(data=df_Gf_Tasks,
                     method = 'pmm',
                     m=20,
                     maxit=10,
                     predictorMatrix = pred2)


###Creates the variables mice.imp_DB and fills them with the DB that were generated through multiple imputation.
mice.imp_df_WM_Tasks <- NULL
mice.imp_df_Gf_Tasks <- NULL
mice.imp_df_WM_Gf_Tasks <- NULL
ParaMaha <- NULL
for(i in 1:20) mice.imp_df_WM_Tasks[[i]] <- complete(imp_gen_df_WM_Tasks, action=i, inc=FALSE)
subject_id <- mice.imp_df_WM_Tasks[[1]][,1]
for(i in 1:20) mice.imp_df_WM_Tasks[[i]] <- mice.imp_df_WM_Tasks[[i]][,-c(1)]
for(i in 1:20) ParaMaha[[i]] <- complete(imp_gen_df_WM_Tasks, action=i, inc=FALSE)
for(i in 1:20) ParaMaha[[i]] <- ParaMaha[[i]][,-c(1)]
for(i in 1:20) mice.imp_df_Gf_Tasks[[i]] <- complete(imp_gen_df_Gf_Tasks, action=i, inc=FALSE)
for(i in 1:20) mice.imp_df_Gf_Tasks[[i]] <- mice.imp_df_Gf_Tasks[[i]][,-c(1)]
for(i in 1:20) mice.imp_df_WM_Gf_Tasks[[i]] <- add_column(mice.imp_df_WM_Tasks[[i]],mice.imp_df_Gf_Tasks[[i]],.after = 8)
for(i in 1:20) mice.imp_df_WM_Gf_Tasks[[i]] <- data.frame(mice.imp_df_WM_Gf_Tasks[[i]])
dplyr::glimpse(mice.imp_df_WM_Gf_Tasks[[1]])
knitr::kable(head(mice.imp_df_WM_Gf_Tasks[[1]], 10))

###Calculates the Mahalonobis Distance and the respective p-value for each participant.
for(i in 1:20) mice.imp_df_WM_Gf_Tasks[[i]]$Mahalanobis_WMC_Gf <- mahalanobis(mice.imp_df_WM_Gf_Tasks[[i]],colMeans(mice.imp_df_WM_Gf_Tasks[[i]]),cov(mice.imp_df_WM_Gf_Tasks[[i]]))
for(i in 1:20) mice.imp_df_WM_Gf_Tasks[[i]]$pvalue_WMC_Gf <- pchisq(mice.imp_df_WM_Gf_Tasks[[i]]$Mahalanobis, df=9, lower.tail=FALSE)
for(i in 1:20) mice.imp_df_WM_Gf_Tasks[[i]] <- add_column(mice.imp_df_WM_Gf_Tasks[[i]],subject_id,.before = 1)
dplyr::glimpse(mice.imp_df_WM_Gf_Tasks[[10]])
knitr::kable(head(mice.imp_df_WM_Gf_Tasks[[10]], 10))

#############################################################################################################
# Extract only subject_id and pvalue_WMC_Gf columns from each imputed dataset
pvals_list <- lapply(mice.imp_df_WM_Gf_Tasks, function(df) df %>% select(subject_id, pvalue_WMC_Gf))

# Merge them all into one dataframe by subject_id
pvals_merged <- reduce(pvals_list, left_join, by = "subject_id")

# Compute the average p-value across the 20 datasets for each subject
pvals_merged <- pvals_merged %>%
  mutate(mean_pvalue = rowMeans(select(., starts_with("pvalue_WMC_Gf")), na.rm = TRUE))

# Identify subjects to exclude (average p < .05)
subjects_to_exclude <- pvals_merged %>%
  filter(mean_pvalue < 0.05) %>%
  pull(subject_id)

# Exclude those subjects from all 20 imputed datasets
for (i in 1:20) {
  mice.imp_df_WM_Gf_Tasks[[i]] <- mice.imp_df_WM_Gf_Tasks[[i]] %>%
    filter(!subject_id %in% subjects_to_exclude)
}

dplyr::glimpse(mice.imp_df_WM_Gf_Tasks[[1]])
knitr::kable(head(mice.imp_df_WM_Gf_Tasks[[1]], 10))
#####################################################################################################################################################################
#
#
#
#
###################################################################################################################################################################
##Generates a DB that includes pooled estimates of the 20 DBs included in the file Imputed_Databases_Validation_PT_version based on Rubin’s (1987) rules.
para_merge_WM_Tasks <- NULL
para_merge_Gf_Tasks <- NULL

subject_id <- mice.imp_df_WM_Gf_Tasks[[1]][,c(1)]
for (i in 1:20) para_merge_WM_Tasks[[i]] <- mice.imp_df_WM_Gf_Tasks[[i]][,c(2:8)]
for (i in 1:20) para_merge_Gf_Tasks[[i]] <- mice.imp_df_WM_Gf_Tasks[[i]][,c(9:11)]

para_merge_WM_Tasks <- para_merge_WM_Tasks[ vapply(para_merge_WM_Tasks, is.data.frame, logical(1)) ]

num_cols <- Reduce(
  intersect,
  lapply(para_merge_WM_Tasks, function(d) names(d)[vapply(d, is.numeric, logical(1))])
)

# 3) Build a 3D array [rows x cols x imputations] for numeric columns
arr <- simplify2array(lapply(para_merge_WM_Tasks, function(d) as.matrix(d[num_cols])))

# 4) Cell-wise mean across imputations (ignore NAs)
para_merge_WM_Tasks <- as.data.frame(apply(arr, c(1, 2), mean, na.rm = TRUE))


para_merge_Gf_Tasks <- para_merge_Gf_Tasks[ vapply(para_merge_Gf_Tasks, is.data.frame, logical(1)) ]

num_cols <- Reduce(
  intersect,
  lapply(para_merge_Gf_Tasks, function(d) names(d)[vapply(d, is.numeric, logical(1))])
)

# 3) Build a 3D array [rows x cols x imputations] for numeric columns
arr <- simplify2array(lapply(para_merge_Gf_Tasks, function(d) as.matrix(d[num_cols])))

# 4) Cell-wise mean across imputations (ignore NAs)
para_merge_Gf_Tasks <- as.data.frame(apply(arr, c(1, 2), mean, na.rm = TRUE))

df_combined_WM_Gf_tasks <- cbind(subject_id,para_merge_WM_Tasks,para_merge_Gf_Tasks)
df_combined_WM_Gf_tasks <- df_combined_WM_Gf_tasks %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
dplyr::glimpse(df_combined_WM_Gf_tasks)
knitr::kable(head(df_combined_WM_Gf_tasks, 10))
################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
##4 - Assessment of univariate and multivariate normality in the distribution of the WM and Gf scores.

##Univariate and multivariate normality were assessed in each of the 20 imputed datasets included in the file Imputed_Databases_Validation_PT_version.xlsx.
##We considered the following criteria to evaluate univariate normality: Skewness < 2; Kurtosis < 7.
##We considered the following criteria to evaluate multivariate normality: Kurtosis<3, or significant Mardia's skewness coefficient (p-value > 0.05).

paraNomr <- NULL
MultNormalVar <- NULL
###Generated 20 DBs that only include the scores of the WM and the Gf tasks.
for(i in 1:20) paraNomr[[i]] <- mice.imp_df_WM_Gf_Tasks[[i]][,-c(1,12,13)]
###Computes Mardia's Skewness coefficients and assess the skewness and kurtosis values of each of the 20 BDs. 
for(i in 1:20) MultNormalVar[[i]]<- mvn(paraNomr[[i]], mvnTest = "mardia")
MultNormalVar[1]

##Generates two columns displaying pooled estimates of skewness and kurtosis for the distribution of each WM and Gf task
##This values are presented in Table 1 of the article by Monteiro et al. (2024).
Megred_MultNormVar <- df_combined_WM_Gf_tasks[,-c(1)]
Megred_MultNormVar <- mvn(df_combined_WM_Gf_tasks,mvnTest = "mardia")
Megred_MultNormVar <- data.frame(Megred_MultNormVar$Descriptives)
Megred_MultNormVar <- Megred_MultNormVar[-c(1),c(9,10)]
dplyr::glimpse(Megred_MultNormVar)
knitr::kable(head(Megred_MultNormVar, 10))
################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
###5 - Computation of the reliability estimates (Cronbach's alpha and McDonalds' Omega).

##This section utilizes the information provided in the Excel file titled 'DB_alphas_omegas_tasks' to compute Cronbach's alpha and McDonald's Omega for each task.
##In this file, the score of every participant in every trial of each WM and Gf task is labeled as 1 (correct) or 0 (incorrect)
##Cronbach's alpha and McDonalds' Omega are computed at the level of individual trials.

###Reading Span
original_RS <- readxl::read_excel(path_alphas,sheet = "Reading_Span")
original_RS <- original_RS %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_RS <- original_RS[,-c(1)]
original_RS <- original_RS[-c(82),]
dplyr::glimpse(original_RS)
knitr::kable(head(original_RS, 10))

###Cronbach's Alpha
alpha_original_RS <- alpha(original_RS)
alpha_original_RS_CIs <- alpha.ci(original_RS,n.obs = 156,n.var = 60,digits = 2)
dplyr::glimpse(alpha_original_RS_CIs)
knitr::kable(head(alpha_original_RS_CIs, 10))

###McDonald's W
omega_original_RS <- omega(original_RS,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###Number Series
original_NSer <- readxl::read_excel(path_alphas,sheet = "Number_Series")
original_NSer <- original_NSer %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_NSer <- original_NSer[,-c(1)]
original_NSer <- original_NSer[-c(82),]
original_NSer_sItem2 <- original_NSer[,-c(2)]
dplyr::glimpse(original_NSer_sItem2)
knitr::kable(head(original_NSer_sItem2, 10))

###Cronbach's Alpha
alpha_original_NSer_sItem2 <- alpha(original_NSer_sItem2)
alpha_original_NSer_sItem2_CIs <- alpha.ci(original_NSer_sItem2,n.obs = 156,n.var = 15,digits = 2)
dplyr::glimpse(alpha_original_NSer_sItem2_CIs)
knitr::kable(head(alpha_original_NSer_sItem2_CIs, 10))

###McDonald's W
omega_original_sItem2 <- omega(original_NSer_sItem2,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###NBack
original_NBack <- readxl::read_excel(path_alphas,sheet = "N_Back")
original_NBack <- original_NBack %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_NBack <- original_NBack[,-c(1)]
original_NBack <- original_NBack[-c(82),]
dplyr::glimpse(original_NBack)
knitr::kable(head(original_NBack, 10))

###Cronbach's Alpha
alpha_original_NBack <- alpha(original_NBack)
alpha_original_NBack_CIs <- alpha.ci(original_NBack,n.obs = 156,n.var = 12,digits = 2)
dplyr::glimpse(alpha_original_NBack_CIs)
knitr::kable(head(alpha_original_NBack_CIs, 10))

###McDonald's W
omega_original_NBack <- omega(original_NBack,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###RAPM
original_RAPM <- readxl::read_excel(path_alphas,sheet = "RAPM")
original_RAPM <- original_RAPM %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_RAPM <- original_RAPM[,-c(1)]
original_RAPM <- original_RAPM[-c(82),]
dplyr::glimpse(original_RAPM)
knitr::kable(head(original_RAPM, 10))

###Cronbach's Alpha
alpha_original_RAPM <- alpha(original_RAPM)
alpha_original_RAPM_CIs <- alpha.ci(original_RAPM,n.obs = 156,n.var = 18,digits = 2)
dplyr::glimpse(alpha_original_RAPM_CIs)
knitr::kable(head(alpha_original_RAPM_CIs, 10))

###McDonald's W
omega_original_RAPM <- omega(original_RAPM,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###Operation Span
original_OS <- readxl::read_excel(path_alphas,sheet = "Operation_Span")
original_OS <- original_OS %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_OS <- original_OS[,-c(1)]
original_OS <- original_OS[-c(82),]
dplyr::glimpse(original_OS)
knitr::kable(head(original_OS, 10))

###Cronbach's Alpha
alpha_original_OS <- alpha(original_OS)
alpha_original_OS_CIs <- alpha.ci(original_OS,n.obs = 156,n.var = 60,digits = 2)
dplyr::glimpse(alpha_original_OS_CIs)
knitr::kable(head(alpha_original_OS_CIs, 10))

###McDonald's W
omega_original_OS <- omega(original_OS,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###Binding Task
original_BT <- readxl::read_excel(path_alphas,sheet = "Binding_Task")
original_BT <- original_BT %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_BT <- original_BT[,-c(1)]
original_BT <- original_BT[-c(82),]
dplyr::glimpse(original_BT)
knitr::kable(head(original_BT, 10))

###Cronbach's Alpha
alpha_original_BT <- alpha(original_BT)
alpha_original_BT_CIs <- alpha.ci(original_BT,n.obs = 156,n.var = 16,digits = 2)
dplyr::glimpse(alpha_original_BT_CIs)
knitr::kable(head(alpha_original_BT_CIs, 10))

###McDonald's W
omega_original_BT <- omega(original_BT,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###Multimodal Span
original_MS <- readxl::read_excel(path_alphas,sheet = "Multimodal_Span")
original_MS <- original_MS %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_MS <- original_MS[,-c(1)]
original_MS <- original_MS[-c(82),]
original_MS <- original_MS[,-c(5)]
dplyr::glimpse(original_MS)
knitr::kable(head(original_MS, 10))

###Cronbach's Alpha
alpha_original_MS <- alpha(original_MS)
alpha_original_MS_CIs <- alpha.ci(original_MS,n.obs = 156,n.var = 5,digits = 2)
dplyr::glimpse(alpha_original_MS_CIs)
knitr::kable(head(alpha_original_MS_CIs, 10))

###McDonald's W
omega_original_MS <- omega(original_MS,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###Letter Series
original_LSer <- readxl::read_excel(path_alphas,sheet = "Letter_Series")
original_LSer <- original_LSer %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_LSer <- original_LSer[,-c(1)]
original_LSer <- original_LSer[-c(82),]
dplyr::glimpse(original_LSer)
knitr::kable(head(original_LSer, 10))

###Cronbach's Alpha
alpha_original_LSer <- alpha(original_LSer)
alpha_original_LSer_CIs <- alpha.ci(original_LSer,n.obs = 156,n.var = 15,digits = 2)
dplyr::glimpse(alpha_original_LSer_CIs)
knitr::kable(head(alpha_original_LSer_CIs, 10))

###McDonald's W
omega_original_LSer <- omega(original_LSer,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###Symmetry Span
original_SS <- readxl::read_excel(path_alphas,sheet = "Symmetry Span")
original_SS <- original_SS %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_SS <- original_SS[,-c(1)]
original_SS <- original_SS[-c(82),]
#here
dplyr::glimpse(original_SS)
knitr::kable(head(original_SS, 10))

###Cronbach's Alpha
alpha_original_SS <- alpha(original_SS)
alpha_original_SS_CIs <- alpha.ci(original_SS,n.obs = 156,n.var = 60,digits = 2)
dplyr::glimpse(alpha_original_SS_CIs)
knitr::kable(head(alpha_original_SS_CIs, 10))

###McDonald's W
omega_original_SS <- omega(original_SS,nfactors = 1,fm="ml")

#####################################################################################################################################################################
###WM Updating Task
original_WMU <- readxl::read_excel(path_alphas,sheet = "WMU Task")
original_WMU <- original_WMU %>%
  filter(!subject_nr %in% subjects_to_exclude)
original_WMU <- original_WMU[,-c(1)]
original_WMU <- original_WMU[-c(82),]
dplyr::glimpse(original_WMU)
knitr::kable(head(original_WMU, 10))


###Cronbach's Alpha
alpha_original_WMU <- alpha(original_WMU)
alpha_original_WMU_CIs <- alpha.ci(original_WMU,n.obs = 156,n.var = 12,digits = 2)
dplyr::glimpse(alpha_original_WMU_CIs)
knitr::kable(head(alpha_original_WMU_CIs, 10))

###McDonald's W
omega_original_WMU <- omega(original_WMU,nfactors = 1,fm="ml")

####################################################################################################################################################################
##Generates the columns with the values of Cronbach's alpha and McDonald's Omega that will be used appended to the table with descriptive statistics.

##Column containing the Cronbach's alpha for each task.
aRS <- alpha_original_RS$total$raw_alpha
aNB <- alpha_original_NBack$total$raw_alpha
aOS <- alpha_original_OS$total$raw_alpha
aBT <- alpha_original_BT$total$raw_alpha
aMS <- alpha_original_MS$total$raw_alpha
aSS <- alpha_original_SS$total$raw_alpha
aWMU <- alpha_original_WMU$total$raw_alpha
aNSer <- alpha_original_NSer_sItem2$total$raw_alpha
aRAPM <- alpha_original_RAPM$total$raw_alpha
aLSer <- alpha_original_LSer$total$raw_alpha

col_alphas <- data.frame(c(aRS,aNB,aOS,aBT,aMS,aSS,aWMU,aNSer,aRAPM,aLSer))
dplyr::glimpse(col_alphas)
knitr::kable(head(col_alphas, 10))

##Column containing McDonalds' Omega for each task.
wRS <- omega_original_RS$omega.tot
wNB <- omega_original_NBack$omega.tot
wOS <- omega_original_OS$omega.tot
wBT <- omega_original_BT$omega.tot
wMS <- omega_original_MS$omega.tot
wSS <- omega_original_SS$omega.tot
wWMU <- omega_original_WMU$omega.tot
wNSer <- omega_original_sItem2$omega.tot
wRAPM <- omega_original_RAPM$omega.tot
wLSer <- omega_original_LSer$omega.tot

col_omegas <- data.frame(c(wRS,wNB,wOS,wBT,wMS,wSS,wWMU,wNSer,wRAPM,wLSer))
dplyr::glimpse(col_omegas)
knitr::kable(head(col_omegas, 10))
################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
##6 - Computes several descriptive statistics (mean, St Dev, St Error, variance) based on pooled estimates of the scores in the WM and Gf tasks. 
##The results of this analysis are presented in Table 1 of the article written by Monteiro et al. (2024).

###Calculates the mean of the WM and Gf tasks and the respective covariance matrix.
dplyr::glimpse(paraNomr)
knitr::kable(head(paraNomr, 10))
descriptive_stats <- fmi(paraNomr)

###Selects the diagonal of the covariance martix.
aux_calc_dev <- data.frame()
for(i in 1:10) aux_calc_dev[i,1] <- descriptive_stats$Covariances$coef[i,i]
###Calculates the St Dev for each WM and Gf task.
for(i in 1:10) aux_calc_dev[i,2] <- sqrt(aux_calc_dev[i,1])
###Calculates the St Error for each WM and Gf task.
for(i in 1:10) aux_calc_dev[i,3] <- aux_calc_dev[i,2]/sqrt(156)
dplyr::glimpse(aux_calc_dev)
knitr::kable(head(aux_calc_dev, 10))

###Generates a table with the descriptive statistcs.
descriptive_stats <- descriptive_stats$Means$coef
descriptive_stats <- data.frame(descriptive_stats)
descriptive_stats <- add_column(descriptive_stats,aux_calc_dev[,2],.after=1)
descriptive_stats <- add_column(descriptive_stats,aux_calc_dev[,3],.after=2)
descriptive_stats <- add_column(descriptive_stats,aux_calc_dev[,1],.after=3)
TaskNames <- c("Reading Span","N-Back Task","Operation Span","Binding Task","Multimodal Span","Symmetry Span","WM Updating Task","Number Series","RAPM","Letter Series")
descriptive_stats <- add_column(descriptive_stats,TaskNames,.before =1)
colnames(descriptive_stats) <- c("Task_Names","Mean","SD","SE","Var")
dplyr::glimpse(descriptive_stats)
knitr::kable(head(descriptive_stats, 10))

###Computes the "Range" of values (minimum score - maximum score) for each WM and Gf task.
MaxMatrix <- matrix(data = NA, ncol = 10,nrow = 20)
for(i in 1:20) for(j in 1:10) MaxMatrix[i,j] <- max(paraNomr[[i]][,j])
MaxColumn <- NULL
for(i in 1:10) MaxColumn[i] <- max(MaxMatrix[,i])
MaxColumn <- data.frame(MaxColumn)
for(i in 1:10) MaxColumn[i,] <- round(MaxColumn[i,],digits = 2)
for(i in 1:10) MaxColumn[i,] <- toString(MaxColumn[i,])
dplyr::glimpse(MaxColumn)
knitr::kable(head(MaxColumn, 10))
MinMatrix <- matrix(data = NA, ncol = 10,nrow = 20)
for(i in 1:20) for(j in 1:10) MinMatrix[i,j] <- min(paraNomr[[i]][,j])
MinColumn <- NULL
for(i in 1:10) MinColumn[i] <- min(MinMatrix[,i])
MinColumn <- data.frame(MinColumn)
for(i in 1:10) MinColumn[i,] <- round(MinColumn[i,],digits = 2)
for(i in 1:10) MinColumn[i,] <- toString(MinColumn[i,])
dplyr::glimpse(MinColumn)
knitr::kable(head(MinColumn, 10))
RangExpTasks <- MaxColumn
for(i in 1:10) RangExpTasks[i,] <- paste(MinColumn[i,],"-",MaxColumn[i,])
dplyr::glimpse(RangExpTasks)
knitr::kable(head(RangExpTasks, 10))

##Generates a table similar to Table 1 of the article by Monteiro et al. (2024).
##This table includes descriptive statistics (Mean,	St Dev,	Range	Skewness,	Kurtosis) and reliability estimates of the WM and Gf measures. 
descriptive_stats <- descriptive_stats[,-c(4,5)]
descriptive_stats <- add_column(descriptive_stats, Megred_MultNormVar,.after  = 3)
descriptive_stats <- add_column(descriptive_stats,RangExpTasks,.after = 3)
descriptive_stats <- add_column(descriptive_stats,col_alphas,.after = 6)
descriptive_stats <- add_column(descriptive_stats,col_omegas,.after = 7)
descriptive_stats <- descriptive_stats %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
colnames(descriptive_stats) <- c("Task Names","Mean","SD","Range","Skewness","Kurtosis","Alpha","Omega")
descriptive_stats <- descriptive_stats[c(1,3,6,2,7,5,4,10,8,9),]
rownames(descriptive_stats) <- NULL
dplyr::glimpse(descriptive_stats)
knitr::kable(head(descriptive_stats, 10))
################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
###7 - Generates a table with quartile, tercile, and median-based percentiles for the WM tasks as it is presented in Table 2 of the article by Monteiro et al. (2024).

percentils <- NULL
percentils <- data.frame(apply(df_combined_WM_Gf_tasks,2,quantile,probs=c(0.05,0.25,0.33,0.50,0.66,0.75,0.95),na.rm=TRUE))
Task_Names_1 <- rownames(percentils)
percentils <- add_columns(percentils,Task_Names_1,.before =1)
percentils <- percentils[,-c(2)]
percentils <- percentils[,c(1,2,4,7,3,8,6,5,11,9,10)]
percentils <- data.frame(percentils)

percentils <- percentils[,c(1,5,8,6,3,10,4,7)]
percentils <- percentils %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

TaskNames <- c("Percentile","Reading Span","Operation Span","Symmetry Span","N-Back Task","WM Updating Task","Multimodal Span","Binding Task")
colnames(percentils) <- TaskNames
dplyr::glimpse(percentils)
knitr::kable(head(percentils, 10))

################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
##8 - Computation of the correlation and covariation matrices of the WM tasks included in the OpenWMB.

##Generates a table similar to Table 3 presented in the article by Monteiro et al. (2024). 
head1<- NULL
pval1 <- NULL
corr_WMC <- miceadds::micombine.cor(mi.res=paraNomr, variables=c(1:10))
pval1 <- corr_WMC[,c(9)]
corr_WMC <- corr_WMC[,-c(4:11)]
corr_WMC <- spread(corr_WMC, variable1, r)
for(i in 1:10) corr_WMC[c(i),c(i+1)] <- 1.00
corr_WMC <- corr_WMC[,c(1,9,7,10,5,11,4,2)]
corr_WMC <- corr_WMC[c(8,6,9,4,10,3,1),]
corr_WMC <- corr_WMC %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

rownames(corr_WMC) <- NULL
colnames(corr_WMC) <- c("Task","Reading Span","Operation Span","Symmetry Span","N-Back Task","WM Updating Task","Multimodal Span","Binding Task")
corr_WMC[,c(1)] <- c("Reading Span","Operation Span","Symmetry Span","N-Back Task","WM Updating Task","Multimodal Span","Binding Task")
dplyr::glimpse(corr_WMC)
knitr::kable(head(corr_WMC, 10))

################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
##9 - Computation of the EFA that evaluated the underlying structure of the WM tasks. 

merged_imp_gen_for_efa <- df_combined_WM_Gf_tasks[2:8]
dplyr::glimpse(merged_imp_gen_for_efa)
knitr::kable(head(merged_imp_gen_for_efa, 10))

##Computes the descriptive statistics for the variables that will be subjected to EFA (Reading Span, Operation Span, Symmetry Span, N-back task, Updating task,
##Multimodal Span, and Binding Task).
summary(merged_imp_gen_for_efa)

##Calculates the Kaiser-Meyer-Olkin measure (KMO). 
##A KMO above .50 suggest that the characteristics of the data are suitable for factorial analysis (Kaiser & Rice, 1974). 
##The result of the KMO (.83) suggested that our data was adequate to conduct an EFA. 
KMO(merged_imp_gen_for_efa)

##Calculates Bartlett's test of sphericity. 
##A significant Bartlett's test of sphericity suggests that the characteristics of the data are suitable for factorial analysis.
##The results of Bartlett’s test of sphericity (χ2(21) = 301.53, p < .001) suggested that our data was adequate to conduct this analysis.
cortest.bartlett(merged_imp_gen_for_efa)

##Calculates eigenvalues
##We applied Kaiser’s criterion (1970) to decide how many factors to retain (eigenvalues > 1).
ev <- eigen(cor(merged_imp_gen_for_efa))
ev$values

##Computes a scree test to evaluate how many factors should be extracted.
scree(merged_imp_gen_for_efa, pc=FALSE)

##Computes a parallel analysis to evaluate how many factors should be extracted.
fa.parallel(merged_imp_gen_for_efa, fa="pc")

##The results of the scree test, the parallel analysis, and Kaiser’s criterion (1970) suggested that a single factor was enough to accommodate all WM tasks.

##Assesses the fit of the unifactorial structure of WMC and estimates the loadings of each task in this factor.
##Method of extraction = Maximum Likelihood, Rotation = Promax.
fit <- factanal(merged_imp_gen_for_efa, factors = 1, rotation="promax")
print(fit, digits=3, cutoff=0.4, sort=TRUE)
loads <- fit$loadings
fa.diagram(loads)     
##The unifactorial structure accounted for 38% of the variance in the WM tasks. 
##All tasks presented acceptable factor loadings (> .40).

##Computes Cronbach's alpha for the unifactorial structure of WMC.
alpha(merged_imp_gen_for_efa)
################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
#10 - Computes the CFA that was used to confirm whether the general WMC factor extracted in the EFA presented an adequate 
#structure to accommodate all seven tasks included in the battery.

#Unifactorial model of WMC (labeled as Model 1 in the article by Monteiro et al. (2024)).
model1WMCGeneralFactor <-
  'WMC =~ NA*Reading_Span + Operation_Span + Symmetry_Span + NBack + WM_Updating_Task + Multimodal_Span + Binding_Task
  WMC ~~ 1*WMC'

#The function bellow fits the unifactorial model to the data.
fit_model1WMCGeneralFactor <- runMI(model1WMCGeneralFactor, 
                                    data=mice.imp_df_WM_Gf_Tasks,
                                    fun="cfa")

##Computes the fit indexes of the model (e. g., chi-square, CFI, RMSEA,SRMR).
aaa <- summary(fit_model1WMCGeneralFactor,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE)

##########################
#Computes the implied pooled covariance matrix.
model_implied_1WMCGeneralFactor_cov <- fitted(fit_model1WMCGeneralFactor, omit.imps = c("no.conv", "no.se"))
dplyr::glimpse(model_implied_1WMCGeneralFactor_cov)
knitr::kable(head(model_implied_1WMCGeneralFactor_cov, 10))

#Computes the implied pooled correlation matrix.
model_implied_1WMCGeneralFactor_corr <- data.frame(model_implied_1WMCGeneralFactor_cov)
colnames(model_implied_1WMCGeneralFactor_corr) <- c("Reading_Span","Operation_Span","Symmetry_span","NBack","WM_Updating_Task","Multimodal_Span","Binding_Task")
model_implied_1WMCGeneralFactor_corr <- data.matrix(model_implied_1WMCGeneralFactor_corr)
model_implied_1WMCGeneralFactor_corr <- cov2cor(model_implied_1WMCGeneralFactor_corr)
dplyr::glimpse(model_implied_1WMCGeneralFactor_corr)
knitr::kable(head(model_implied_1WMCGeneralFactor_corr, 10))

##Computes the standardized residual matrix of covariance. 
Std_residual_matrix_1WMCGenFactor <- cfa(model1WMCGeneralFactor,data = df_combined_WM_Gf_tasks, estimator = "ML")
resid(Std_residual_matrix_1WMCGenFactor,type="standardized")

##########################
#Generates a graph similar to the one presented in Figure 2 of the article by Monteiro et al.(2024).
#This plot displays the standardized factor loadings, squared multiple correlations, and standardized error terms of model 1.
fit_model_test1 <- cfa(model1WMCGeneralFactor, 
                       data=mice.imp_df_WM_Gf_Tasks[[1]],estimator="ML")
SEMPLOT <- semPlot::semPlotModel(fit_model_test1)
aaaa<- data.frame(summary(fit_model1WMCGeneralFactor,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE))
aaaa <- aaaa$std.all[-c(16:22)]
SEMPLOT@Pars$std <- aaaa
Rsq_1WMC <- summary(fit_model1WMCGeneralFactor,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE)
Rsq_1WMC <- data.frame(Rsq_1WMC)
for(i in 1:length(Rsq_1WMC[,"std.all"])) Rsq_1WMC[i,"RSq"] <- as.character(round(Rsq_1WMC[i,"std.all"]^2,2))
vecNames1WMC <- c("RS","OS","SS","NB","UT","MS","BT") 
vecRsq1WMC <- Rsq_1WMC[1:7,"RSq"]
vecIndi1WMC <- paste(vecNames1WMC," (",vecRsq1WMC,")")
vecIndi1WMC[8] <- "WMC"
vecIndi1WMC[5] <- "UT ( 0.60 )"
vecIndi1WMC <- c("RS (.47)","OS (.48)","SS (.46)","NB (.23)","UT (.60)","MS (.18)","BT (.18)", "WMC")
Path1WMC <- semPaths(SEMPLOT,'std',intercepts = TRUE, residuals = TRUE, layout = "tree",edge.color = "black",edge.label.color = "black",rotation = 4,sizeMan = 10,sizeLat = 12,
                     nodeLabels = vecIndi1WMC, optimizeLatRes = TRUE, shapeLat = "ellipse")
png(file= "Model1.png",width=4096, height=3277)
semPaths(SEMPLOT,'std',intercepts = TRUE, residuals = TRUE, layout = "tree", edge.color = "black",rotation = 4,sizeMan = 8,sizeLat = 10,
         nodeLabels = vecIndi1WMC, edge.width=0.7, fade = FALSE, fixedStyle = c("black"),freeStyle = c("black"),cut=1, ,edge.label.cex = 0.75,mar=c(4,4,4,4))
dev.off()
################################################################################################################################################################
#
#
#
#
###############################################################################################################################################################
##11 - Computes the SEM that was used to assess the correlation between the unifactorial model of  WMC extracted in the EFA and the latent Gf factor derived 
##from the Letter_Series, the Number_Series, and the RAPM.

##Model that was used to assess the correlation between the unifactorial model of WMC and the Gf factor (labeled as Model 2 in the article by Monteiro et al. (2024)).
model_1GfFactor_1WMCFactor <- 'WMC =~ NA*Reading_Span + Operation_Span + Symmetry_Span + NBack + WM_Updating_Task + Multimodal_Span + Binding_Task
              Gf =~ NA*RAPM + Letter_Series + Number_Series
              Gf ~ WMC
              WMC ~~ 1*WMC
              Gf ~~ 1*Gf'

#The function bellow fits the model to the data.
fit_model_1GfFactor_1WMCFactor <- runMI(model_1GfFactor_1WMCFactor, 
                                        data=mice.imp_df_WM_Gf_Tasks,
                                        fun="sem")

#Computes the fit indexes of the model (e. g., chi-square, CFI, RMSEA,SRMR).
summary(fit_model_1GfFactor_1WMCFactor,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE,test="D2")

##########################
##Computes the implied pooled covariance matrix.
model_implied_1GfFactor_1WMCFactor_cov <- fitted(fit_model_1GfFactor_1WMCFactor, omit.imps = c("no.conv", "no.se"))
View(data.frame(model_implied_1GfFactor_1WMCFactor_cov))
dplyr::glimpse(model_implied_1GfFactor_1WMCFactor_cov)
knitr::kable(head(model_implied_1GfFactor_1WMCFactor_cov, 10))

#Computes the implied pooled correlation matrix.
model_implied_1GfFactor_1WMCFactor_corr <- data.frame(model_implied_1GfFactor_1WMCFactor_cov)
colnames(model_implied_1GfFactor_1WMCFactor_corr) <- c("Reading_Span","Operation_Span","Symmetry_span","NBack","WM_Updating_Task","Multimodal_Span","Binding_Task")
model_implied_1GfFactor_1WMCFactor_corr <- data.matrix(model_implied_1GfFactor_1WMCFactor_corr)
model_implied_1GfFactor_1WMCFactor_corr <- cov2cor(model_implied_1GfFactor_1WMCFactor_corr)
dplyr::glimpse(model_implied_1GfFactor_1WMCFactor_corr)
knitr::kable(head(model_implied_1GfFactor_1WMCFactor_corr, 10))

###Computes the standardized residual matrix of covariance.
Std_residual_matrix_1GfFactor_1WMCFactor <- cfa(model_1GfFactor_1WMCFactor,data = df_combined_WM_Gf_tasks, estimator = "ML")
resid(Std_residual_matrix_1GfFactor_1WMCFactor,type="standardized")

##########################
#Generates a graph similar to the one presented in Figure 2 of the article by Monteiro et al.(2024).
#This plot displays the standardized factor loadings, squared multiple correlations, and standardized error terms of model 2.
fit_model_test3 <- sem(model_1GfFactor_1WMCFactor, 
                       data=mice.imp_df_WM_Gf_Tasks[[1]],estimator="ML")
SEMPLOT <- semPlot::semPlotModel(fit_model_test3)
aaaa<- data.frame(summary(fit_model_1GfFactor_1WMCFactor,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE))
aaaa <- aaaa$std.all[-c(24:34)]
aaaa2 <- aaaa
aaaa2[c(1:7)] <- rev(aaaa2[c(1:7)])
aaaa2[c(9:10)] <- rev(aaaa2[c(9:10)])
aaaa2[c(14:20)] <- rev(aaaa2[c(14:20)])
aaaa2[c(22:23)] <- rev(aaaa2[c(22:23)])
SEMPLOT@Pars$std <- aaaa2
Rsq_1WMC_1Gf <- summary(fit_model_1GfFactor_1WMCFactor,fit.measures = TRUE, standardized = TRUE,rsquare = TRUE)
Rsq_1WMC_1Gf <- data.frame(Rsq_1WMC_1Gf)
for(i in 1:length(Rsq_1WMC_1Gf[,"std.all"])) Rsq_1WMC_1Gf[i,"RSq"] <- as.character(round(Rsq_1WMC_1Gf[i,"std.all"]^2,2))
vecNames1WMC_1Gf <- c("RS","OS","SS","NB","UT","MS","BT","RAPM","L_Ser","N_Ser","Gf") 
vecRsq_1WMC_1Gf <- Rsq_1WMC_1Gf[1:11,"RSq"]
vecIndi1WMC_1Gf <- paste(vecNames1WMC_1Gf," (",vecRsq_1WMC_1Gf,")")
vecIndi1WMC_1Gf[12] <- "WMC"
vecIndi1WMC_1Gf[6] <- "MS ( 0.20 )"
vecIndi1WMC_1Gf <- c("BT (.21)","MS (.20)","UT (.65)","NB (.23)","SS (.43)","OS (.46)","RS (.42)","RAPM (.37)","NSer (.45)","LSer (.49)","WMC","Gf")
vecIndi1WMC_1Gf
semPaths(SEMPLOT,'std',intercepts = TRUE, residuals = TRUE, layout = "tree",edge.color = "black")
png(file= "Model2.png",width=4096, height=3277)
semPaths(SEMPLOT,'std',intercepts = TRUE, residuals = TRUE, layout = "tree", edge.color = "black",rotation = 2,sizeMan = 9,sizeLat = 11, optimizeLatRes = TRUE,
         nodeLabels = vecIndi1WMC_1Gf, edge.width=0.7, fade = FALSE, fixedStyle = c("black"),freeStyle = c("black"),cut=1, edge.label.cex = 0.75, mar = c(5,5,5,5))
dev.off()