# IMPORTING Data ###########################################################
data <- read.table("~/Desktop/GPN/Data/AI_Append/KLU_APC2_Master_2020_06_09_p012.csv", header = TRUE, sep = ",")

# Filter Data ##############################################################
data <- data[data$FaceNames_Exclude != "Yes" & data$Visit_Relative == 1,] #Issues with face name data and only 1 scan/subject - 87 observations

# Recode Variables ##############################################################
data$Race_cat <- data$Race != 'White' #not white = 1 (True)
data$Education_cat <- data$Education > 12  #higher education = 1 (True)
data$LETTER_FLUENCY <- (data$FLUENA + data$FLUENF+ data$FLUENS) / 3
data$STRINTERFERENCE <- (data$STRCW - data$STRCOL) / data$STRCOL
data$PiB_STATUS_CODE <- (data$PiBStatus_SUVR_GTM_FS_Global == "pos")
data$PiB_STATUS_CODE[data$PiBStatus_SUVR_GTM_FS_Global == ""] = NA
data$APOE_CODE[data$APOE_CODE == ""] = NA

# Cognitive Data Normalization ##################################################
#Memory
REYIM_Mean <- 15.4 #doi: 10.1136/jnnp.2004.045567
REYDE_Mean <- 14.7 #doi: 10.1136/jnnp.2004.045567
#Visiospatial
REYCO_Mean <-22.3 #doi: 10.1136/jnnp.2004.045567
BLOCKDES_Mean <- 11.4 #doi: 10.1136/jnnp.2004.045567
#Language
BOSTON1_Mean <- 26.9 #doi: 10.1136/jnnp.2004.045567
FLUEN_Mean <- 15.6 #doi: 10.1136/jnnp.2004.045567
#Executive/Attention
TRAILAS_Mean <- 45.6 #doi: 10.1136/jnnp.2004.045567
TRAILBS_Mean <-107.5 #doi: 10.1136/jnnp.2004.045567
SPANSF_Mean <- 6.4 #doi: 10.1136/jnnp.2004.045567
SPANSB_Mean <- 4.4 #doi: 10.1136/jnnp.2004.045567
DIGSYMWR_Mean <- 46.8 #doi: 10.1136/jnnp.2004.045567

#Citation Notes: - add n values
#Our sample: avg age: 74.8; >12 yrs education: 72%, avg. education: 14.897

#doi: 10.1136/jnnp.2004.045567 - avg. age (for normal subjects): 79.5; >12 yrs education: 61.5%
#Most means from this source are more low/poor compared to our sample

# Z Transform ####################################################################
#Negative z value means that lower value = higher performance
# doi:10.1016/j.jalz.2017.12.003 - method of composite calculation
#doi/ 10.1136/jnnp.2004.045567 - standard deviations from normative data
REYIM_Z <- (data$REYIM-REYIM_Mean) / 4.8
REYDE_Z <- (data$REYDE-REYDE_Mean) / 4.8
REYCO_Z <- (data$REYCO-REYCO_Mean) / 2.1
BLOCKDES_Z <- (data$BLOCKDES-BLOCKDES_Mean) / 4.8
BOSTON1_Z <-(data$BOSTON1-BOSTON1_Mean) / 2.6
FLUEN_Z <- (data$FLUEN-FLUEN_Mean) / 4.8
TRAILAS_Z <- (data$TRAILAS-TRAILAS_Mean) / 17.5
TRAILAS_Z_INV <- -1*TRAILAS_Z
SPANSF_Z <- (data$SPANSF-SPANSF_Mean) / 1.2
SPANSB_Z <- (data$SPANSB-SPANSB_Mean) / 1.2
TRAILBS_Z <- (data$TRAILBS-TRAILBS_Mean) / 49.3
TRAILBS_Z_INV <- -1*TRAILBS_Z
DIGSYMWR_Z <- (data$DIGSYMWR-DIGSYMWR_Mean) / 12.3

# Domain Scores #########################################################################
#doi:10.1016/j.jalz.2017.12.003., doi:10.1080/13607860903071014. (Both Beth Snitz articles), https://www.ncbi.nlm.nih.gov/books/NBK285344/ - for SPANSB in Executive
data$memory <- (REYIM_Z + REYDE_Z) /2
data$visiospatial <- (REYCO_Z + BLOCKDES_Z)/2
data$language <- (BOSTON1_Z +FLUEN_Z) / 2
data$executive <- (TRAILBS_Z_INV +SPANSB_Z) / 2
data$attention <- (TRAILAS_Z_INV + SPANSF_Z) / 2
data$executive_attention <- (TRAILAS_Z_INV + TRAILBS_Z_INV + SPANSF_Z + SPANSB_Z + DIGSYMWR_Z) / 5

#Intraclass Correlation ################################################################
# Pearson (Linear Correlation between composite and raw scores)
library("irr")
#Memory
REYIM_Pearson_Correlation <- cor(data$memory, data$REYIM, use = "complete.obs")
REYDE_Pearson_Correlation <- cor(data$memory, data$REYDE, use = "complete.obs")

# Visiospatial
BLOCKDES_Pearson_Correlation <- cor(data$visiospatial, data$BLOCKDES, use = "complete.obs")
REYCO_Pearson_Correlation <- cor(data$visiospatial, data$REYCO, use = "complete.obs")

#Langugae
BOSTON1_Pearson_Correlation <- cor(data$language, data$BOSTON1, use = "complete.obs")
FLUEN_Pearson_Correlation <- cor(data$language, data$FLUEN, use = "complete.obs")

#Executive
TRAILBS_Pearson_Correlation <- cor(data$executive, -1*data$TRAILBS, use = "complete.obs")
SPANSB_Pearson_Correlation <- cor(data$executive, data$SPANSB, use = "complete.obs")

#Attention
TRAILAS_Pearson_Correlation <- cor(data$attention, -1*data$TRAILAS, use = "complete.obs")
SPANSF_Pearson_Correlation <- cor(data$attention, data$SPANSF, use = "complete.obs")

#Executive_Attention
TRAILBS_Combo_Pearson_Correlation <- cor(data$executive_attention, -1*data$TRAILBS, use = "complete.obs")
TRAILAS_Combo_Pearson_Correlation <- cor(data$executive_attention, -1*data$TRAILAS, use = "complete.obs")
SPANSF_Combo_Pearson_Correlation <- cor(data$executive_attention, data$SPANSF, use = "complete.obs")
DIGSYMWR_Combo_Pearson_Correlation <- cor(data$executive_attention, data$DIGSYMWR, use = "complete.obs")
SPANSB_Combo_Pearson_Correlation <- cor(data$executive_attention, data$SPANSB, use = "complete.obs")

#Interclass Correlation (correlation between z-scores within composite score) ########3
#https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/
memory_icc_scores <-  cbind(REYIM_Z, REYDE_Z)
memory_icc_values <- icc(memory_icc_scores, model = "twoway", type = "agreement", unit = "single")
memory_icc <- memory_icc_values$value

visiospatial_icc_scores <- cbind(REYCO_Z,BLOCKDES_Z)
visiospatial_icc_values <- icc(visiospatial_icc_scores, model = "twoway", type = "agreement", unit = "single")
visiospatial_icc <- visiospatial_icc_values$value

language_icc_scores <- cbind(BOSTON1_Z,FLUEN_Z)
language_icc_values <- icc(language_icc_scores, model = "twoway", type = "consistency", unit = "single")
language_icc <- language_icc_values$value

executive_icc_scores <- cbind(TRAILBS_Z_INV,SPANSB_Z)
executive_icc_values <- icc(executive_icc_scores, model = "twoway", type = "consistency", unit = "single")
executive_icc <- executive_icc_values$value

attention_icc_scores <- cbind(TRAILAS_Z_INV,SPANSF_Z)
attention_icc_values <- icc(attention_icc_scores, model = "twoway", type = "consistency", unit = "single")
attention_icc <- attention_icc_values$value

executive_attention_icc_scores <- cbind(TRAILAS_Z_INV,TRAILBS_Z_INV,SPANSF_Z,SPANSB_Z,DIGSYMWR_Z)
executive_attention_icc_values <- icc(executive_attention_icc_scores, model = "twoway", type = "consistency", unit = "single")
executive_attention_icc <- executive_attention_icc_values$value

# Association with AI ####################################################################
mdll_hippocampus_AI <- lm(Hippocampus_AI ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_hippocampus_AI)

mdll_DLPFC_AI <- lm(DLPFC_AI ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_DLPFC_AI)

# Association with Activation ###################################################
mdl_hippocampus_L_Activation <- lm(Left_Hippocampus_Activation ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_hippocampus_L_Activation)

mdl_hippocampus_R_Activation <- lm(Right_Hippocampus_Activation ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_hippocampus_R_Activation)

mdl_DLPFC_L_Activation <- lm(Left_DLPFC_Activation ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_DLPFC_L_Activation)

mdl_DLPFC_R_Activation <- lm(Right_DLPFC_Activation ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_DLPFC_R_Activation)

# Association with Cognitive Factors ###############################################
#memory1 (hippocampus - 0.075)
mdl_memory_hippocampus <- lm(memory ~ Hippocampus_AI, data = data)
summary(mdl_memory_hippocampus)
mdl_memory_DLPFC <- lm(memory ~ DLPFC_AI, data = data)
summary(mdl_memory_DLPFC)

mdl_visiospatial_hippocampus <- lm(visiospatial ~ Hippocampus_AI, data = data)
summary(mdl_visiospatial_hippocampus)
mdl_visiospatial_DLPFC <- lm(visiospatial ~ DLPFC_AI, data = data)
summary(mdl_visiospatial_DLPFC)

mdl_language_hippocampus <- lm(language ~ Hippocampus_AI, data = data)
summary(mdl_language_hippocampus)
mdl_language_DLPFC <- lm(language ~ DLPFC_AI, data = data)
summary(mdl_language_DLPFC)

mdl_executive_hippocampus <- lm(executive ~ Hippocampus_AI, data = data)
summary(mdl_executive_hippocampus)
mdl_executive_DLPFC <- lm(executive ~ DLPFC_AI, data = data)
summary(mdl_executive_DLPFC)

mdl_attention_hippocampus <- lm(attention ~ Hippocampus_AI, data = data)
summary(mdl_attention_hippocampus)
mdl_attention_DLPFC <- lm(attention ~ DLPFC_AI, data = data)
summary(mdl_attention_DLPFC)

mdl_executive_attention_hippocampus <- lm(executive_attention ~ Hippocampus_AI, data = data)
summary(mdl_executive_attention_hippocampus)
mdl_executive_attention_DLPFC <- lm(executive_attention ~ DLPFC_AI, data = data)
summary(mdl_executive_attention_DLPFC)

# Cognitive Factors ###################################################################################################
#Executive1 (0.01), executive/attention (0.00)
mdl_memory <- lm(memory ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory)

mdl_visiospatial <- lm(visiospatial ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial)

mdl_language <- lm(language ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language)

mdl_executive <- lm(executive ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive)

mdl_attention <- lm(attention ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention)

mdl_executive_attention <- lm(executive_attention ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention)

# Cognitive Factors with All Variables ################################################################
#executive1 (0.0299), executive_attention (0.02)
mdl_memory_all <- lm(memory ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_memory_all)

mdl_visiospatial_all <- lm(visiospatial ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_visiospatial_all)

mdl_language_all <- lm(language ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_language_all)

mdl_executive_all <- lm(executive ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_all)

mdl_attention_all <- lm(attention ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_attention_all)

mdl_executive_attention_all <- lm(executive_attention ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE, data = data)
summary(mdl_executive_attention_all)

# Association with FWHM ##########################################################################################
mdl_Hippocampus_FWHM_AI <- lm(Hippocampus_FWHM_AI ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiBStatus_SUVR_GTM_FS_Global+APOE_CODE, data = data)
summary(mdl_Hippocampus_FWHM_AI)
mdl_DLPFC_FWHM_AI <- lm(DLPFC_FWHM_AI ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiBStatus_SUVR_GTM_FS_Global+APOE_CODE, data = data)
summary(mdl_DLPFC_FWHM_AI)

# Demographics Table ###############################################################
if (!require("kableExtra")) install.packages("kableExtra")
library(knitr)
library(kableExtra)
if (!require("scales")) install.packages("scales")
library(scales)

num_subjects <- nrow(data)

#Calculate Data - Some data missing
mean_education <- mean(data$Education, na.rm = TRUE)
stdv_education <- sd(data$Education, na.rm = TRUE)
education_over_12 <- sum(data$Education_cat, na.rm = TRUE)
education_under_12 <- length(data$Education_cat)-sum(data$Education_cat, na.rm = TRUE) - sum(is.na(data$Education_cat))
percent_over_12 <- education_over_12 / (length(data$Education_cat) - sum(is.na(data$Education_cat)))
percent_under_12 <- education_under_12 / (length(data$Education_cat) - sum(is.na(data$Education_cat)))


mean_age <- mean(data$Age_CurrentVisit, na.rm = TRUE)
age_stdv <- sd(data$Age_CurrentVisit, na.rm = TRUE)

male_count <- sum(data$Sex == "Male", na.rm = TRUE)
percent_male <- male_count / (length(data$Sex) - sum(data$Sex == ""))
female_count <- sum(data$Sex == "Female", na.rm = TRUE)
percent_female <- female_count / (length(data$Sex) - sum(data$Sex == ""))

white_count <- sum(data$Race == "White")
percent_white <- white_count / (length(data$Race) - sum(data$Race == ""))
black_count <- sum(data$Race == "Black")
percent_black <- black_count / (length(data$Race) - sum(data$Race == ""))
asian_count <- sum(data$Race == "Asian")
percent_asian <- asian_count / (length(data$Race) - sum(data$Race == ""))

#Make Table
demographic_data <- data.frame(Variable = c("Total_Subjects", "Mean", "Standard Deviation", "Over 12 Years", "Under 12 Years", "Mean", "Standard Deviation", "Male Count", "Female Count", "White Count", "Black Count", "Asian Count"),
                               Value = c(num_subjects, mean_education, stdv_education, education_over_12, education_under_12,mean_age, age_stdv, male_count, female_count, white_count,black_count,asian_count),
                               Percent = c("", "", "", label_percent()(percent_over_12), label_percent()(percent_under_12),"","",label_percent()(percent_male), label_percent()(percent_female),label_percent()(percent_white),label_percent()(percent_black),label_percent()(percent_asian)))

kable(demographic_data, caption = "Demographic data for normal aging", digits = 2) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
                pack_rows("Education", 2,5) %>%
                pack_rows("Age", 6,9) %>%
                pack_rows("Race", 10,12)
  
# Correlation Tables ########################################################################
if (!require("formattable")) install.packages("formattable")
library(formattable)

memory_values <- data.frame("REYDE_Pearson" = REYDE_Pearson_Correlation, "REYIM_Pearson" =REYIM_Pearson_Correlation, "Memory_ICC" = memory_icc)
formattable(memory_values, align = "c", caption = "Memory Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.")

visiospatial_values <- data.frame("BLOCKDES_Pearson" = BLOCKDES_Pearson_Correlation, "REYCO_Pearson" = REYCO_Pearson_Correlation, "Visiospatial_ICC" = visiospatial_icc)
formattable(visiospatial_values, align = "c", caption = "Visiospatial Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.", header = c("Pearson Correlations", ""))

language_values <- data.frame("BOSTON1_Pearson" = BOSTON1_Pearson_Correlation, "FLUEN_Pearson" = FLUEN_Pearson_Correlation, "Language_ICC" = language_icc)
formattable(language_values, align = "c", caption = "Language Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.", header = c("Pearson Correlations", ""))

executive_values <- data.frame("TRAILBS_Pearson" = TRAILBS_Pearson_Correlation, "SPANSB_Pearson" = SPANSB_Pearson_Correlation, "Executive_ICC" = executive_icc)
formattable(executive_values, align = "c", caption = "Executive Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.", header = c("Pearson Correlations", ""))

attention_values <- data.frame("TRAILAS_Pearson" = TRAILAS_Pearson_Correlation, "SPANSF_Pearson" = SPANSF_Pearson_Correlation, "Attention_ICC" = attention_icc)
formattable(attention_values, align = "c", caption = "Attention Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.", header = c("Pearson Correlations", ""))

executive_attention_values <- data.frame("DIGSYMWR_Pearson" = DIGSYMWR_Combo_Pearson_Correlation, "TRAILAS_Pearson" = TRAILAS_Combo_Pearson_Correlation, "SPANSF_Pearson" = SPANSF_Combo_Pearson_Correlation, "TRAILBS_Pearson" = TRAILBS_Combo_Pearson_Correlation, "SPANSB_Pearson" = SPANSB_Combo_Pearson_Correlation, "Executive_Attention_ICC" = executive_attention_icc)
formattable(executive_attention_values, align = "c", caption = "Executive and Attention domain Correlations. Pearson correlations were calcuated between raw scores and domain values.  ICC values were calculated between z-scores of tests within the same domain.", header = c("Pearson Correlations", ""))

# Regression Tables/Plots ######################################################################
if (!require("sjplot")) install.packages("sjplot")
library(sjplot)

#Memory ~ Hippocampus AI
#p-value: 0.07482
tab_model(mdl_memory_hippocampus, dv.labels = "Memory Vs. Hippocampus Asymmetry Index")
plot(data$Hippocampus_AI, data$memory, ylab = "Memory Composite Score", xlab = "Hippocampus Asymmetry Index", main = "Memory Vs. Hippocampus Asymmetry Index")
abline(mdl_memory_hippocampus, col = "red")
plot(mdl_memory_hippocampus, main = "Memory Vs. Hippocampus Asymmtery Index")

#executive ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
#p-value: 0.01229
#executive_attention ~ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
#p-value: 0.007688
tab_model(mdl_executive, mdl_executive_attention,  
  pred.labels = c("Intercept", "Age at Visit", "Sex", "Race",
                  "Education (years)", "FDG Global", 
                  "PiB Global Status", "APOE Status"),show.ci = FALSE, dv.labels = c("Executive", "Executive and Attention"))
plot(mdl_executive, main = "Executive")
plot(mdl_executive_attention, main = "Executive and Attention")

#executive ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
# p-value: 0.02993
#executive_attention ~ DLPFC_AI + Hippocampus_AI+ Age_CurrentVisit+Sex+Race_cat+Education_cat+FDG_SUVR_GTM_FS_Global+PiB_STATUS_CODE+APOE_CODE
#p-value: 0.02018
tab_model(mdl_executive_all, mdl_executive_attention_all,  
          pred.labels = c("Intercept", "DLPFC Asymmetry", "Hippocampus Asymmetry","Age at Visit", "Sex", "Race",
                          "Education (years)", "FDG Global", 
                          "PiB Global Status", "APOE Status"),show.ci = FALSE, dv.labels = c("Executive", "Executive and Attention"))
plot(mdl_executive_all, main = "Executive Vs. All")
plot(mdl_executive_attention_all, main = "Executive and Attention Vs. All")


#################################################################################

save.image(file = "~/Desktop/RStudio Scripts/AI_Results_2020_06_23.rda") #path for spreadsheet - saves all variables




