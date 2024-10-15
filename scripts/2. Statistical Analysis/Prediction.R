################################################################################
### README - README - README - README - README - README - README - README -  ###
### This script contains the code for performing the statistical analysis of ###
### familial risks for my second study, on the familial aggregation of pers- ###
### istence to methotrexate in monotherapy for Swedish patients with early   ###
### RA, here within a sub-cohort taken to reflect the clinical setting. All  ###
### major data extraction and wrangling has been handled elsewhere.          ###
### This script uses logistic regression to obtain the odds of persisting on ###
### treatment at one and three years respectively, given a family history of ###
### persistence at one and three years respectively. This is here done in b- ###
### oth the 'main' cohort as well as the two sensitivity cohorts.            ###
### I chose to use logistic regression here over log-binomial to avoid the   ###
### convergence issues of the estimation algorithm.                          ###
### SCRIPT LAST UPDATED: 19/5 - 2021.                                        ###
### - Changed method back to logistic regression.                            ###
### - Updated to new linkage data and changed file paths accordingly.        ###
### README - README - README - README - README - README - README - README -  ###
################################################################################

### SECTION 0: SETUP
library(dplyr)    #General purpose data wrangling
library(ggplot2)    #For figures, plots and visualization
library(haven)    #Used to read SAS files with `read_sas()`
library(readr)    #General purpose writing files (more efficient than base tools)
library(stringr)    #For parsing results into a more readable table as strings

setwd("H:/Projekt/Familiarity of mono-MTX/")    #Set working directory to abbreviate future file paths

### SECTION 1: INDEX PATIENTS
###### SECTION 1.1: PERSISTENCE AT ONE YEAR

main_pred1yr <- read_sas("Data Extraction/cohort_index_1yr.sas7bdat")    #Use `read_sas()` to load the data

M_main_pred1yr <- glm(persist_1yr ~ any_rel_persist_1yr + kon + age_at_MTX_start + dis_deb_year,
                      data = main_pred1yr, 
                      family = binomial(link = "logit"))    #Fits the LOGISTIC regression model

res_main_pred1yr <- summary(M_main_pred1yr)$coefficients    #Extracts coefficients

###### SECTION 1.2: PERSISTENCE AT THREE YEARS

main_pred3yr <- read_sas("Data Extraction/cohort_index_3yr.sas7bdat")

M_main_pred3yr <- glm(persist_3yr ~ any_rel_persist_3yr + kon + age_at_MTX_start + dis_deb_year,
                      data = main_pred3yr, 
                      family = binomial(link = "logit"))

res_main_pred3yr <- summary(M_main_pred3yr)$coefficients

### SECTION 2: SENSITIVITY
###### SECTION 2.1: SIBLING COHORT
######### SECTION 2.1.1: PERSISTENCE AT ONE YEAR

sib_pred1yr <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_index_1yr_sib.sas7bdat")

M_sib_pred1yr <- glm(persist_1yr ~ any_rel_persist_1yr + kon + age_at_MTX_start + dis_deb_year,
                     data = sib_pred1yr, 
                     family = binomial(link = "logit"))

res_sib_pred1yr <- summary(M_sib_pred1yr)$coefficients

######### SECTION 2.1.2: PERSISTENCE AT THREE YEARS

sib_pred3yr <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_index_3yr_sib.sas7bdat")

M_sib_pred3yr <- glm(persist_3yr ~ any_rel_persist_3yr + kon + age_at_MTX_start + dis_deb_year,
                        data = sib_pred3yr, 
                     family = binomial(link = "logit"))

res_sib_pred3yr <- summary(M_sib_pred3yr)$coefficients

###### SECTION 2.2: ROBUST COHORT
######### SECTION 2.2.1: PERSISTENCE AT ONE YEAR

robust_pred1yr <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_index_1yr_rob.sas7bdat")

M_robust_pred1yr <- glm(persist_1yr ~ any_rel_persist_1yr + kon + age_at_MTX_start + dis_deb_year,
                        data = robust_pred1yr, 
                        family = binomial(link = "logit"))

res_robust_pred1yr <- summary(M_robust_pred1yr)$coefficients

######### SECTION 2.2.2: PERSISTENCE AT THREE YEARS

robust_pred3yr <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_index_3yr_rob.sas7bdat")

M_robust_pred3yr <- glm(persist_3yr ~ any_rel_persist_3yr + kon + age_at_MTX_start + dis_deb_year,
                        data = robust_pred3yr, 
                        family = binomial(link = "logit"))

res_robust_pred3yr <- summary(M_robust_pred3yr)$coefficients

### SECTION 3: PRESENTATION OF RESULTS
###### SECTION 3.1: RESULTS TO A READABLE TABLE

raw_table <- rbind(res_main_pred1yr[2, c(1, 2, 4)],
                   res_sib_pred1yr[2, c(1, 2, 4)],
                   res_robust_pred1yr[2, c(1, 2, 4)],
                   res_main_pred3yr[2, c(1, 2, 4)],
                   res_sib_pred3yr[2, c(1, 2, 4)],
                   res_robust_pred3yr[2, c(1, 2, 4)])    #Bind all the relevant estimates into a 'raw' table

parsed_table <- raw_table %>% 
  as_tibble %>%
  mutate(OR = str_c(round(exp(Estimate), 4)),
         CI_L = exp(Estimate - `Std. Error` * qnorm(1 - 0.05/2)),
         CI_U = exp(Estimate + `Std. Error` * qnorm(1 - 0.05/2)),
         `95% CI` = str_c(round(CI_L, 2), round(CI_U, 2), sep = "-"),
         TYPE = c("Main: 1 year", "Siblings: 1 year", "Robust: 1 year", 
                  "Main: 3 years", "Siblings: 3 years", "Robust: 3 years"),
         P = round(`Pr(>|z|)`, 4)) %>%
  select(TYPE, OR, `95% CI`, P)      #Parse the raw estimates to make them more readable by human eyes

write_tsv(parsed_table, "Scripts/Prediction.txt")    #Write the parsed table to the folder

###### SECTION 3.2: RESULTS TO A READABLE FIGURE
figure <- raw_table %>%
  as_tibble %>%
  mutate(OR = exp(Estimate),
         CI_L = exp(Estimate - `Std. Error` * qnorm(1 - 0.05/2)),
         CI_U = exp(Estimate + `Std. Error` * qnorm(1 - 0.05/2)),
         type = c(1, 1, 1, 3, 3, 3),
         cohort = c("Main", "Siblings", "Robust", "Main", "Siblings", "Robust")) %>%
  ggplot(aes(y = OR, x = factor(type), shape = factor(cohort))) +
  geom_point(position = position_dodge(0.9), size = 3) +
  geom_errorbar(aes(ymin = CI_L, ymax = CI_U),
                width = 0.25, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, alpha = 0.25, size = 1, linetype = "dashed") +
  xlab("") + ylab("Odds ratio of persisting in index patients, \n given family history of persistence") +
  scale_x_discrete(breaks = c(1, 3),
                   labels = c("Persistence at one year", "Persistence at three years")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12),
                     expand = expansion(c(0, 0.05)), limits = c(0, 12)) +
  scale_shape_manual(values = c(15, 16, 17),
                     name = "Cohort:") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "Grey 3", size = 0.25),
        legend.position = "bottom",
        legend.title = element_text(size = 13))    #Create a figure visualizing the results

ggsave("Scripts/Prediction.jpg", plot = figure)    #Use `ggsave()` to store the figure in the folder