### POTENTIAL WEAKNESSES:
### As long as `Main.R` is correct, there's no reason to believe that this
### would not be, as they share code and procedure, more or less, completely.

################################################################################
### README - README - README - README - README - README - README - README -  ###
### This script contains the code for performing the statistical analysis of ###
### the exploratory analysis of familial risks for my second study, on the   ###
### familial aggregation of persistence to methotrexate in monotherapy for   ###
### Swedish patients with early RA. All major data extraction and wrangling  ###
### has been handled elsewhere. This script uses logistic regression to obt- ###
### ain the risk of EULAR response at three and six months respectively, gi- ###
### ven a family history of EULAR response at three and six months respecti- ###
### vely. This is here done in both the 'main' cohort as well as the two se- ###
### nsitivity cohorts.                                                       ###
### I chose to use logistic regression here over log-binomial to avoid the   ###
### convergence issues of the estimation algorithm.                          ###
### Robust standard errors are computed using the `sandwich` package (see Z- ###
### eileis, 2006 for details) where I use the default HC estimator (here de- ###
### noted by 'HC3', see Zeileis, 2004, p.4 for details). Here, only the coh- ###
### orts which contain individuals with multiple first-degree relatives use  ###
### the robust standard errors.                                              ###
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
library(lmtest)    #For the function `coeftest()` which is used with `sandwich::vcovHC()` to obtain robust standard errors
library(sandwich)    #For the function `vcovHC()` which is used with `coeftest()` to obtain robust standard errors
library(stringr)    #For parsing results into a more readable table as strings

setwd("H:/Projekt/Familiarity of mono-MTX/")    #Set working directory to abbreviate future file paths

### Returns TRUE if there are duplicated `pid` after removing the individuals with missing `var_name`
### Will be used to decide whether to use robust standard errors or not.
any_duplicated_inds <- function(data, var_name){
  max_n_dupl <- data %>% 
    filter(!is.na(!!sym(var_name))) %>%
    group_by(pid) %>% summarise(n_dupl = n()) %>%
    summarise(max_n_dupl = max(n_dupl)) %>%
    unlist
  ifelse(max_n_dupl > 1, T, F)
}

### SECTION 1: MAIN COHORT

main <- read_sas("Data Extraction/cohort.sas7bdat")    #Use `read_sas()` to load the data

###### SECTION 1.1: EULAR RESPONSE AT THREE MONTHS

M_main_EULAR3 <- glm(three_months_responder ~ rel_three_months_responder + kon + age_at_MTX_start + dis_deb_year,
                     data = main, 
                     family = binomial(link = "logit"))    #Fits the LOGISTIC regression model

res_main_EULAR3 <- if(any_duplicated_inds(main, "rel_three_months_responder")){    #If individuals with multiple first-degree relative in data
  coeftest(M_main_EULAR3, vcov = vcovHC(M_main_EULAR3))    #Extract coefficients WITH robust standard errors
}else{
  coeftest(M_main_EULAR3)    #Else extract coefficients WITHOUT robust standard errors
}

##### SECTION 1.2: EULAR RESPONSE AT SIX MONTHS

M_main_EULAR6 <- glm(six_months_responder ~ rel_six_months_responder + kon + age_at_MTX_start + dis_deb_year,
                     data = main, 
                     family = binomial(link = "logit"))

res_main_EULAR6 <- if(any_duplicated_inds(main, "rel_six_months_responder")){
  coeftest(M_main_EULAR6, vcov = vcovHC(M_main_EULAR6))
}else{
  coeftest(M_main_EULAR6)
}

### SECTION 2: SENSITIVITY
###### SECTION 2.1: SIBLING COHORT

sib <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_sib.sas7bdat")

######### SECTION 2.1.1: EULAR RESPONSE AT THREE MONTHS

M_sib_EULAR3 <- glm(three_months_responder ~ rel_three_months_responder + kon + age_at_MTX_start + dis_deb_year,
                    data = sib, 
                    family = binomial(link = "logit"))

res_sib_EULAR3 <- if(any_duplicated_inds(sib, "rel_three_months_responder")){
  coeftest(M_sib_EULAR3, vcov = vcovHC(M_sib_EULAR3))
}else{
  coeftest(M_sib_EULAR3)
}

######### SECTION 2.1.1: EULAR RESPONSE AT SIX MONTHS

M_sib_EULAR6 <- glm(six_months_responder ~ rel_six_months_responder + kon + age_at_MTX_start + dis_deb_year,
                    data = sib, 
                    family = binomial(link = "logit"))

res_sib_EULAR6 <- if(any_duplicated_inds(sib, "rel_six_months_responder")){
  coeftest(M_sib_EULAR6, vcov = vcovHC(M_sib_EULAR6))
}else{
  coeftest(M_sib_EULAR6)
}

###### SECTION 2.2: ROBUST COHORT

robust <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_rob.sas7bdat")

######### SECTION 2.2.1: EULAR RESPONSE AT THREE MONTHS

M_robust_EULAR3 <- glm(three_months_responder ~ rel_three_months_responder + kon + age_at_MTX_start + dis_deb_year,
                       data = robust, 
                       family = binomial(link = "logit"))

res_robust_EULAR3 <- if(any_duplicated_inds(robust, "rel_three_months_responder")){
  coeftest(M_robust_EULAR3, vcov = vcovHC(M_robust_EULAR3))
}else{
  coeftest(M_robust_EULAR3)
}

############ SECTION 2.2.1: EULAR RESPONSE AT SIX MONTHS

M_robust_EULAR6 <- glm(six_months_responder ~ rel_six_months_responder + kon + age_at_MTX_start + dis_deb_year,
                       data = robust, 
                       family = binomial(link = "logit"))

res_robust_EULAR6 <- if(any_duplicated_inds(robust, "rel_six_months_responder")){
  coeftest(M_robust_EULAR6, vcov = vcovHC(M_robust_EULAR6))
}else{
  coeftest(M_robust_EULAR6)
}

### SECTION 3: PRESENTATION OF RESULTS
###### SECTION 3.1: RESULTS TO A READABLE TABLE

raw_table <- rbind(res_main_EULAR3[2, c(1, 2, 4)],
                   res_sib_EULAR3[2, c(1, 2, 4)],
                   res_robust_EULAR3[2, c(1, 2, 4)],
                   res_main_EULAR6[2, c(1, 2, 4)],
                   res_sib_EULAR6[2, c(1, 2, 4)],
                   res_robust_EULAR6[2, c(1, 2, 4)])    #Bind all the relevant estimates into a 'raw' table

parsed_table <- raw_table %>% 
  as_tibble %>%
  mutate(OR = str_c(round(exp(Estimate), 4)),
         CI_L = exp(Estimate - `Std. Error` * qnorm(1 - 0.05/2)),
         CI_U = exp(Estimate + `Std. Error` * qnorm(1 - 0.05/2)),
         `95% CI` = str_c(round(CI_L, 2), round(CI_U, 2), sep = "-"),
         TYPE = c("Main: EULAR 3 months", "Siblings: EULAR 3 months", "Robust: EULAR 3 months",
                  "Main: EULAR 6 months", "Siblings: EULAR 6 months", "Robust: EULAR 6 months"),
         P = round(`Pr(>|z|)`, 4)) %>%
  select(TYPE, OR, `95% CI`, P)      #Parse the raw estimates to make them more readable by human eyes

write_tsv(parsed_table, "Scripts/EULAR_response.txt")    #Write the parsed table to the folder

###### SECTION 3.2: RESULTS TO A READABLE FIGURE
figure <- raw_table %>%
  as_tibble %>%
  mutate(OR = exp(Estimate),
         CI_L = exp(Estimate - `Std. Error` * qnorm(1 - 0.05/2)),
         CI_U = exp(Estimate + `Std. Error` * qnorm(1 - 0.05/2)),
         type = c(3, 3, 3, 6, 6, 6),
         cohort = rep(c("Main", "Siblings", "Robust"), 2)) %>%
  ggplot(aes(y = OR, x = factor(type), shape = factor(cohort))) +
  geom_point(position = position_dodge(0.9), size = 3) +
  geom_errorbar(aes(ymin = CI_L, ymax = CI_U),
                width = 0.25, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, alpha = 0.25, size = 1, linetype = "dashed") +
  xlab("") + ylab("Odds ratio of persisting, \n given family history of EULAR response") +
  scale_x_discrete(breaks = c(3, 6),
                   labels = c("EULAR response: three months", 
                              "EULAR response: six months")) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7),
                     expand = expansion(c(0, 0.05)), limits = c(0, 7)) +
  scale_shape_manual(values = c(15, 16, 17),
                    name = "Cohort:") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "Grey 3", size = 0.25),
        legend.position = "bottom",
        legend.title = element_text(size = 13))    #Create a figure visualizing the results

ggsave("Scripts/EULAR_response.jpg", plot = figure)    #Use `ggsave()` to store the figure in the folder
