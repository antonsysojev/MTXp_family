### POTENTIAL WEAKNESSES:
### There doesn't seem to be much that is or could be wrong.
### We have to trust the `logbin()` function from the package with the same name
### and it makes sense to do so since basic comparison with `glm()` provides
### identical results. The benefit of using `logbin()` is the additional tools
### for mitigation of convergence issues, which is an enormous issue for `glm()`.
###   I think one can also use the `survival` package for robust standard errors,
### but I didn't try this here and I like having the Zeileis package since it
### comes with published citation as well as discussion of theory.
###   I was initially skeptical about the confidence interval being simply the
### exponential of the confidence interval for the log(RR), but it seems to hold
### due to the invariance property of the MLE? So, we don't need the Delta-method?
### But my computations with the Delta-method does not really work out, but this
### may be due to me being stupid.

################################################################################
### README - README - README - README - README - README - README - README -  ###
### This script contains the code for performing the statistical analysis of ###
### familial risks for my second study, on the familial aggregation of pers- ###
### istence to methotrexate in monotherapy for Swedish patients with early   ###
### RA. All major data extraction and wrangling has been handled elsewhere.  ###
### This script uses log-binomial regression to obtain the risk of persisti- ###
### ng on treatment at one and three years respectively, given a family his- ###
### tory of persistence at one and three years respectively. This is here d- ###
### one in both the 'main' cohort as well as the two sensitivity cohorts.    ###
### Robust standard errors are computed using the `sandwich` package (see Z- ###
### eileis, 2006 for details) where I use the default HC estimator (here de- ###
### noted by 'HC3', see Zeileis, 2004, p.4 for details).                     ###
### SCRIPT LAST UPDATED: 19/5 - 2021.                                        ###
### - Updated to log-binomial regression (again).                            ###
### - Changed outputs from odds to risk.                                     ###
### - Updated the figure to be better representative.                        ###
### - Updated to new linkage data and changed file paths accordingly.        ###
### README - README - README - README - README - README - README - README -  ###
################################################################################

### SECTION 0: SETUP
library(dplyr)    #General purpose data wrangling
library(ggplot2)    #For figures, plots and visualization
library(haven)    #Used to read SAS files with `read_sas()`
library(readr)    #General purpose writing files (more efficient than base tools)
library(lmtest)    #For the function `coeftest()` which is used with `sandwich::vcovHC()` to obtain robust standard errors
library(logbin)    #For log-binomial regression with alternative estimation algorithms
library(sandwich)    #For the function `vcovHC()` which is used with `coeftest()` to obtain robust standard errors
library(stringr)    #For parsing results into a more readable table as strings

setwd("H:/Projekt/Familiarity of mono-MTX/")

### SECTION 1: MAIN COHORT

main <- read_sas("Data Extraction/cohort.sas7bdat")

main <- main %>% mutate(age_cat = cut(age_at_MTX_start,
                                      breaks = c(0, 49, 64, Inf),
                                      labels = c(0, 1, 2)))

###### SECTION 1.1: PERSISTENCE AT ONE YEAR

M_main_pers1yr <- logbin(persist_1yr ~ rel_persist_1yr + kon + age_cat + dis_deb_year,
                         data = main, method = "cem")

res_main_pers1yr <- coeftest(M_main_pers1yr, vcov = vcovHC(M_main_pers1yr))

###### SECTION 1.2: PERSISTENCE AT THREE YEARS

M_main_pers3yr <- logbin(persist_3yr ~ rel_persist_3yr + kon + age_cat + dis_deb_year,
                         data = main, method = "cem")

res_main_pers3yr <- coeftest(M_main_pers3yr, vcov = vcovHC(M_main_pers3yr))

### SECTION 2: SENSITIVITY
###### SECTION 2.1: SIBLING COHORT

sib <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_sib.sas7bdat")

sib <- sib %>% mutate(age_cat = cut(age_at_MTX_start,
                                    breaks = c(0, 49, 64, Inf),
                                    labels = c(0, 1, 2)))

######### SECTION 2.1.1: PERSISTENCE AT ONE YEAR

M_sib_pers1yr <- logbin(persist_1yr ~ rel_persist_1yr + kon + age_cat + dis_deb_year,
                        data = sib, method = "cem")

res_sib_pers1yr <- coeftest(M_sib_pers1yr, vcov = vcovHC(M_sib_pers1yr))

######### SECTION 2.1.2: PERSISTENCE AT THREE YEARS

M_sib_pers3yr <- logbin(persist_3yr ~ rel_persist_3yr + kon + age_cat + dis_deb_year,
                        data = sib, method = "cem")

res_sib_pers3yr <- coeftest(M_sib_pers3yr, vcov = vcovHC(M_sib_pers3yr))

###### SECTION 2.2: ROBUST COHORT

robust <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_rob.sas7bdat")

robust <- robust %>% mutate(age_cat = cut(age_at_MTX_start,
                                          breaks = c(0, 49, 64, Inf),
                                          labels = c(0, 1, 2)))

######### SECTION 2.2.1: PERSISTENCE AT ONE YEAR

M_robust_pers1yr <- logbin(persist_1yr ~ rel_persist_1yr + kon + age_cat + dis_deb_year,
                           data = robust, method = "cem")

res_robust_pers1yr <- coeftest(M_robust_pers1yr, vcov = vcovHC(M_robust_pers1yr))

######### SECTION 2.2.2: PERSISTENCE AT THREE YEARS

M_robust_pers3yr <- logbin(persist_3yr ~ rel_persist_3yr + kon + age_cat + dis_deb_year,
                           data = robust, method = "cem")

res_robust_pers3yr <- coeftest(M_robust_pers3yr, vcov = vcovHC(M_robust_pers3yr))

### SECTION 3: PRESENTATION OF RESULTS
###### SECTION 3.1: RESULTS TO A READABLE TABLE

raw_table <- rbind(res_main_pers1yr[2, c(1, 2, 4)],
                   res_sib_pers1yr[2, c(1, 2, 4)],
                   res_robust_pers1yr[2, c(1, 2, 4)],
                   res_main_pers3yr[2, c(1, 2, 4)],
                   res_sib_pers3yr[2, c(1, 2, 4)],
                   res_robust_pers3yr[2, c(1, 2, 4)])

parsed_table <- raw_table %>% 
  as_tibble %>%
  mutate(RR = str_c(round(exp(Estimate), 4)),
         CI_L = exp(Estimate - `Std. Error` * qnorm(1 - 0.05/2)),
         CI_U = exp(Estimate + `Std. Error` * qnorm(1 - 0.05/2)),
         `95% CI` = str_c(round(CI_L, 2), round(CI_U, 2), sep = "-"),
         TYPE = c("Main: 1 year", "Siblings: 1 year", "Robust: 1 year", 
                  "Main: 3 years", "Siblings: 3 years", "Robust: 3 years"),
         P = round(`Pr(>|z|)`, 4)) %>%
  select(TYPE, RR, `95% CI`, P) 

write_tsv(parsed_table, "Scripts/Main.txt")

###### SECTION 3.2: RESULTS TO A READABLE FIGURE

figure <- raw_table %>%
  as_tibble %>%
  mutate(RR = exp(Estimate),
         CI_L = exp(Estimate - `Std. Error` * qnorm(1 - 0.05/2)),
         CI_U = exp(Estimate + `Std. Error` * qnorm(1 - 0.05/2)),
         type = c(1, 1, 1, 3, 3, 3),
         cohort = c("Main", "Siblings", "Robust", "Main", "Siblings", "Robust")) %>%
  ggplot(aes(y = RR, x = factor(type), shape = factor(cohort))) +
  geom_point(stat = "identity", position = position_dodge(0.9), size = 3) +
  geom_errorbar(aes(ymin = CI_L, ymax = CI_U),
                width = 0.25, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, alpha = 0.5, linetype = "dashed", size = 1) +
  xlab("") + ylab("Risk of persisting, \n given family history of persistence") +
  scale_x_discrete(breaks = c(1, 3),
                   labels = c("Persistence at one year", "Persistence at three years")) +
  scale_y_continuous(breaks = c(1, 2, 3),
                     expand = expansion(c(0, 0.05)), limits = c(0.5, 3)) +
  scale_shape_manual(values = c(15, 16, 17), name = "Cohort:") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "Grey 3", size = 0.25),
        legend.position = "bottom",
        legend.title = element_text(size = 13))

ggsave("Scripts/Main.jpg", plot = figure)