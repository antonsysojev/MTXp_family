### POTENTIAL WEAKNESSES:
### The `polychor()` function of the `polycor` package gives identical results
### to the `tetrachoric()` function from the `psych` package.
### Standard errors seem correct based on theory.
### What concerns me is that the formula given in [Tenesa and Haley, 2013] of
###   t = (n_11 n_22 - n_12 n_21) / (n_11 n_22 + n_12 n_21)
### gives a different result than the `polychor()` function. The same goes for
### the cos(180/sqrt(odds) + 1) evaluation. It may be the case that both of these
### are APPROXIMATIONS, but it is the sole weakness I can spot with this code.

################################################################################
### README - README - README - README - README - README - README - README -  ###
### This script contains the code for performing the statistical analysis of ###
### heritability for my second study, on the familial aggregation of persis- ###
### tence to methotrexate in monotherapy for Swedish patients with early RA. ###
### All major data extraction and wrangling has been handled elsewhere.      ###
### This script uses polychoric correlation (or here equivalent: tetrachori- ###
### c correlation) to obtain estimates of the heritability of persistence to ###
### methotrexate in monotherapy at one and three years respectively. Herita- ###
### bility is estimated as twice the polychoric correlation as the cohort c- ###
### onsists of exclusively first-degree relatives (see Tenesa, 2013 for more ###
### details). This is here done in both the 'main' cohort as well as the two ###
### sensitivity cohorts.                                                     ###
### Note that I do not here take into account individuals with multiple rel- ###
### atives (see Che, 2021 for a discussion on the limitations arising due to ###
### this).                                                                   ###
### Since standard errors are only available for the estimated polychoric c- ###
### orrelation, I use the Delta method to derive the heritability standard   ###
### errors for use in computation of confidence intervals. See the           ###
### `Heritability_note.docx` for details on this derivation.                 ###
### SCRIPT LAST UPDATED: 7/12 - 2021.                                        ###
### - Fixed a bug that used the siblings (p3yr) standard error for the robu- ###
### st (p3yr). Luckily, these are near-identical so no real difference.      ###
### README - README - README - README - README - README - README - README -  ###
################################################################################

### SECTION 0: SETUP
library(dplyr)    #General purpose data wrangling
library(ggplot2)    #For figures, plots and visualization
library(haven)    #Used to read SAS files with `read_sas()`
library(polycor)    #For built-in tetrachoric correlations
library(readr)    #General purpose writing files (more efficient than base tools)
library(stringr)    #For parsing results into a more readable table as strings

setwd("H:/Projekt/Familiarity of mono-MTX/")    #Set working directory to abbreviate future file paths

### SECTION 1: MAIN COHORT

main <- read_sas("Data Extraction/cohort.sas7bdat")    #Use `read_sas()` to load the data

###### SECTION 1.1: PERSISTENCE AT ONE YEAR

res_main_pers1yr <- polychor(main$persist_1yr, main$rel_persist_1yr, std.err = T)    #Estimates the tetrachoric correlation. The input `std.error = T` retrieves standard errors and more

###### SECTION 1.2: PERSISTENCE AT THREE YEARS

res_main_pers3yr <- polychor(main$persist_3yr, main$rel_persist_3yr, std.err = T)

### SECTION 2: SENSITIVITY
###### SECTION 2.1: SIBLING COHORT

sib <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_sib.sas7bdat")

######### SECTION 2.1.1: PERSISTENCE AT ONE YEAR

res_sib_pers1yr <- polychor(sib$persist_1yr, sib$rel_persist_1yr, std.err = T)

######### SECTION 2.1.2: PERSISTENCE AT THREE YEARS

res_sib_pers3yr <- polychor(sib$persist_3yr, sib$rel_persist_3yr, std.err = T)

###### SECTION 2.2: ROBUST COHORT

robust <- read_sas("Data Extraction/Sensitivity cohorts/sensitivity_rob.sas7bdat")

###### SECTION 2.2.1: PERSISTENCE AT ONE YEAR

res_robust_pers1yr <- polychor(robust$persist_1yr, robust$rel_persist_1yr, std.err = T)

###### SECTION 2.2.2: PERSISTENCE AT THREE YEARS

res_robust_pers3yr <- polychor(robust$persist_3yr, robust$rel_persist_3yr, std.err = T)

### SECTION 3: PRESENTATION OF RESULTS
###### SECTION 3.1: RESULTS TO A READABLE TABLE

raw_table <- rbind(c(pcorr = res_main_pers1yr$rho, se = sqrt(res_main_pers1yr$var)[1, 1]),
                   c(pcorr = res_sib_pers1yr$rho, se = sqrt(res_sib_pers1yr$var)[1, 1]),
                   c(pcorr = res_robust_pers1yr$rho, se = sqrt(res_robust_pers1yr$var)[1, 1]),
                   c(pcorr = res_main_pers3yr$rho, se = sqrt(res_main_pers3yr$var)[1, 1]),
                   c(pcorr = res_sib_pers3yr$rho, se = sqrt(res_sib_pers3yr$var)[1, 1]),
                   c(pcorr = res_robust_pers3yr$rho, se = sqrt(res_robust_pers3yr$var)[1, 1]))    #Bind all the relevant estimates into a 'raw' table

parsed_table <- raw_table %>%
  as_tibble() %>%
  mutate(heritability = round(pcorr / 0.5, 4),
         CI_L = pcorr / 0.5 - se / 0.5 * qnorm(1 - 0.05/2),
         CI_U = pcorr / 0.5 + se / 0.5 * qnorm(1 - 0.05/2),
         `95% CI` = str_c(round(CI_L, 2), round(CI_U, 2), sep = "-"),
         out_of_bounds_l = ifelse(CI_L < 0, T, F),
         out_of_bounds_u = ifelse(CI_U > 1, T, F),
         `Capped 95% CI` = str_c(ifelse(out_of_bounds_l, "0'", round(CI_L, 2)),
                                   ifelse(out_of_bounds_u, "1'", round(CI_U, 2)), sep = "-"),
         TYPE = c("Main: 1 year", "Siblings: 1 year", "Robust: 1 year", 
                  "Main: 3 years", "Siblings: 3 years", "Robust: 3 years")) %>%
  select(TYPE, heritability, `95% CI`, `Capped 95% CI`)     #Parse the raw estimates to make them more readable by human eyes

write_tsv(parsed_table, "Scripts/Heritability.txt")    #Write the parsed table to the folder

###### SECTION 3.2: RESULTS TO A READABLE FIGURE

figure <- raw_table %>%
  as_tibble %>%
  mutate(h2 = pcorr / 0.5, h2_se = se / 0.5, 
         type = c(1, 1, 1, 3, 3, 3), 
         cohort = c("Main", "Siblings", "Robust", "Main", "Siblings", "Robust"),
         CI_L = h2 - h2_se * qnorm(1 - 0.05/2),
         CI_U = h2 + h2_se * qnorm(1 - 0.05/2))  %>%
  ggplot(aes(y = h2, x = factor(type), fill = factor(cohort))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "Grey 10") +
  geom_errorbar(aes(ymin = ifelse(CI_L < 0, 0, CI_L), ymax = CI_U),
                width = 0.25, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5, size = 1) +
  xlab("") + ylab("Heritability") +
  scale_x_discrete(breaks = c(1, 3),
                   labels = c("Persistence at one year", "Persistence at three years")) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     expand = expansion(c(0, 0.05)), limits = c(0, 100)) +
  coord_cartesian(ylim = c(0, 1.5)) +
  scale_fill_manual(values = c("Grey 50", "Grey 75", "Snow"),
                    name = "Cohort:") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "Grey 3", size = 0.25),
        legend.position = "bottom",
        legend.title = element_text(size = 13))    #Create a figure visualizing the results

ggsave("Scripts/Heritability.jpg", plot = figure)    #Use `ggsave()` to store the figure in the folder
