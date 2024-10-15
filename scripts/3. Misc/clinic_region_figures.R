library(gridExtra)
library(ggplot2)
library(dplyr)
library(haven)

df <- read_sas("H:/Projekt/Familiarity of mono-MTX/Data Extraction/Misc/clin_reg.sas7bdat")

### WE NEED THREE PLOTS:
### PLOT 1: NON-STRATIFIED, PRESENT THE DISTRIBUTION OF CLINICS.
### PLOT 2: STRATIFIED ON PERSISTENCE AT ONE-YEAR, PRESENT THE FREQUENCY DISTRIBUTION OF CLINICS.
### PLOT 3: STRATIFIED ON PERSISTENCE AT THREE-YEARS, PRESENT THE FREQUENCY DISTRIBUTION OF CLINICS.

df %>% 
  filter(persist_1yr == 1) %>%
  group_by(tillhor) %>%
  summarise(N = n()) %>%
  mutate(FREQ = N / nrow(df %>% filter(persist_1yr == 1)), persist_1yr = 1) %>%
  bind_rows(df %>% 
              filter(persist_1yr == 0) %>%
              group_by(tillhor) %>%
              summarise(N = n()) %>%
              mutate(FREQ = N / nrow(df %>% filter(persist_1yr == 0)), persist_1yr = 0)) %>%
  ggplot(aes(x = factor(tillhor), y = FREQ, fill = factor(persist_1yr))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9)) + 
  coord_flip() + xlab("") + ylab("Proportion") +
  ggtitle("Proportion patients per clinic, \nstratified by persistence at one year") +
  scale_fill_manual(labels = c("Non-persistent", "Persistent"), 
                    values = c("BLUE", "RED"), 
                    name = "Peristence at \none year")
  
df %>% 
  filter(persist_3yr == 1) %>%
  group_by(tillhor) %>%
  summarise(N = n()) %>%
  mutate(FREQ = N / nrow(df %>% filter(persist_3yr == 1)), persist_3yr = 1) %>%
  bind_rows(df %>% 
              filter(persist_3yr == 0) %>%
              group_by(tillhor) %>%
              summarise(N = n()) %>%
              mutate(FREQ = N / nrow(df %>% filter(persist_3yr == 0)), persist_3yr = 0)) %>%
  ggplot(aes(x = factor(tillhor), y = FREQ, fill = factor(persist_3yr))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  coord_flip() + xlab("") + ylab("Proportion") +
  ggtitle("Proportion patients per clinic, \nstratified by persistence at three years") +
  scale_fill_manual(labels = c("Non-persistent", "Persistent"),
                    values = c("BLUE", "RED"),
                    name = "Persistence at \nthree years")

### MODIFIED BAR PLOTS

full_p1yr_scaledbar <- df %>%
  filter(persist_1yr == 1) %>%
  group_by(tillhor) %>%
  summarise(N_p1yr = n()) %>%
  mutate(p1yr = 1) %>%
  bind_rows(df %>%
              filter(persist_1yr == 0) %>%
              group_by(tillhor) %>%
              summarise(N_p1yr = n()) %>%
              mutate(p1yr = 0)) %>%
  left_join(df %>%
              group_by(tillhor) %>%
              summarise(N = n()), by = "tillhor") %>%
  mutate(N_freq = N_p1yr / N) %>%
  ggplot(aes(x = factor(tillhor), y = N_freq, fill = factor(p1yr))) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.66, alpha = 0.5, linetype = "dashed", col = "Grey 3") +
  coord_flip() + xlab("") + ylab("Proportion, scaled to clinic size") +
  ggtitle("Proportion persistent patients per clinic, scaled to one \nDashed line represents general proportion persistent") +
  scale_fill_discrete(labels = c("Non-persistent", "Persistent"), 
                      name = "Peristence \nat one year")

sub_p1yr_scaledbar <- df %>%
  filter(persist_1yr == 1) %>%
  group_by(tillhor) %>%
  summarise(N_p1yr = n()) %>%
  mutate(p1yr = 1) %>%
  bind_rows(df %>%
              filter(persist_1yr == 0) %>%
              group_by(tillhor) %>%
              summarise(N_p1yr = n()) %>%
              mutate(p1yr = 0)) %>%
  left_join(df %>%
              group_by(tillhor) %>%
              summarise(N = n()), by = "tillhor") %>%
  mutate(N_freq = N_p1yr / N) %>%
  filter(N >= 5) %>%
  ggplot(aes(x = factor(tillhor), y = N_freq, fill = factor(p1yr))) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.66, alpha = 0.5, linetype = "dashed", col = "Grey 3") +
  coord_flip() + xlab("") + ylab("Proportion, scaled to clinic size") +
  ggtitle("Proportion persistent patients per clinic, scaled to one \nDashed line represents general proportion persistent \nExcluding clinics with <5 patients") +
  scale_fill_discrete(labels = c("Non-persistent", "Persistent"), 
                      name = "Peristence \nat one year")

grid.arrange(full_p1yr_scaledbar, sub_p1yr_scaledbar, ncol = 1, nrow = 2)

full_p3yr_scaledbar <- df %>%
  filter(persist_3yr == 1) %>%
  group_by(tillhor) %>%
  summarise(N_p3yr = n()) %>%
  mutate(p3yr = 1) %>%
  bind_rows(df %>%
              filter(persist_3yr == 0) %>%
              group_by(tillhor) %>%
              summarise(N_p3yr = n()) %>%
              mutate(p3yr = 0)) %>%
  left_join(df %>%
              group_by(tillhor) %>%
              summarise(N = n()), by = "tillhor") %>%
  mutate(N_freq = N_p3yr / N) %>%
  ggplot(aes(x = factor(tillhor), y = N_freq, fill = factor(p3yr))) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = "dashed", col = "Grey 3") +
  coord_flip() + xlab("") + ylab("Proportion, scaled to clinic size") +
  ggtitle("Proportion persistent patients per clinic, scaled to one \nDashed line represents general proportion persistent") +
  scale_fill_discrete(labels = c("Non-persistent", "Persistent"), 
                      name = "Peristence \nat three years")

sub_p3yr_scaledbar <- df %>%
  filter(persist_3yr == 1) %>%
  group_by(tillhor) %>%
  summarise(N_p3yr = n()) %>%
  mutate(p3yr = 1) %>%
  bind_rows(df %>%
              filter(persist_3yr == 0) %>%
              group_by(tillhor) %>%
              summarise(N_p3yr = n()) %>%
              mutate(p3yr = 0)) %>%
  left_join(df %>%
              group_by(tillhor) %>%
              summarise(N = n()), by = "tillhor") %>%
  mutate(N_freq = N_p3yr / N) %>%
  filter(N >= 5) %>%
  ggplot(aes(x = factor(tillhor), y = N_freq, fill = factor(p3yr))) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.5, alpha = 0.5, linetype = "dashed", col = "Grey 3") +
  coord_flip() + xlab("") + ylab("Proportion, scaled to clinic size") +
  ggtitle("Proportion persistent patients per clinic, scaled to one \nDashed line represents general proportion persistent \nExcluding clinics with <5 patients") +
  scale_fill_discrete(labels = c("Non-persistent", "Persistent"), 
                      name = "Peristence \nat three years")

grid.arrange(full_p3yr_scaledbar, sub_p3yr_scaledbar, ncol = 1, nrow = 2)