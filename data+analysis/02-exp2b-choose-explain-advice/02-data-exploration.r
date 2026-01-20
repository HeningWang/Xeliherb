library(tidyverse)
library(tidyboot)
library(aida)
library(brms)
library(faintr)
library(dplyr)

##################################################

# these options help Stan run faster
options(mc.cores = parallel::detectCores())

# use the aida-theme for plotting
theme_set(theme_aida())

# global color scheme / non-optimized
project_colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
} 

##################################################
## analysis
##################################################

d <- read_csv('01-data-annotated-pilot-04.csv') %>% 
  mutate(responseType = case_when(measure == 'choice' ~ response,
                                  TRUE ~ responseType))

d %>% filter(measure == "choice") %>% 
  count(response) %>% 
  mutate(proportion = n/sum(n))

sumStatsChoice <- d %>% filter(measure == 'choice') %>% 
  mutate(decision = ifelse(response == "only xeliherb", 0, 1)) %>%
  tidyboot_mean(decision)
show(sumStatsChoice)  

#sumStatsChoice %>% ggplot(aes(x))

d2 <- d %>% filter(responseType != "") %>% 
  select(submission_id, measure, responseType) %>% 
  pivot_wider(id_cols = submission_id, names_from = measure, values_from = responseType) %>% 
  count(choice, reproduction) %>% 
  arrange(choice,reproduction) %>% 
  ungroup()

sumStats <- d2 %>% 
  group_by(choice) %>% 
  mutate(proportion = n / sum(n)) %>% 
  ungroup()
  
sumStats %>% ggplot(aes(x = reproduction, y = proportion, fill = reproduction)) + 
  geom_col() +
  facet_wrap(~choice, ncol = 1) + coord_flip() + 
  # omit legend
  theme(legend.position = "none") -> p

p

ggsave("results_exp2b.png", p, width = 8, height = 6)


