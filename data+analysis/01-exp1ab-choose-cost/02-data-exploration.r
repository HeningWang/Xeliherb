library(tidyverse)
library(tidyboot)
library(aida)
library(brms)
library(faintr)

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
## auxiliary functions 
##################################################

load_wrangle_data <- function(filename_raw, filename_wrangled, version_name) {
  d <- read_csv(filename_raw) 

  # decision trials  
  d_decision <- filter(d, trialNR == 0) %>% 
    rename(responseTimeDecision = responseTime,
           decision = response) %>% 
    select(-justification) %>% 
    mutate(decision = case_when(decision == "both xeliherb and ralocrop" ~ "both", TRUE ~ "single"))
  # justifcation trials
  d_justification <- filter(d, trialNR == 1) %>% 
    rename(responseTimeJustifcation = responseTime) %>% 
    select(submission_id, justification, responseTimeJustifcation)
  
  d <- full_join(d_decision, d_justification, by = 'submission_id')
  
  d <- d %>% mutate(version = version_name)
  
  write_csv(d, filename_wrangled)
  
  return(d) 
}

make_barplot <- function(d, title) {
  sumStats <- d %>% 
    mutate(decision = ifelse(decision == "both", 1, 0)) %>% 
    group_by(condition) %>% 
    tidyboot_mean(column = decision) %>% 
    mutate(condition = factor(condition, levels = c("deniedCausation", "commonCause", "association", "intervention")))
  
  sumStats %>% 
    ggplot(aes(x = condition, fill = condition, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, width =0.2) +
    theme(legend.position = 'none') +
    xlab('condition') +
    ylab('proportion of "both" choice') +
    ylim(0,1) +
    ggtitle(title)
}

run_regression_single <- function(d) {
  # run regression model
  fit <- brm(decision ~ condition, 
             family = bernoulli(link = "logit"), 
             data = d %>% mutate(decision = factor(decision, levels = c("single", "both"))))
  fit
}

run_regression_joint <- function(d) {
  # run regression model
  fit <- brm(decision ~ condition * version, 
             family = bernoulli(link = "logit"), 
             data = d %>% mutate(decision = factor(decision, levels = c("single", "both"))))
  fit
}

evaluate_fit <- function(fit) {
  # collect pairwise comparisons
  
  deniedCausationVScommonCause <- faintr::compare_groups(
    fit, 
    lower = condition == "deniedCausation",
    higher  = condition == "commonCause"
  )
  
  commonCauseVSassociation <- faintr::compare_groups(
    fit, 
    lower = condition == "commonCause",
    higher  = condition == "association"
  )
  
  associationVSintervention <- faintr::compare_groups(
    fit, 
    lower = condition == "association",
    higher  = condition == "intervention"
  )
  
  deniedCaustaionVSassociation <- faintr::compare_groups(
    fit, 
    lower = condition == "deniedCausation",
    higher  = condition == "association"
  )
  
  deniedCausationVSintervention <- faintr::compare_groups(
    fit, 
    lower = condition == "deniedCausation",
    higher  = condition == "intervention"
  )
  
  tribble(
    ~comparison, ~posterior, ~"95%HDI (low)", ~"95%HDI (high)",
    "denied causation < common cause" , deniedCausationVScommonCause$post_prob, deniedCausationVScommonCause$l_ci, deniedCausationVScommonCause$u_ci,  
    "common cause < association" , commonCauseVSassociation$post_prob, commonCauseVSassociation$l_ci, commonCauseVSassociation$u_ci,
    "association < intervention" , associationVSintervention$post_prob, associationVSintervention$l_ci, associationVSintervention$u_ci,
    "denied causation < association" , deniedCaustaionVSassociation$post_prob, deniedCaustaionVSassociation$l_ci, deniedCaustaionVSassociation$u_ci,
    "denied causation < intervention" , deniedCausationVSintervention$post_prob, deniedCausationVSintervention$l_ci, deniedCausationVSintervention$u_ci,
  )
}

evaluate_fit_joint <- function(fit) {
  # collect pairwise comparisons
  
  deniedCausationVScommonCause <- faintr::compare_groups(
    fit, 
    lower = condition == "deniedCausation",
    higher  = condition == "commonCause"
  )
  
  commonCauseVSassociation <- faintr::compare_groups(
    fit, 
    lower = condition == "commonCause",
    higher  = condition == "association"
  )
  
  associationVSintervention <- faintr::compare_groups(
    fit, 
    lower = condition == "association",
    higher  = condition == "intervention"
  )
  
  deniedCaustaionVSassociation <- faintr::compare_groups(
    fit, 
    lower = condition == "deniedCausation",
    higher  = condition == "association"
  )
  
  deniedCausationVSintervention <- faintr::compare_groups(
    fit, 
    lower = condition == "deniedCausation",
    higher  = condition == "intervention"
  )
  
  pilot1aVSpilot1b <- faintr::compare_groups(
    fit,
    lower = version == "pilot-1b",
    higher = version == "pilot-1a"
  )
  
  tribble(
    ~comparison, ~posterior, ~"95%HDI (low)", ~"95%HDI (high)",
    "pilot 1b < pilot 1a" , pilot1aVSpilot1b$post_prob, pilot1aVSpilot1b$l_ci, pilot1aVSpilot1b$u_ci,
    "denied causation < common cause" , deniedCausationVScommonCause$post_prob, deniedCausationVScommonCause$l_ci, deniedCausationVScommonCause$u_ci,  
    "common cause < association" , commonCauseVSassociation$post_prob, commonCauseVSassociation$l_ci, commonCauseVSassociation$u_ci,
    "association < intervention" , associationVSintervention$post_prob, associationVSintervention$l_ci, associationVSintervention$u_ci,
    "denied causation < association" , deniedCaustaionVSassociation$post_prob, deniedCaustaionVSassociation$l_ci, deniedCaustaionVSassociation$u_ci,
    "denied causation < intervention" , deniedCausationVSintervention$post_prob, deniedCausationVSintervention$l_ci, deniedCausationVSintervention$u_ci,
  )
}

d1 <- load_wrangle_data('01-raw-data-pilot-1a.csv', '01-wrangled-data-pilot-1a.csv', 'pilot-1a')
d2 <- load_wrangle_data('01-raw-data-pilot-1b.csv', '01-wrangled-data-pilot-1b.csv', 'pilot-1b')

plot_d1 <- make_barplot(d1, "w/o mentioning costs")
plot_d2 <- make_barplot(d2, "w/ explicit costs")

plot_d1
plot_d2

ggsave(plot_d1, filename = 'result-exp1a.png', width = 8, height = 6)
ggsave(plot_d2, filename = 'result-exp1b.png', width = 8, height = 6)

fitt_d21 <- run_regression_single(d1)
fit2 <- run_regression_single(d2)

fit_joint <- run_regression_joint(rbind(d1,d2))

evaluate_fit(fit1)
evaluate_fit(fit2)

evaluate_fit_joint(fit_joint)
