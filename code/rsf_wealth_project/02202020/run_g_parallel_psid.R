#########################################################################################################
## (0) Libraries and functions
#########################################################################################################
rm(list=ls())
repo <- 'E:/Data/Fragile_Families/'
setwd(repo)
package_lib <- 'E:/Data/Fragile_Families/uploads/11072018/g_packages'
.libPaths(c(.libPaths(), package_lib))
library(data.table, lib.loc = package_lib)
library(mice, lib.loc = package_lib)
library(survey)
library(boot)
library(MASS)
library(snow)
library(miceadds)
library(mitools)
library(ggplot2)
source("E:/Data/addhealth/code/master_g/master_g_functions.R")

#########################################################################################################
## Choose 1) models, 2) interventions to calculate and 3) outcome 
effect_paths <- 'wealth_sd_cds_bmi_sd'
control_course <- 'control_wealth_sd_minus1'
intervention_course <- 'treatment_wealth_sd_plus1'
outcome_vars <- 'cds_bmi_sd'
#########################################################################################################

#########################################################################################################
## (1) G-formula setup
#########################################################################################################
## Load all input objects for this g-formula run.
path_cb <- fread(paste0("E:/Data/Fragile_Families/uploads/02112020/effectall_paths_",effect_paths,".csv"))
sim_dir <- 'E:/Data/addhealth/psid/course_simulations/'
input_objects <- readRDS('E:/Data/addhealth/imputed_data/input_objects_psid.RDS')
for(obj in names(input_objects)) assign(obj, input_objects[[obj]])
total_sim_count <- 500
processors <- 40
parallelize <- T
use_mean_betas <- F
## Specify intervention rules and natural rules as a list of functions to apply to the data. 
natural_courses <- list(
  function(d) {
    return(d)
  }
)
names(natural_courses) <- 'natural_course'
intervention_rules <- list(
  function(d) {
    d[, fam_wealth_weq_sd_1 := -1]
    d[, fam_wealth_weq_sd_2 := -1]
    d[, fam_wealth_weq_sd_3 := -1]
    d[, fam_wealth_weq_sd_4 := -1]
    return(d)
  },
  function(d) {
    d[, `as.factor(below1k_1)1` := 1]
    d[, `as.factor(below1k_2)1` := 1]
    d[, `as.factor(below1k_3)1` := 1]
    d[, `as.factor(below1k_4)1` := 1]
    return(d)
  },
  function(d) {
    d[, fam_wealth_weq_sd_1 := 1]
    d[, fam_wealth_weq_sd_2 := 1]
    d[, fam_wealth_weq_sd_3 := 1]
    d[, fam_wealth_weq_sd_4 := 1]
    return(d)
  },
  function(d) {
    d[, `as.factor(below1k_1)1` := 0]
    d[, `as.factor(below1k_2)1` := 0]
    d[, `as.factor(below1k_3)1` := 0]
    d[, `as.factor(below1k_4)1` := 0]
    return(d)
  }
)
names(intervention_rules) <- c('control_wealth_sd_minus1','control_wealth_below1k',
                               'treatment_wealth_sd_plus1','treatment_wealth_above1k')
intervention_rules <- intervention_rules[names(intervention_rules) %in% c(control_course,intervention_course)]

#########################################################################################################
## (2) Calculate g-formulas
#########################################################################################################
ptm <- proc.time()
if(!parallelize) {
  ## Calculate all g-formulas serially on local machine
  message(paste0('Running ',total_sim_count,' draws locally'))
  message(paste0('\nInterventions selected: ', names(intervention_rules)))
  g <- lapply(1:total_sim_count, run_gformula_bw_cluster,
              data=data, outcome_vars=outcome_vars, 
              models=models, binary_cols=binary_cols, steps=1:max(path_cb[, age]),
              sim_dir = sim_dir, path_cb = path_cb, mc_replicates = 20,
              decomp = TRUE, decomp_compare_df = control_course,
              natural_courses, intervention_rules, mean_betas = use_mean_betas)
  # natural_agg <- readRDS("E:/Data/addhealth/psid/course_simulations/natural_course/0/sim1.RDS")
  # control_agg <- readRDS("E:/Data/addhealth/psid/course_simulations/control_below1k_course/0/sim1.RDS")
  # treatment_agg <- readRDS("E:/Data/addhealth/psid/course_simulations/treatment_below1k_course/0/sim1.RDS")
}
if(parallelize) {
  ## Initialize parallel computing cluster
  message(paste0('Running ',total_sim_count,' draws in parallel using ', processors, ' processors'))
  message(paste0('\nInterventions selected: ', names(intervention_rules)))
  cluster <- makeCluster(processors)
  clusterExport(cluster, list('repo','package_lib'))
  clusterEvalQ(cluster, .libPaths(c(.libPaths(), package_lib)))
  clusterEvalQ(cluster, library(data.table, lib.loc = package_lib))
  clusterEvalQ(cluster, library(pillar))
  clusterEvalQ(cluster, library(mice, lib.loc = package_lib))
  clusterEvalQ(cluster, library(survey, lib.loc = package_lib))
  clusterEvalQ(cluster, library(boot))
  clusterEvalQ(cluster, library(MASS))
  clusterEvalQ(cluster, library(miceadds, lib.loc = package_lib))
  clusterEvalQ(cluster, library(mitools, lib.loc = package_lib))
  clusterEvalQ(cluster, source("E:/Data/addhealth/code/master_g/master_g_functions.R"))
  clusterExport(cluster, list('path_cb'))
  # clusterEvalQ(cluster, path_cb <- fread(paste0("E:/Data/Fragile_Families/uploads/02112020/effect_paths_",effect_paths,".csv")))
  ## Distribute g-formula jobs
  clusterApplyLB(cluster, 1:total_sim_count, run_gformula_bw_cluster,
                 data=data, outcome_vars=outcome_vars, 
                 models=models, binary_cols=binary_cols, steps=1:max(path_cb[, age]),
                 sim_dir = sim_dir, path_cb = path_cb, mc_replicates = 20,
                 decomp = TRUE, decomp_compare_df = control_course,
                 natural_courses, intervention_rules, mean_betas = use_mean_betas)
  ## Stop cluster
  stopCluster(cluster)
}
print(proc.time() - ptm)
#########################################################################################################
#########################################################################################################

#########################################################################################################
## (3) Summarize results 
#########################################################################################################
## Post-process all g-formula results
lapply(c(names(natural_courses),names(intervention_rules)), post_process_course, total_sims=total_sim_count, sim_dir=sim_dir)
## Calculate all effects for given outcome and interventions
o <- paste0(outcome_vars,'_',max(path_cb[, age]))
effect_table <- get_course_effect(intervention_course=intervention_course,
                                  compare_course=control_course, 
                                  outcome_var=o,
                                  total_sims=total_sim_count, sim_dir=sim_dir)
paths <- list.dirs(paste0(sim_dir,'/',intervention_course,'/effects/'), recursive = F, full.names = F)
gg_effects <- effect_table[, c('intervention',paste0('NIE_',paths)), with=F]
gg_effects <- melt(gg_effects, id.vars='intervention')
effect_gg <- ggplot(data=gg_effects) + 
  geom_bar(aes(x=as.factor(intervention),
               y=value,
               fill=variable),
           color='black',
           stat='identity') +
  geom_hline(yintercept=0, size=2) + 
  scale_fill_manual(values=c('#e41a1c','#4daf4a','#377eb8','#984ea3','#ff7f00','#ffff33','#a65628','grey')) + 
  labs(y=o,x='Intervention',fill='Direct and\nindirect effects') + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, margin = margin(r=10)),
        axis.title.x = element_text(size = 20, margin = margin(t=10)),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) 
print(effect_gg)
print(effect_table)