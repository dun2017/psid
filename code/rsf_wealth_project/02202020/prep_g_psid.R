############################################################################
rm(list=ls())
set.seed(122290)
ptm <- proc.time()
## Options needed for this run.
# repo <- 'E:/Data/Fragile_Families/'
# local_cores <- 1
# use_image <- TRUE
# image_path <- 'E:/Data/Fragile_Families/11142018/'
# package_lib <- 'E:/Data/Fragile_Families/uploads/11072018/g_packages'
# .libPaths(c(.libPaths(), package_lib))
# library(session)
# out_dir <- 'E:/Data/Fragile_Families/results/july11/'
# setwd(repo)
# package_lib <- 'E:/Data/Fragile_Families/uploads/11072018/g_packages'
# .libPaths(c(.libPaths(), package_lib))
# library(session)
# library(data.table, lib.loc = package_lib)
# library(tidyverse, lib.loc = package_lib)
# library(mice, lib.loc = package_lib)
# library(dplyr, lib.loc = package_lib)
# library(nnet, lib.loc = package_lib)
# library(parallel, lib.loc = package_lib)
library(data.table)
library(tidyverse)
library(mice)
library(dplyr)
library(nnet)
library(parallel)
library(survey)
library(boot)
library(MASS)
library(snow)
select <- dplyr::select
source("C:/Users/ngraetz/Documents/repos/psid/code/rsf_wealth_project/02202020/master_g_functions.R")
effect_paths <- 'wealth_sd_cds_bmi_sd'

############################################################################

## Inputs for g-formula
path_cb <- fread(paste0("C:/Users/ngraetz/Documents/repos/psid/code/rsf_wealth_project/02202020/effectall_paths_",effect_paths,".csv"))
weight_var <- 'cds_child_weight'

## Change wealth or outcome variable?

## Load data
# DF <- readRDS("E:/Data/Fragile_Families/uploads/02122020/imputed_input_data_for_secure_v2.RDS")
# DF <- readRDS("E:/Data/Fragile_Families/uploads/02202020/imputed_input_data_for_secure_v4.RDS")
DF <- readRDS("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/imputed_input_data_for_secure_MARCH16.RDS")
DF_imp_list <- DF[[1]] ## Full dataset long on imputation for feeding through g-formula simulation.
mi_list <- DF[[2]] ## Special imputation list object of all imputed datasets for fitting/pooling survey-weighted GLMs.
rm(DF)

## Load variable lists from path codebook
tc_vars <- unique(unlist(lapply(path_cb[, tc_vars], function(x) unlist(strsplit(x,',')))))
tv_vars <- unique(unlist(lapply(path_cb[, tv_vars], function(x) unlist(strsplit(x,',')))))
binary_cols <- c(tc_vars,tv_vars)[grepl('as.factor',c(tc_vars,tv_vars))]
tv_vars <- gsub('as[.]factor[(]','',tv_vars)
tv_vars <- gsub('[)]','',tv_vars)

## Make long imputed dataset to pool for simulations
fix_imps <- function(d,return_wide) {
  d <- as.data.table(d)
  d[, married := as.numeric(as.character(married))]
  d[, college := as.numeric(as.character(college))]
  d[, male := as.numeric(as.character(male))]
  d[, black := as.numeric(as.character(ifelse(head_race=='black',1,0)))]
  d <- d[!is.na(cds_child_weight)] ## Drop a few children who were not observed in 2002, 2007 and drop after 2007 (for now)
  d[, actual_age := age]
  d <- d[actual_age %in% 3:18, ] ## Subset to include lagged 1995 exposures (two years before CDS cohort)
  ## Copy to 1995 for "t=1", but we can just change the names of all variables in the other years to "lagged" because
  ## they were already merged on from previous main waves (at least just for wealth, will have to actually lag CDS variables)
  # d[, below1k := ifelse(fam_wealth_weq_sd <= -.435, 1, 0)]
  d[, below1k := ifelse(fam_wealth<1000, 1, 0)]
  d1995 <- copy(d[cds_year==1997,]) 
  d1995[, year := 1995]
  d1995[, cds_year := 1995]
  d1995[, actual_age := actual_age-2]
  d <- d[order(id,age)]
  d <- rbind(d1995,d)
  ## Recode variables to be time-specific given effects codebook
  d[cds_year==1995, age := 1]
  d[cds_year==1997, age := 2]
  d[cds_year==2002, age := 3]
  d[cds_year==2007, age := 4]
  d[, pweight := cds_child_weight]
  ## RESHAPE WIDE ON T
  tv_vars_reshape <- c('pweight',unique(gsub('_1|_2|_3|_4','',tv_vars)))
  d_wide <- dcast(d, as.formula(paste0('id+',paste(tc_vars,collapse='+'),'~age')), value.var=tv_vars_reshape)
  ## Make everything numeric
  for(v in binary_cols) {
    v_num <- gsub('as[.]factor[(]','',v)
    v_num <- gsub('[)]','',v_num)
    d_wide[, (v_num) := as.numeric(as.character(get(v_num)))]
  }
  if(return_wide) return_d <- d_wide
  if(!return_wide) return_d <- d
  return(return_d)
}
DF_imp_long <- rbindlist(lapply(DF_imp_list[[1]], fix_imps, return_wide=F))
DF_imp_list[[1]] <- lapply(DF_imp_list[[1]], fix_imps, return_wide=T)

## Make formulas and set up model families
make_formula <- function(v) {
  f <- paste0(v,' ~ ',paste0(path_cb[update_vars==v, gsub(',','+',(tc_vars))],'+',
                             path_cb[update_vars==v, gsub(',','+',(tv_vars))]))
  as.formula(f)
}
formulas <- lapply(unique(path_cb[, update_vars]), make_formula)
names(formulas) <- unique(path_cb[, update_vars])
families <- lapply(path_cb[, family], get)
names(families) <- path_cb[, family]

############################################################################
############### SET SURVEY DESIGN, FIT MODELS, RUN G-FORMULA ###############
############################################################################
## Fit full set of longitudinal models, creating a table of adjusted and unajusted effects to compare later.
## Need a design for each time step
all_models <- list()
for(t in 2:max(path_cb[,age])) {
  message(paste0('Fitting models for step: ',t))
  ## Formulas for this step
  t_formulas <- formulas[grepl(paste0('_',t),names(formulas))] 
  t_families <- families[grepl(paste0('_',t),names(formulas))]
  ## Non-censored sample for this step (but weights are longitudinal, correcting for censoring)
  grab_period <- function(d,t) {
    t_d <- d[!is.na(get(paste0('pweight_',t))),]
    ## Add actual age for this model
    # t_d[, actual_age := get(paste0('actual_age_',t))]
    return(t_d)
  }
  t_DF_imp_list <- DF_imp_list
  t_DF_imp_list[[1]] <- lapply(t_DF_imp_list[[1]], grab_period, t)
  ## Survey design for this step
  ff.design <- svydesign(id=~id, weights=as.formula(paste0('~pweight_',t)), data=t_DF_imp_list)
  ## Models for this step
  t_models <- gfit.init.survey.imputed(t_formulas, t_families, survey_design=ff.design)
  all_models <- c(all_models,t_models)
}
for(i in 1:length(families)) all_models[[i]]$family$family <- names(families[i])
names(all_models) <- unlist(lapply(formulas, function(x) all.vars(x)[1]))

## Pool imputed datasets
clean_factor_vars <- gsub('as[.]factor[(]','',binary_cols)
clean_factor_vars <- gsub('[)]','',clean_factor_vars)
for(a in 1:max(path_cb[, age])) clean_factor_vars <- clean_factor_vars[!grepl(paste0('_',a),clean_factor_vars)]
for(v in 1:length(clean_factor_vars)) DF_imp_long[, (paste0(binary_cols[v],'1')) := get(clean_factor_vars[v])]
## Process time-varying binary variables
DF_imp_long <- add_tv_factors(DF_imp_long, prep=T)
## Process other 
clean_tv_vars <- gsub('_1','',tv_vars[grepl('_1',tv_vars)])
for(v in clean_tv_vars) DF_imp_long[age==1, (paste0(v,'_1')) := get(v)]
collapse_vars <- c(tc_vars[!(tc_vars %in% binary_cols)], paste0(binary_cols,'1'), tv_vars[grepl('_1',tv_vars)])
DF_pooled <- DF_imp_long[, lapply(.SD, mean), by=c('id','age','actual_age','pweight'), .SDcols=collapse_vars]
ff.design.pooled <- svydesign(id=~id, weights=~pweight, data=DF_pooled)

## Define intervention rules for g-formula.
intervention_rules_bad <- function(d) {
  d[, fam_wealth_weq_sd_1 := -1]
  d[, fam_wealth_weq_sd_2 := -1]
  d[, fam_wealth_weq_sd_3 := -1]
  d[, fam_wealth_weq_sd_4 := -1]
  return(d)
}
intervention_rules_good <- function(d) {
  d[, fam_wealth_weq_sd_1 := 1]
  d[, fam_wealth_weq_sd_2 := 1]
  d[, fam_wealth_weq_sd_3 := 1]
  d[, fam_wealth_weq_sd_4 := 1]
  return(d)
}

## Add function to create quintiles during simulation to special_rules.
## Need to hardcode quintile cuts to special function (idk how else to code this ahead of time).
special_rules <- NULL

## SAVE ALL G-FORMULA INPUTS, CLOSE R SESSION, LAUNCH FROM A NEW ONE.
data <- DF_pooled[age==1, ]
models <- all_models
duration_vars <- c()
ever_vars <- c()
dummy_vars <- c()
age_interaction_vars <- c()
input_object_names <- c('repo','package_lib','data','models','tc_vars','weight_var','ff.design.pooled',
                        'duration_vars','ever_vars','dummy_vars','age_interaction_vars','binary_cols','intervention_rules_bad','intervention_rules_good','special_rules')
input_objects <- lapply(input_object_names, get)
names(input_objects) <- input_object_names
saveRDS(input_objects, 'E:/Data/addhealth/imputed_data/input_objects_psid.RDS')
