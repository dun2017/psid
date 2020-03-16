# SURVEY WEIGHTS: function for running multiple model fits simulataneously
gfit.init.survey.imputed <- function(formulas, families, survey_design, data=NULL, kwargs=NULL, sub=NULL){
  if(class(formulas) != "list"){
    stop("formulas argument must be a list of formulas.")
  }
  if(class(families) != "list"){
    stop("families argument must be a list of family objects.")
  }
  parLengths <- sapply(list(formulas, families), length)
  if(length(unique(parLengths)) != 1){
    stop("families, formulas, and functions arguments must be same length.")
  }
  lapply(1:length(formulas), function(i){
    message(paste0('Fitting formula ', i, ', ',names(families[i])))
    ## Fit model function call (svyglm) over imputation list
    if(names(families[i]) %in% c('gaussian','quasibinomial')) {
      model_list <- with(survey_design, svyglm(formula=formulas[[i]],
                                               family=families[[i]],
                                               design=survey_design))
    }
    if(names(families[i]) %in% 'svyolr') {
      model_list <- with(survey_design, svyolr(formula=formulas[[i]],
                                               design=survey_design))
    }
    ## Extract fitted betas/vcovs and pool
    beta_list <- lapply(model_list, FUN = function(x){coef(x)})
    vcov_list <- lapply(model_list, FUN = function(x){vcov(x, complete=T)})
    pooled_model <- miceadds::pool_mi(qhat = beta_list, u = vcov_list)
    return(pooled_model)
  })
}

add_tv_factors <- function(d,prep=F) {
  if(!prep) {
    for(a in 1:max(d[, age])) {
      age_factors <- binary_cols[grepl(paste0('_',a,'[)]'),binary_cols)]
      clean_age_factors <- gsub('as[.]factor[(]','',age_factors)
      clean_age_factors <- gsub('[)]','',clean_age_factors)
      for(f in clean_age_factors) d[age==a, (paste0('as.factor(',f,')1')) := get(f)]
    }
  }
  if(prep) {
    for(a in 1:max(d[, age])) {
      age_factors <- binary_cols[grepl(paste0('_',a,'[)]'),binary_cols)]
      clean_age_factors <- gsub('as[.]factor[(]','',age_factors)
      clean_age_factors <- gsub(paste0('_',a,'[)]'),'',clean_age_factors)
      for(f in clean_age_factors) d[age==a, (paste0('as.factor(',f,'_',a,')1')) := get(f)]
    }
  }
  return(d)
}

# data
# models
# outcome_vars
# weight_var
# effect_path
# duration_vars
# ever_vars
# dummy_vars
# age_interaction_vars
# binary_cols
# b=1
# steps=1:max(path_cb[, age])
# mc_replicates = 20
# decomp = TRUE
# decomp_compare_df = 'control_course'
# mean_betas = TRUE
# intervention_subset = NULL
# age_interaction_vars = NULL
# duration_vars = NULL
# ever_vars = NULL
# dummy_vars = NULL
# mean_betas = FALSE
# special_rules = NULL
# special_vars = NULL
# extra_save_vars = NULL
run_gformula_bw_cluster <- function(b, data, outcome_vars, models, binary_cols, steps, sim_dir, path_cb, mc_replicates,
                                    decomp, decomp_compare_df, natural_courses, intervention_rules, intervention_subset = NULL,
                                    age_interaction_vars = NULL, duration_vars = NULL, ever_vars = NULL, dummy_vars = NULL,
                                    mean_betas = FALSE, special_rules = NULL, special_vars = NULL, extra_save_vars = NULL) {
  
  # test <- c(b, outcome_vars, binary_cols)
  # write.csv(test, 'E:/Data/addhealth/psid/args.csv')
  ## Update output dir so that sims are split into groups of 250 for saving in any particular folder.
  ## Having thousands of tiny files in a single directory makes things uber slow.
  message(intervention_subset)
  message(paste0('Sample ', b))
  batch <- round(b/250)
  # write.csv(binary_cols, 'E:/Data/addhealth/psid/course_simulations/control_below1k_course/0/binary.csv')
  
  # Do not double-count special rules variables in the "effect_paths", even though we use this to decompose total effects.
  update_vars <- path_cb[, update_vars]
  binary_cols <- paste0(binary_cols,'1')
  
  # Multiply dataset X times to reduce MC error from individual-level prediction.
  # Update "id" variable to be "id+mc" to avoid lagging incorrectly within "id" in functions below.
  # Order by id, age.
  mc_data <- function(mc, d) {
    d <- copy(data)
    d[, id := paste0(id,'_',mc)]
    return(d)
  }
  data <- as.data.table(data)
  data <- data[age==min(age), ]
  data[, actual_age_1 := actual_age]
  if(sum(c('id','age','pweight') %in% names(data))!=3) stop('Data must contain variables id, age, pweight')
  data <- rbindlist(lapply(1:mc_replicates, mc_data, d=data))
  
  # Create an unweighted, nationally representative black and white dataset using the weighted data.
  # We already accounted for the weighting in the models, so we now have nationally representative models anyway.
  # This makes sampling in interventions (e.g. drawing from the white natural course) more straightforward.
  data <- data[sample(.N,replace=T,prob=pweight)]
  ## Need to update ids to stay unique after sampling with replacement
  data[, new_id := 1:.N, by=c('id','age')]
  data[, id := paste0(id,'_',new_id)]
  data[, new_id := NULL]
  
  # Need to stochasitcally draw from predicted probabilities of binary variables pooled across imputed datasets.
  data[, (binary_cols) := lapply(.SD, function(x) {rbinom(nrow(data), 1, x)}), .SDcols = binary_cols]
  
  test <- c(b, outcome_vars, binary_cols)
  write.csv(test, 'E:/Data/addhealth/psid/args2.csv')
  
  # Sample betas
  if(mean_betas) betas_draw <- lapply(models, function(m) coef(m)) ## For testing points estimates only
  if(!mean_betas) betas_draw <- lapply(models, function(m) mvrnorm(1, mu = coef(m), Sigma = vcov(m)))
  names(betas_draw) <- names(models)
  
  # Estimate natural course(s).
  for(n in names(natural_courses)) {
    message(paste0('   Simulating ', n, ' course...'))
    natural_function <- natural_courses[[n]]
    natural_course <- progressSimulation_fast(data=natural_function(data), models, betas_draw, binary_cols,
                                              duration_vars, ever_vars, dummy_vars,
                                              effect_cb=path_cb)
    natural_course[, sim := b]
    collapse_vars <- names(natural_course)[!(names(natural_course) %in% c('id','age','actual_age','pweight'))]
    natural_agg <- natural_course[, lapply(.SD, mean), .SDcols=collapse_vars, by='sim']
    natural_agg[, name := n]
    dir.create(paste0(sim_dir, '/',n,'/'), showWarnings=F)
    dir.create(paste0(sim_dir, '/',n,'/', batch),showWarnings=F)
    saveRDS(natural_agg, paste0(sim_dir,'/',n,'/',batch,'/sim',b,'.RDS'))
    saveRDS(natural_course, paste0(sim_dir,'/',n,'/',batch,'/sim',b,'_micro.RDS'))
  }
  
  # Estimate intervention courses
  # message('   Simulating treatment course...')
  # treatment_course <- progressSimulation_fast(data=data, models, betas_draw,
  #                                             duration_vars, ever_vars, dummy_vars,
  #                                             effect_cb=path_cb, intervention_rules=intervention_rules_good)
  # treatment_course[, sim := b]
  # treatment_agg <- treatment_course[, lapply(.SD, mean), .SDcols=collapse_vars, by='sim']
  # treatment_agg[, name := 'treatment_course']
  # dir.create(paste0(sim_dir, '/treatment_course'),showWarnings=F)
  # dir.create(paste0(sim_dir, '/treatment_course/', batch),showWarnings=F)
  # saveRDS(treatment_agg, paste0(sim_dir,'/treatment_course/',batch,'/sim',b,'.RDS'))
  # message('   Simulating control course...')
  # control_course <- progressSimulation_fast(data=data, models, betas_draw,
  #                                             duration_vars, ever_vars, dummy_vars,
  #                                             effect_cb=path_cb, intervention_rules=intervention_rules_bad)
  # control_course[, sim := b]
  # control_agg <- control_course[, lapply(.SD, mean), .SDcols=collapse_vars, by='sim']
  # control_agg[, name := 'control_course']
  # dir.create(paste0(sim_dir, '/control_course'),showWarnings=F)
  # dir.create(paste0(sim_dir, '/control_course/', batch),showWarnings=F)
  # saveRDS(control_agg, paste0(sim_dir,'/control_course/',batch,'/sim',b,'.RDS'))
  for(n in names(intervention_rules)) {
    message(paste0('   Simulating ', n, ' course...'))
    intervention_function <- intervention_rules[[n]]
    int_data <- copy(data)
    if(!is.null(intervention_subset)) int_data <- intervention_subset(data)
    intervention_course <- progressSimulation_fast(data=int_data, models, betas_draw, binary_cols,
                                                   duration_vars, ever_vars, dummy_vars,
                                                   effect_cb=path_cb, intervention_rules=intervention_function)
    intervention_course[, sim := b]
    collapse_vars <- names(intervention_course)[!(names(intervention_course) %in% c('id','age','actual_age','pweight'))]
    intervention_agg <- intervention_course[, lapply(.SD, mean), .SDcols=collapse_vars, by='sim']
    intervention_agg[, name := n]
    dir.create(paste0(sim_dir, '/',n,'/'), showWarnings=F)
    dir.create(paste0(sim_dir, '/',n,'/', batch),showWarnings=F)
    saveRDS(intervention_agg, paste0(sim_dir,'/',n,'/',batch,'/sim',b,'.RDS'))
    saveRDS(intervention_course, paste0(sim_dir,'/',n,'/',batch,'/sim',b,'_micro.RDS'))
    ## Decompose ATE for this intervention
    if(decomp & n!=decomp_compare_df) {
      ## Read microdata simulants for the two courses we are drawing from to decompose effects.
      control_course <- readRDS(paste0(sim_dir,'/',decomp_compare_df,'/',batch,'/sim',b,'_micro.RDS'))
      control_course[, age := 1]
      ## Set up decomposition pathways.
      effect_paths <- path_cb[, update_vars]
      clean_paths <- effect_paths
      for(t in path_cb[, unique(age)]) clean_paths <- gsub(paste0('_',t),'',clean_paths)
      clean_paths <- unique(clean_paths)
      clean_paths <- clean_paths[!(clean_paths %in% outcome_vars)]
      ## Loop over all possible pathways (automatically this is all time-varying pathways besides outcome variables).
      for(path in clean_paths) {
        message(paste0('   Simulating effect course: ', path, '...'))
        if(grepl('as[.]factor',path)) {
          path <- gsub('as[.]factor[(]','',path)
          path <- gsub('[)]','',path)
        }
        # Simulate course for this isolated pathway.
        tc_vars <- unique(unlist(lapply(path_cb[, tc_vars], function(x) unlist(strsplit(x,',')))))
        tv_vars <- unique(unlist(lapply(path_cb[, tv_vars], function(x) unlist(strsplit(x,',')))))
        all_vars <- unique(c(tc_vars,tv_vars,path_cb[, update_vars]))
        all_vars <- all_vars[!grepl('factor',all_vars)]
        all_vars <- c(all_vars,binary_cols)
        ## Draw EVERYTHING from natural course EXCEPT draw path values from TREATMENT course and PREDICT outcomes.
        these_intervention_vars <- all_vars[grepl(path,all_vars)]
        these_natural_vars <- all_vars[!grepl(path,all_vars)]
        for(o in outcome_vars) these_natural_vars <- these_natural_vars[!grepl(o,these_natural_vars)]
        # these_natural_vars <- these_natural_vars[these_natural_vars!='actual_age']
        course <- progressSimulation_fast(data=control_course, models, betas_draw, binary_cols,
                                          duration_vars, ever_vars, dummy_vars,
                                          effect_cb=path_cb, intervention_rules=NULL,
                                          natural_vars=these_natural_vars, intervention_var=these_intervention_vars,
                                          natural_DF=control_course, intervention_DF=intervention_course)
        course[, sim := b]
        course_agg <- course[, lapply(.SD, mean), .SDcols=collapse_vars, by='sim']
        course_agg[, name := path]
        dir.create(paste0(sim_dir, '/',n,'/effects'),showWarnings=F)
        dir.create(paste0(sim_dir, '/',n,'/effects/', path),showWarnings=F)
        dir.create(paste0(sim_dir, '/',n,'/effects/', path, '/', batch),showWarnings=F)
        saveRDS(course_agg, paste0(sim_dir, '/',n,'/effects/', path, '/', batch,'/sim',b,'.RDS'))
      }
    }
  }
  
  ## All direct and indirect effects of ATE
  # if(direct_and_indirect) {
  #   effect_paths <- path_cb[, update_vars]
  #   clean_paths <- effect_paths
  #   for(t in path_cb[, unique(age)]) clean_paths <- gsub(paste0('_',t),'',clean_paths)
  #   clean_paths <- unique(clean_paths)
  #   clean_paths <- clean_paths[!(clean_paths %in% outcome_vars)]
  #   for(path in clean_paths) {
  #     message(paste0('   Simulating effect course: ', path, '...'))
  #     # Simulate course for this isolated pathway.
  #     tc_vars <- unique(unlist(lapply(path_cb[, tc_vars], function(x) unlist(strsplit(x,',')))))
  #     tv_vars <- unique(unlist(lapply(path_cb[, tv_vars], function(x) unlist(strsplit(x,',')))))
  #     all_vars <- unique(c(tc_vars,tv_vars,path_cb[, update_vars]))
  #     all_vars <- all_vars[!grepl('factor',all_vars)]
  #     all_vars <- c(all_vars,binary_cols)
  #     ## Draw EVERYTHING from natural course EXCEPT draw path values from TREATMENT course and PREDICT outcomes.
  #     these_intervention_vars <- all_vars[grepl(path,all_vars)]
  #     these_natural_vars <- all_vars[!grepl(path,all_vars)]
  #     for(o in outcome_vars) these_natural_vars <- these_natural_vars[!grepl(o,these_natural_vars)]
  #     these_natural_vars <- these_natural_vars[these_natural_vars!='actual_age']
  #     course <- progressSimulation_fast(data=data, models, betas_draw,
  #                                       duration_vars, ever_vars, dummy_vars,
  #                                       effect_cb=path_cb, intervention_rules=NULL,
  #                                       natural_vars=these_natural_vars, intervention_var=these_intervention_vars,
  #                                       natural_DF=control_course, intervention_DF=treatment_course)
  #     course[, sim := b]
  #     course_agg <- course[, lapply(.SD, mean), .SDcols=collapse_vars, by='sim']
  #     course_agg[, name := path]
  #     dir.create(paste0(sim_dir, '/', path),showWarnings=F)
  #     dir.create(paste0(sim_dir, '/',path,'/', batch),showWarnings=F)
  #     saveRDS(course_agg, paste0(sim_dir,'/',path,'/',batch,'/sim',b,'.RDS'))
  #   }
  # }
  message('Sample finished!')
  return(NULL)
  
}

# data=natural_function(data)
# effect_cb=path_cb
# interaction_vars=NULL
# intervention_rules=NULL
# special_rules=NULL
# natural_vars=NULL
# intervention_var=NULL
# natural_DF=NULL
# intervention_DF=NULL
# special_vars=NULL
# special_var_names=NULL
# exp_vars=NULL
# paths=NULL
progressSimulation_fast <- function(data,
                                    models,
                                    betas_draw,
                                    binary_cols,
                                    duration_vars=NULL,
                                    ever_vars=NULL,
                                    dummy_vars=NULL,
                                    effect_cb,
                                    interaction_vars=NULL,
                                    intervention_rules=NULL,
                                    special_rules=NULL, 
                                    natural_vars=NULL, 
                                    intervention_var=NULL,
                                    natural_DF=NULL,
                                    intervention_DF=NULL,
                                    special_vars=NULL,
                                    special_var_names=NULL,
                                    exp_vars=NULL,
                                    paths=NULL){
  ## Setup
  ptm <- proc.time()
  simDF <- copy(data)
  steps <- 1:max(path_cb[, age])
  ## We have to assert the intervention rules at t=0 as well as in updating below.
  if(!is.null(intervention_rules)) simDF <- intervention_rules(simDF)
  ## We have to assert scenario rules for effect calculations at t=1 as well as in updating below.
  if(!is.null(natural_DF)) simDF <- effect_rules_fast(simDF, natural_vars, intervention_var, natural_DF, intervention_DF) 
  ## Reset duration-weighted variables to their t=1 values just in case the intervention affects these values (all because of pooled imputation).
  for(v in duration_vars) simDF[, (paste0('d.',v)) := get(v)]
  ## Reset "ever' variables to their t=0 values just in case the intervention affects these values (all because of pooled imputation).
  for(v in ever_vars) simDF[, (paste0('e.',v)) := get(v)]
  ## Assert special rules.
  if(!is.null(special_rules)) {
    names_before <- names(copy(simDF))
    simDF <- special_rules(simDF, models, betas, intervention_var, intervention_rules, natural_DF, intervention_DF)
    special_var_names <- names(simDF)[grep('numeric|integer', sapply(simDF, class))]
    special_var_names <- special_var_names[!(special_var_names %in% names_before)]
  }
  ## Add intercept variable for manual predicting with draws from our models.
  simDF[, ('(Intercept)') := 1]
  upDF <- copy(simDF)
  ## Simulate forward step by step, predicting all variables (or updating according to provided rules).
  message('      Beginning simulation course at time ', min(steps))
  for(y in (min(steps)+1):max(steps)) {
    message(paste0('      Simulating time ', y))
    ## Subset to this time step in effects codebook.
    age_cb <- effect_cb[age==y, ]
    ## Progress forward: update time step and actual age of observations.
    upDF[, age := age+1]
    upDF[, (paste0('actual_age_',y)) := get(paste0('actual_age_',y-1)) + unique(age_cb[, actual_inc])]
    ## Update one variable at a time in the order they come in the effects codebook.
    for(v in age_cb[, update_vars]) {
      ## If binary, make clean to match model name. This will get set back to factor at the end by add_tv_factors()
        v <- gsub('as[.]factor[(]','',v)
        v <- gsub('[)]','',v)
      ## 1) Update special variables and duration/ever variables first.
        if(!is.null(special_rules)) upDF <- special_rules(upDF, models, betas_draw, intervention_var, intervention_rules, natural_DF, intervention_DF)
        upDF <- update_duration_ever(upDF, duration_vars, ever_vars)
      ## 2) Assert intervention/scenario, reset dummies/interactions, and assert intervention/scenario again.
        if(!is.null(intervention_rules)) upDF <- intervention_rules(upDF)
        upDF <- update_dummy_interaction(upDF, dummy_vars, interaction_vars)
        if(!is.null(intervention_rules)) upDF <- intervention_rules(upDF)
      ## 3) Predict new variable from models
        upDF <- natural_rules_fast(upDF, v, models, betas_draw)
        ## Update for next prediction/draw steps if it is a binary variable.
        if(sum(grepl(v,binary_cols))!=0) {
          upDF[, (paste0('as.factor(',v,')1')) := NULL]
          upDF[, (paste0('as.factor(',v,')1')) := get(v)]
          upDF[, (v) := NULL]
        }
      ## 4) Replace predictions with either scenario values or intervention rules, if necessary.
        if(!is.null(natural_DF)) upDF <- effect_rules_fast(upDF, natural_vars, intervention_var, natural_DF, intervention_DF) 
        if(!is.null(intervention_rules)) upDF <- intervention_rules(upDF)
    }
  }
  print(proc.time() - ptm)
  return(upDF)
}

natural_rules_fast <- function(d, update_vars, models, betas) {
  for(v in update_vars) d[, (v) := simPredict_fast(v, d, models, betas)]
  return(d)
}

simPredict_fast <- function(v, DF, models, betas){
  model_index <- match(v, names(models))
  model_ <- models[[model_index]]
  betas_ <- betas[[model_index]]
  if(model_$family$family %in% c('binomial','quasibinomial')) {
    newDF <- as.data.table(DF[age==max(age), ])
    beta_names <- names(betas_)
    newDF <- newDF[, beta_names, with=F]
    setcolorder(newDF, beta_names)
    predicted_probs <- inv.logit(as.numeric(betas_ %*% t(as.matrix(newDF))))
    sim <- as.integer(rbinom(nrow(newDF), 1, predicted_probs))
  }
  if(model_$family$family == 'gaussian') {
    newDF <- as.data.table(DF[age==max(age), ])
    beta_names <- names(betas_)
    newDF <- newDF[, beta_names, with=F]
    setcolorder(newDF, beta_names)
    sim <- as.numeric(betas_ %*% t(as.matrix(newDF)))
    ## MANUALLY ADD LINEAR PROBABILITY MODEL EXCEPTIONS HERE (CODEBOOK LATER) - "sim" is the linear model predicted probabilities, so need to draw 1/0s.
    if(v %in% c('college','in_school','employed')) {
      sim[sim>1] <- 1
      sim[sim<0] <- 0
      sim <- as.integer(rbinom(nrow(newDF), 1, sim))
    }
  }
  if(model_$family$family == 'svyolr') {
    ints <- betas_[grep('[|]', names(betas_))]
    beta_vals <- betas_[!(names(betas_) %in% names(ints))]
    beta_names <- names(beta_vals)
    newDF <- as.data.table(DF[age==max(age), ])
    newDF <- newDF[, beta_names, with=F]
    setcolorder(newDF, beta_names)
    linear_part <- data.table(linear=as.numeric(beta_vals %*% t(as.matrix(newDF))))
    all_cat_names <- unlist(lapply(strsplit(names(ints),'[|]'), function(x) x[1]))
    linear_part[, (all_cat_names[1]) := (1 / (1 + exp(-(ints[1]-linear))))]
    for(int in 2:length(ints)) {
      cat <- all_cat_names[int]
      linear_part[, (cat) := (1 / (1 + exp(-(ints[int]-linear)))) - (1 / (1 + exp(-(ints[int-1]-linear))))]
    }
    last_cat <- strsplit(names(ints)[length(ints)],'[|]')[[1]][2]
    linear_part[, (last_cat) := 1 - (1 / (1 + exp(-(ints[length(ints)]-linear))))]
    linear_part[, linear := NULL]
    sim <- as.character(rMultinom(as.matrix(linear_part),1))
  }
  return(sim)
}

update_dummy_interaction <- function(d, dummy_vars, interaction_vars) {
  for(i in interaction_vars) {
    for(v in names(d)[!(names(d) %in% c('id','age','actual_age','pweight'))])
    d[, (paste0(v,':',i)) := get(i) * get(v)]
    d[, (paste0(i,':',v)) := get(i) * get(v)] 
  }
  for(v in dummy_vars) {
    for(val in unique(data[, get(gsub('l.','',v))])) upDF[, (paste0(v,val)) := ifelse(get(v)==val, 1, 0)]
  }
  return(d)
}

update_duration_ever <- function(d, duration_vars, ever_vars) {
  for(v in duration_vars) {
    d[, (paste0('d.',v)) := rowSums(.SD, na.rm=T), .SDcols = grep(v,names(d))]
    d[, (paste0('d.',v)) := get(paste0('d.',v)) / age]
  }
  for(v in ever_vars) {
    d[, (paste0('e.',v)) := rowSums(.SD, na.rm=T), .SDcols = grep(v,names(d))]
    d[, (paste0('e.',v)) := ifelse(get(paste0('e.',v))==0,0,1)]
  }
  return(d)
}

effect_rules_fast <- function(d, natural_vars, intervention_var, natural_DF, intervention_DF) {
  ## Replace all variable values with those from the natural course. 
  if(!is.null(natural_DF)) {
    nDF <- copy(natural_DF)
    setnames(nDF, natural_vars, paste0('n.',natural_vars))
    d <- merge(d, nDF[, c('id',paste0('n.', natural_vars)), with=F], by=c('id'))
    for(v in natural_vars) d[, (v) := get(paste0('n.',v))]
    d[, (paste0('n.',natural_vars)) := NULL]
  }
  ## Replace effect path variable with values from the intervention course.
  if(!is.null(intervention_DF)) {
    iDF <- copy(intervention_DF)
    setnames(iDF, intervention_var, paste0('i.',intervention_var))
    d <- merge(d, iDF[, c('id',paste0('i.', intervention_var)), with=F], by=c('id'))
    for(v in intervention_var) d[, (v) := get(paste0('i.',v))]
    d[, (paste0('i.',intervention_var)) := NULL]
  }
  return(d)
}

# course <- 'control_below1k_course'
# total_sims <- 45
post_process_course <- function(course, total_sims, sim_dir) {
  message(paste0('Processing ', course, ' course across ', total_sims, ' simulations...'))
  read_course_sim <- function(x, course) {
    batch <- round(x/250)
    readRDS(paste0(sim_dir,'/',course,'/',batch,'/sim',x,'.RDS'))
  }
  all_course_sims <- rbindlist(lapply(1:total_sims,read_course_sim,course))
  saveRDS(all_course_sims, paste0(sim_dir,'/', course, '_', total_sims, '.RDS'))
  ## Also process any direct/indirect courses for this ATE (just check any nested folders).
  if(dir.exists(paste0(sim_dir,'/',course,'/effects/'))) {
    paths <- list.dirs(paste0(sim_dir,'/',course,'/effects/'), recursive = F, full.names = F)
    for(path in paths) {
      read_path_sim <- function(x, course, path) {
        batch <- round(x/250)
        readRDS(paste0(sim_dir,'/',course,'/effects/',path,'/',batch,'/sim',x,'.RDS'))
      }
      all_path_sims <- rbindlist(lapply(1:total_sims,read_path_sim,course,path))
      saveRDS(all_path_sims, paste0(sim_dir,'/', course, '_', path, '_', total_sims, '.RDS'))
    }
  }
}

get_course_means <- function(course, total_sims, tv_vars, sim_dir) {
  this_course <- readRDS(paste0(sim_dir,'/', course, '_', total_sims, '.RDS'))
  this_course[, sim := NULL]
  lowers <- this_course[, lapply(.SD, quantile, 0.025, na.rm=T), by='name', .SDcols=names(this_course)[names(this_course)!='name']]
  lowers <- melt(lowers, id.vars='name', value.name='lower')
  uppers <- this_course[, lapply(.SD, quantile, 0.975, na.rm=T), by='name', .SDcols=names(this_course)[names(this_course)!='name']]
  uppers <- melt(uppers, id.vars='name', value.name='upper')
  means <- this_course[, lapply(.SD, mean, na.rm=T), by='name', .SDcols=names(this_course)[names(this_course)!='name']]
  means <- melt(means, id.vars='name', value.name='mean')
  all_courses <- Reduce(merge, list(means,lowers,uppers))
  setnames(all_courses, 'name', 'course')
  return(all_courses)
}

# intervention_course='treatment_course'
# compare_course='control_course'
# outcome_var='cds_bmi_4'
# total_sims=total_sim_count
# sim_dir=sim_dir
# denom_treatment=NULL
# denom_compare=NULL
get_course_effect <- function(intervention_course, compare_course, outcome_var, total_sims, sim_dir,
                              denom_treatment=NULL, denom_compare=NULL) {  
  compare_course <- readRDS(paste0(sim_dir, '/', compare_course, '_', total_sims, '.RDS'))
  setnames(compare_course, outcome_var, 'compare_outcome')
  compare_course <- compare_course[, c('sim','compare_outcome')]
  ## Merge on intervention course and calculate ATE 
  int_course <- readRDS(paste0(sim_dir, '/', intervention_course, '_', total_sims, '.RDS'))
  setnames(int_course, outcome_var, 'int_outcome')
  int_course <- int_course[, c('sim','int_outcome')]
  effect_table <- merge(compare_course, int_course, by=c('sim'))
  effect_table[, ATE := int_outcome - compare_outcome]
  if(!is.null(denom_treatment)) {
    denom_treatment <- readRDS(paste0(sim_dir, '/', denom_treatment, '_', total_sims, '.RDS'))
    setnames(denom_treatment, outcome_var, 'denom_treatment_outcome')
    denom_treatment <- denom_treatment[, c('sim','denom_treatment_outcome')]
    denom_compare <- readRDS(paste0(sim_dir, '/', denom_compare, '_', total_sims, '.RDS'))
    setnames(denom_compare, outcome_var, 'denom_compare_outcome')
    denom_compare <- denom_compare[, c('sim','denom_compare_outcome')]
    denom <- merge(denom_compare, denom_treatment, by='sim')
    denom[, denom_diff := denom_treatment - denom_compare]
    effect_table <- merge(effect_table, denom[, c('sim','denom_diff')], by='sim')
    effect_table[, ATE_percent := (int_outcome - compare_outcome) / denom_diff]
  }
  ## ATE p-values
  if(effect_table[, mean(ATE)]>=0) effect_table[, ATE_p := ifelse(ATE<0, 1, 0)]
  if(effect_table[, mean(ATE)]<0) effect_table[, ATE_p := ifelse(ATE>0, 1, 0)]
  ## Merge on any NDE/IDE courses
  paths <- list.dirs(paste0(sim_dir,'/',intervention_course,'/effects/'), recursive = F, full.names = F)
  for(path in paths) {
    path_course <- readRDS(paste0(sim_dir,'/', intervention_course, '_', path, '_', total_sims, '.RDS'))
    setnames(path_course, outcome_var, paste0('NIE_',path))
    path_course <- path_course[, c('sim',paste0('NIE_',path)), with=F]
    effect_table <- merge(effect_table, path_course, by=c('sim'))
    ## Calculate all effects (differencing effect course with compare course)
    effect_table[, (paste0('NIE_',path)) := get(paste0('NIE_',path)) - compare_outcome]
    ## Effect difference as percent of ATE
    effect_table[, (paste0('NIE_',path,'_pctATE')) := get(paste0('NIE_',path)) / ATE]
    ## Effect p-value
    if(effect_table[, mean(get(paste0('NIE_',path)))]>=0) effect_table[, (paste0('NIE_',path,'_p')) := ifelse(get(paste0('NIE_',path))<0, 1, 0)]
    if(effect_table[, mean(get(paste0('NIE_',path)))]<0) effect_table[, (paste0('NIE_',path,'_p')) := ifelse(get(paste0('NIE_',path))>0, 1, 0)]
  }
  ## Summarize: 
  ##    Mean: ATE, ATE p-value
  ##      Optional (if denom): ATE percent
  ##      Optional (if paths): NIEs, NIEs percent, NIEs p-value
  ##    SE: ATE
  ##      Optional (if paths): NIEs
  mean_vars <- c('ATE','ATE_p')
  se_vars <- c('ATE')
  if(!is.null(denom_treatment)) mean_vars <- c(mean_vars,'ATE_percent')
  if(length(paths)!=0) {
    for(path in paths) {
      mean_vars <- c(mean_vars,paste0('NIE_',path),paste0('NIE_',path,'_p'),paste0('NIE_',path,'_pctATE'))
      se_vars <- c(se_vars,paste0('NIE_',path))
    }
  }
  effect_table[, intervention := intervention_course]
  effect_table[, outcome := outcome_var]
  setcolorder(effect_table, c('intervention','outcome'))
  se_table <- effect_table[, (paste0(se_vars,'_se')) := lapply(.SD, sd), .SDcols=se_vars, by=c('intervention','outcome')]
  se_table <- unique(se_table[, c('intervention','outcome',paste0(se_vars,'_se')), with=F])
  mean_table <- effect_table[, (mean_vars) := lapply(.SD, mean), .SDcols=mean_vars, by=c('intervention','outcome')]
  mean_table <- unique(mean_table[, c('intervention','outcome',mean_vars), with=F])
  full_table <- merge(mean_table,se_table,by=c('intervention','outcome'))
  message('ATE: ', full_table[, round(ATE,2)])
  message('Sum NIEs: ', full_table[, round(rowSums(.SD),2), .SDcols=paste0('NIE_',paths)])
  for(path in paths) message('   ',path,': ',full_table[, round(get(paste0('NIE_',path)),2)])
  return(full_table) 
}

