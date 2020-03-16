library(data.table)
library(ggplot2)
library(lme4)
library(lmerTest)
library(mice)
library(miceadds)
library(mitools)

repo <- 'C:/Users/ngraetz/Documents/repos/psid/'
source(paste0(repo,'code/relative_functions.R'))
in_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/'
out_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/'

## Load clean, merged data with all variables for analyses.
old <- fread(paste0(in_dir,'merged_MAIN_CDS.csv'))
old <- old[head_race %in% c('white','black'),]
old[, head_race := factor(head_race, levels = c('white','black'))]
## Drop no child weights right away (this gets rids of censored years, we don't want to impute these)
old <- old[cds_child_weight!=0 & !is.na(cds_child_weight),]

all2 <- fread(paste0(in_dir,'merged_MAIN_CDS5.csv'))
all2 <- all2[head_race %in% c('white','black'),]
all2[, head_race := factor(head_race, levels = c('white','black'))]
## Drop no child weights right away (this gets rids of censored years, we don't want to impute these)
##########################################
## FEB 6 EDITS TO NOT DROP ZERO WEIGHTS
# all2 <- all2[cds_child_weight!=0 & !is.na(cds_child_weight),]
## TRY TO KEEP ANY IDS THAT HAVE NON-ZERO WEIGHTS IN FIRST CDS YEAR 1999
all2[, keep := ifelse(cds_year==1997 & cds_child_weight!=0 & !is.na(cds_child_weight),1,0),]
all2[, keep := max(keep,na.rm=T), by='id']
all2 <- all2[keep==1 & cds_year>=1997, ]
test <- merge(old[,c('id','age','fam_wealth_weq')], all2[,c('id','age','fam_wealth_weq','l.fam_wealth_weq')], by=c('id','age'))
##########################################
all <- copy(all2)
all <- all[!is.na(fam_wealth_weq),] ## 1 observation

## Figure out how to normalize wealth

## Make total debts, assets
all[, l.fam_assets := l.fam_wealth_weq + l.fam_debt]

## Use a top and lower bound and then standardize?
length(all[fam_wealth_weq>=-150000 & fam_wealth_weq<=500000, fam_wealth_weq]) / dim(all)[1]
all[fam_wealth_weq < -150000, fam_wealth_weq := -150000]
all[fam_wealth_weq > 500000, fam_wealth_weq := 500000]
all[fam_wealth_weq>=-150000 & fam_wealth_weq<=500000, fam_wealth_weq_sd := scale(fam_wealth_weq)]
full_dist <- density(all[, fam_wealth_weq])
full_dist.dt <- data.table(fam_wealth_weq=full_dist$x, fam_wealth_weq_density=full_dist$y)
all_wealth_mean <- mean(all[, fam_wealth_weq])
all_wealth_sd <- sd(all[, fam_wealth_weq])

length(all[fam_income<=200000, fam_income]) / dim(all)[1]
all[fam_income > 200000, fam_income := 200000]
all[, fam_income_sd := scale(fam_income)]
all_income_mean <- mean(all[, fam_income])
all_income_sd <- sd(all[, fam_income])

length(all[fam_wealth_noeq>=-200000 & fam_wealth_noeq<=250000, fam_wealth_noeq]) / dim(all)[1]
all[fam_wealth_noeq < -200000, fam_wealth_noeq := -200000]
all[fam_wealth_noeq > 250000, fam_wealth_noeq := 250000]
all[, fam_wealth_noeq_sd := scale(fam_wealth_noeq)]
# all_wealth_mean <- mean(all[, fam_wealth_noeq])
# all_wealth_sd <- sd(all[, fam_wealth_noeq])

## Scale lagged
all[l.fam_wealth_weq < -150000, l.fam_wealth_weq := -150000]
all[l.fam_wealth_weq > 500000, l.fam_wealth_weq := 500000]
all[l.fam_wealth_weq>=-150000 & l.fam_wealth_weq<=500000, l.fam_wealth_weq_sd := scale(l.fam_wealth_weq)]
all[l.fam_income > 200000, l.fam_income := 200000]
all[, l.fam_income_sd := scale(l.fam_income)]
all[, head_race_caps := ifelse(head_race=='black','Black','White')]

# pdf('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/wealth_distribution.pdf',width=8,height=5)
png('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/wealth_distribution.png',height=8,width=12, units='in',res=600)
ggplot() + 
  geom_density(data=all,
                 aes(x=fam_wealth_weq,
                     fill=head_race_caps),
                 alpha=0.5,size=0.5,color='black') + 
  # geom_density(data=all,
  #              aes(x=fam_wealth_weq),
  #              color='black',
  #              size=1)
  geom_line(data=full_dist.dt,
            aes(x=fam_wealth_weq,
                y=fam_wealth_weq_density),
            size=1, color='black') + 
  scale_fill_grey(name='Race') + 
  # scale_color_grey() +
  # guides(color=FALSE) + 
  lims(x=c(-150000,500000)) + 
  labs(x='Household wealth',y='Density') + 
  theme_minimal() + 
  theme(axis.text = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        axis.title = element_text(size=20))
dev.off()

## Standardized continuous variables
# scale_vars <- c('fam_wealth_weq')
# all[, (scale_vars) := lapply(.SD, scale), .SDcols=scale_vars]

## Other variable transforms
all[!is.na(cds_srh), cds_srhfairpoor := ifelse(cds_srh %in% c('fair','poor'), 1, 0)]
all[!is.na(cds_pcg_srh), cds_pcg_srhfairpoor := ifelse(cds_pcg_srh %in% c('fair','poor'), 1, 0)]
all[edu_highest_parent_1=='0', edu_highest_parent_1 := NA]
all[edu_highest_parent_1=='higher', edu_highest_parent_1 := 'college']
all[healthcare_exp<0, healthcare_exp := 0]
all[housing_exp<0, housing_exp := 0]
exp_vars <- c('education_exp','childcare_exp','healthcare_exp','housing_exp')
all[, (exp_vars) := lapply(.SD, function(x) log(x+1)), .SDcols=exp_vars]
all[, cds_dr_visits := as.double(cds_dr_visits)]
all[, cds_hospital_visits := as.double(cds_hospital_visits)]
all[!is.na(cds_dr_visits), cds_dr_visits := log((cds_dr_visits+0.1))]
all[!is.na(cds_hospital_visits), cds_hospital_visits := log((cds_hospital_visits+0.1))]
all[, cds_economic_strain := as.double(cds_economic_strain)]
all[!is.na(cds_economic_strain), cds_economic_strain := log((cds_economic_strain+0.1))]
all[, married := ifelse(marital_status=='married', 1, 0)]
## Extra outcomes?
all[cds_reading %in% c(0,999), cds_reading := NA]
all[cds_math %in% c(0,999), cds_math := NA]
all[, chronic := ifelse(cds_allergies==1 | cds_anemia==1 | cds_asthma==1 | cds_ear==1, 1, 0)]
# all[, mental := ifelse(cds_depression!=1 | cds_emotional==1, 1, 0)]
all[, chronic_count := cds_allergies + cds_anemia + cds_asthma + cds_ear]
# all[, mental := factor(mental, levels=c(0,1))]
# all[, cds_depression := factor(cds_depression, levels=c(0,1))]
all[, chronic := factor(chronic, levels=c(0,1))]
all[, below1k := ifelse(fam_wealth_weq<1000,1,0)] ## 42% for Black compared to 17% for white
all[!is.na(edu_highest_parent_1), college := ifelse(edu_highest_parent_1=='college',1,0)]
## NEW MECHANISMS
all[, cds_exercise_minutes := (30*cds_exercise_home) + cds_exercise_school]
all[, cds_fruitveg_days := cds_fruit_days + cds_veggie_days]
all[age<10, cds_exercise_minutes := NA]
all[age<10, cds_fruitveg_days := NA]
all[age<10, cds_sleephours := NA]
all[cds_school_days_missed>60, cds_school_days_missed := NA]
all[, cds_school_days_missed := log(cds_school_days_missed+0.01)]
all[, cds_missed_activity := (cds_missed_school+cds_missed_social)]

## MULTIPLE IMPUTATION
weights <- all[, c('id','age','cds_child_weight','year','cds_year','below1k',
                   'education_exp','childcare_exp','healthcare_exp','housing_exp',
                   'cds_economic_strain')]

# id_vars <- c('id','head_race')
# outcome_vars <- c('cds_bmi')
#                   # 'cds_reading','cds_math','chronic','chronic_count','cds_depression') 
# mech_vars <- c('cds_parenting_strain')
# control_vars <- c('male','age','birth_order','married','edu_highest_parent_1',
#                   'fam_income_sd','fam_wealth_weq_sd')
# all_to_imp <- all[, c(id_vars,outcome_vars,mech_vars,control_vars), with=F]

id_vars <- c('id','head_race','male','age','birth_order','married','fam_income_sd','fam_wealth_weq_sd','fam_debt','own_rent')
all_to_imp <- all[, c(id_vars,'cds_bmi','cds_parenting_strain','college',
                      'cds_missed_activity','cds_school_days_missed','cds_exercise_minutes','cds_fruitveg_days','cds_sleephours'), with=F]
missing <- sapply(all_to_imp, function(x) sum(is.na(x))/dim(all_to_imp)[1])
missing.dt <- data.table(variable=names(missing), missing_proportion=missing)
write.csv(missing.dt, paste0(out_dir,'/missing_props.csv'), row.names = F)
meth <- c(rep('',length(id_vars)),'norm','norm','logreg','norm','norm','norm','norm','norm')
# names(meth) <- c(id_vars,outcome_vars,mech_vars,control_vars)
names(meth) <- names(all_to_imp)
meth[missing.dt[missing_proportion==0, variable]] <- ''
# all_to_imp[, cds_srhfairpoor := factor(cds_srhfairpoor, levels=c(0,1))]
# all_to_imp[, cds_pcg_srhfairpoor := factor(cds_pcg_srhfairpoor, levels=c(0,1))]
# all_to_imp[, edu_highest_parent_1 := factor(edu_highest_parent_1, levels=unique(edu_highest_parent_1))]
# all_to_imp[, male := factor(male, levels=c(0,1))]
# all_to_imp[, married := factor(married, levels=c(0,1))]
all_to_imp[, college := factor(college, levels=c(0,1))]
mi_list <- mice(all_to_imp, method=meth, m=30, maxit=30)
df_imp <- as.data.table(mice::complete(mi_list, 'long'))
imp_list <- mitools::imputationList(lapply(1:30, function(n) mice::complete(mi_list, action=n)))
saveRDS(list(imp_list, mi_list), paste0(in_dir,'/imputed_input_data_march16.RDS'))
all[, college := as.numeric(as.character(college))]
df_imp[, college := as.numeric(as.character(college))]

all[, cds_pcg_srhfairpoor := as.numeric(cds_pcg_srhfairpoor)]
all[cds_pcg_srhfairpoor==1, cds_pcg_srhfairpoor := 0]
all[cds_pcg_srhfairpoor==2, cds_pcg_srhfairpoor := 1]
all[, cds_srhfairpoor := as.numeric(cds_srhfairpoor)]
all[cds_srhfairpoor==1, cds_srhfairpoor := 0]
all[cds_srhfairpoor==2, cds_srhfairpoor := 1]
df_imp[, cds_pcg_srhfairpoor := as.numeric(cds_pcg_srhfairpoor)]
df_imp[cds_pcg_srhfairpoor==1, cds_pcg_srhfairpoor := 0]
df_imp[cds_pcg_srhfairpoor==2, cds_pcg_srhfairpoor := 1]
df_imp[, cds_srhfairpoor := as.numeric(cds_srhfairpoor)]
df_imp[cds_srhfairpoor==1, cds_srhfairpoor := 0]
df_imp[cds_srhfairpoor==2, cds_srhfairpoor := 1]

## Descriptive Figure 2
fix_weights <- function(d) {
  d <- as.data.table(d)
  d[, f_head_race := factor(head_race, levels=c('white','black'))]
  d <- d[!is.na(cds_child_weight) & cds_child_weight!=0,]
  d[age<6, agegrp := 3]
  d[age>=6 & age <10, agegrp := 8]
  d[age>=10 & age <14, agegrp := 12]
  d[age>=14, agegrp := 16]
  d[, male := as.numeric(as.character(male))]
  d[, married := as.numeric(as.character(married))]
  d[, cds_srhfairpoor := as.numeric(as.character(cds_srhfairpoor))]
  d[, cds_pcg_srhfairpoor := as.numeric(as.character(cds_pcg_srhfairpoor))]
  d[, fam_income := (fam_income_sd*all_income_sd) + all_income_mean]
  d[, fam_wealth := (fam_wealth_weq_sd*all_wealth_sd) + all_wealth_mean]
  for(v in unique(d[, edu_highest_parent_1])) d[, (paste0('edu_',gsub(' ','_',v))) := ifelse(edu_highest_parent_1==v,1,0)]
  d[, cds_bmi_sd := scale(cds_bmi), by='agegrp']
  return(d)
}
imp_master <- readRDS(paste0(in_dir,'/imputed_input_data4.RDS'))
imp_list <- imp_master[[1]]
mi_list <- imp_master[[2]]
imp_list[[1]] <- lapply(imp_list[[1]], fix_weights)
des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list)
get_v_trend <- function(v) {
  t <- MIcombine(with(des, svyby(formula=as.formula(paste0('~',v)), design = des, by=~f_head_race+agegrp, FUN=svymean, ci=T)))
  d <- as.data.table(summary(t))[, c('results','(lower','upper)')]
  d[, cat := rownames(summary(t))]
  setnames(d, c('mean','lower','upper','cat'))
  d[, race := tstrsplit(cat,'[.]',keep=1)]
  d[, agegrp := tstrsplit(cat,'[.]',keep=2)]
  d[, agegrp := as.numeric(agegrp)]
  d[, outcome := v]
  return(d)
}
trends <- rbindlist(lapply(c('cds_bmi','cds_bmi_sd'), get_v_trend))
trends[outcome=='cds_bmi', outcome := 'Raw BMI']
trends[outcome=='cds_bmi_sd', outcome := 'BMI age-specific z-score']
trends[, race := ifelse(race=='black','Black','White')]
png(paste0('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/health_trends',Sys.Date(),'.png'),height=12,width=8, units='in',res=600)
ggplot(data=trends) +
  # geom_errorbar(aes(x=agegrp,
  #                   ymax=upper,ymin=lower,
  #                   color=race),width=0.7,size=1) +
  geom_point(aes(x=agegrp,
                 y=mean,
                 fill=race),
             shape=21,size=6) + 
  scale_x_continuous(breaks=c(3,8,12,16),labels=c('0-6','6-10','10-14','14-18')) + 
  scale_fill_grey(name='Race') + 
  labs(y='Mean',x='Age group',fill='Race') + 
  guides(color=FALSE) + 
  facet_wrap(~outcome,scales = 'free',ncol=1) + 
  theme_bw() + 
  theme(axis.text = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        axis.title = element_text(size=20),
        strip.text = element_text(size=20))
dev.off()

## Make descriptive Table 1 (columns = all, white, black, significant difference)
des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list)
get_table1 <- function(v) {

  if(v %in% c('fam_income','fam_wealth')) {
  single_des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list[[1]][[1]])
  race_medians <- svyby(as.formula(paste0('~',v)), ~f_head_race, single_des, svyquantile, quantiles=0.5,ci=TRUE,se=T)
  race <- as.data.table(race_medians)
  race[, race := rownames(race_medians)]
  setnames(race, v, 'results')
  race <- race[, c('results','se','race')]
  full_mean <- svyquantile(as.formula(paste0('~',v)), single_des, quantiles=0.5,ci=TRUE,se=T)
  full_mean <- data.table(race='full',results=as.numeric(full_mean$quantiles),se=(as.numeric(full_mean$quantiles)-as.numeric(full_mean$CIs)[1])/1.96)
  
  all <- rbind(race, full_mean)
  all[, variable := v]
  all <- dcast(all, variable ~ race, value.var = c('results','se'))
  p <- t.test(rnorm(1000,all$results_white,all$se_white),
         rnorm(1000,all$results_black,all$se_black))$p.value
  all[, p := p]
  }
  if(!(v %in% c('fam_income','fam_wealth'))) {
  race_means <- MIcombine(with(des, svyby(as.formula(paste0('~',v)), by=~f_head_race, svymean, na.rm=T)))
  race <- as.data.table(summary(race_means))
  race[, race := rownames(summary(race_means))]
  race <- race[, c('results','se','race')]
  full_mean <- as.data.table(summary(MIcombine(with(des, svymean(as.formula(paste0('~',v)), na.rm=T)))))
  full_mean[, race := 'full']
  full_mean <- full_mean[, c('results','se','race')]
  all <- rbind(race, full_mean)
  all[, variable := v]
  all <- dcast(all, variable ~ race, value.var = c('results','se'))
  p <- with(des, svyglm(formula=as.formula(paste0(v,'~f_head_race'))))
  beta_list <- lapply(p, function(x){coef(x)})
  vcov_list <- lapply(p, function(x){vcov(x,complete=T)})
  p <- miceadds::pool_mi(qhat=beta_list, u=vcov_list)
  all[, p := summary(p)$p[2]]
  }
  
  return(all)
}
table_vars <- c('age','male','birth_order','married',"edu_less_than_hs","edu_hs","edu_some_college","edu_college",'fam_income','fam_wealth','cds_parenting_strain','cds_economic_strain','cds_bmi','cds_bmi_sd')
table1 <- rbindlist(lapply(table_vars, get_table1))
setcolorder(table1, c('variable','results_full','se_full','results_white','se_white','results_black','se_black','p'))
table1[, pvalue := '']
table1[p<=0.05, pvalue := paste0(pvalue,'*')]
table1[p<=0.01, pvalue := paste0(pvalue,'*')]
table1[p<=0.001, pvalue := paste0(pvalue,'*')]
table1[p<=0.10 & is.na(p), pvalue := paste0(pvalue,'.')]
table1[, p := NULL]
for(v in c('se_full','se_white','se_black','results_full','results_white','results_black')) table1[, (v) := as.character(round(get(v),2))]
for(v in c('se_full','se_white','se_black')) table1[, (v) := paste0('(',get(v),')')]
clean_names <- data.table(variable=c("(Intercept)","as.factor(male)1","male","birth_order","as.factor(head_race)black",
                                     "age","as.factor(married)1","married",
                                     "as.factor(edu_highest_parent_1)less than hs",
                                     "as.factor(edu_highest_parent_1)hs",
                                     "as.factor(edu_highest_parent_1)some college",
                                     "as.factor(edu_highest_parent_1)college",
                                     "edu_less_than_hs","edu_hs","edu_some_college","edu_college",
                                     "fam_income","fam_wealth",
                                     "fam_income_sd","l.fam_income_sd","fam_wealth_weq_sd","l.fam_wealth_weq_sd",
                                     "fam_wealth_weq_sd:age","age:fam_wealth_weq_sd","fam_wealth_noeq_sd",
                                     "education_exp","childcare_exp","healthcare_exp","housing_exp",
                                     "b_education_exp","b_childcare_exp","b_healthcare_exp","b_housing_exp",
                                     "cds_dr_visits","cds_hospital_visits",
                                     "cds_parenting_strain","cds_economic_strain",
                                     "cds_bmi","cds_srhfairpoor","cds_bmi_sd",
                                     "mar_rsq","cond_rsq","bic","aic","N","people"),
                          name=c("Intercept","Male","Male","Birth order","Black",
                                 "Age","Married","Married",
                                 "Less than HS",
                                 "High school",
                                 "Some college",
                                 'College',
                                 "Less than HS",
                                 "High school",
                                 "Some college",
                                 'College',
                                 "Family income","Family wealth",
                                 "Family income","Family income","Family wealth w eq","Family wealth w eq",
                                 'Wealth*Age','Wealth*Age',
                                 "Family wealth no eq",
                                 "Edu spending","Child spending","Health spending","Housing spending",
                                 "Edu spending","Child spending","Health spending","Housing spending",
                                 "Dr visits","Hospital visits",
                                 "Parenting strain","Economic strain",
                                 'BMI','Self-rated health','BMI age-specific z-score',
                                 'Marginal R2','Conditional R2','BIC','AIC','Person-years','Individuals'))
clean_names[, cov_sort := 1:.N]
table1 <- merge(table1, clean_names, by='variable')
table1 <- table1[order(cov_sort)]
table1[, cov_sort := NULL]
table1[, variable := NULL]
setcolorder(table1, 'name')
setnames(table1, c('Variable','Fullmean','FullSE','Whitemean','WhiteSE','Blackmean','BlackSE','p'))
setkey(table1, NULL)
ft <- flextable(table1, theme_fun = theme_booktabs) %>%
  add_footer_lines(c('* p < 0.05; ** p < 0.01; *** p < 0.001')) %>%
  fontsize(size = 12, part='all') %>%
  font(fontname = 'Times New Roman', part='all') %>%
  autofit()

setwd('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/')
doc <- read_docx() %>%
  body_add_flextable(value = ft) %>%
  body_end_section_landscape() %>% # a landscape section is ending here
  print(target = "table1.docx")

## OUTCOME/MECHANISM TRAJECTORIES BY WEALTH
## Make age trajectories from 0 to 18
traj_data <- copy(all)
traj_data[fam_wealth_weq_sd >= 0.5, wealth_cat := '>= 0.5 SD']
traj_data[fam_wealth_weq_sd <= -0.5, wealth_cat := '<= -0.5 SD']
traj_data[is.na(wealth_cat), wealth_cat := '-0.5 to 0.5 SD']
traj_data[, wealth_cat := factor(wealth_cat, levels=c('<= -0.5 SD','-0.5 to 0.5 SD','>= 0.5 SD'))]
traj_data[, n := 1]
traj_data[age<6, agegrp := 3]
traj_data[age>=6 & age <10, agegrp := 8]
traj_data[age>=10 & age <14, agegrp := 12]
traj_data[age>=14, agegrp := 16]
traj_data[, cds_pcg_srhfairpoor := as.numeric(cds_pcg_srhfairpoor)]
traj_data_agg <- traj_data[, lapply(.SD, mean, na.rm=T), .SDcols=c(outcome_vars,mech_vars), by=c('agegrp','wealth_cat')]
traj_data <- copy(df_imp)
traj_data[fam_wealth_weq_sd >= 0.5, wealth_cat := '>= 0.5 SD']
traj_data[fam_wealth_weq_sd <= -0.5, wealth_cat := '<= -0.5 SD']
traj_data[is.na(wealth_cat), wealth_cat := '-0.5 to 0.5 SD']
traj_data[, wealth_cat := factor(wealth_cat, levels=c('<= -0.5 SD','-0.5 to 0.5 SD','>= 0.5 SD'))]
traj_data[, n := 1]
traj_data[age<6, agegrp := 3]
traj_data[age>=6 & age <10, agegrp := 8]
traj_data[age>=10 & age <14, agegrp := 12]
traj_data[age>=14, agegrp := 16]
traj_data[, cds_pcg_srhfairpoor := as.numeric(cds_pcg_srhfairpoor)]
traj_data_agg_imp <- traj_data[, lapply(.SD, mean, na.rm=T), .SDcols=c(outcome_vars,mech_vars), by=c('agegrp','wealth_cat')]
traj_data_agg[, type := 'Raw data']
traj_data_agg_imp[, type := 'Imputed data']
traj_data_agg <- rbind(traj_data_agg, traj_data_agg_imp)

library(gridExtra)
pdf('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/outcome_and_mechanism_trajectories.pdf',width=15,height=8)
for(v in c(outcome_vars,mech_vars)) {
# traj_counts <- traj_data[!is.na(get(v)), list(personyears=sum(n)), by=c('wealth_cat','head_race')]
# traj_counts <- traj_counts[order(head_race,wealth_cat)]
gg_outcome <- ggplot() + 
  geom_line(data=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), ],
              aes(x=agegrp,
                  y=get(v),
                  color=wealth_cat,
                  linetype=type), size=1) + 
  scale_color_manual(values=c('red','black','blue')) + 
  # annotation_custom(tableGrob(traj_counts, rows=NULL), 
  #                   xmin=16, xmax=6, ymin=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), quantile(get(v),.15)],
  #                   ymax=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), quantile(get(v),.15)]) + 
  labs(x='Age',y=v) + 
  theme_minimal() + 
  theme(axis.title = element_text(size=20)) 
print(gg_outcome)
gg_outcome <- ggplot() + 
  geom_density(data=traj_data,
            aes(x=get(v),
                fill=wealth_cat),alpha=0.3) + 
  geom_density(data=traj_data,
               aes(x=get(v)),fill=NA,size=1) + 
  scale_fill_manual(values=c('red','black','blue')) + 
  # annotation_custom(tableGrob(traj_counts, rows=NULL), 
  #                   xmin=16, xmax=6, ymin=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), quantile(get(v),.15)],
  #                   ymax=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), quantile(get(v),.15)]) + 
  labs(x='Age',y=v) + 
  theme_minimal() + 
  theme(axis.title = element_text(size=20)) 
print(gg_outcome)
# traj_counts <- tableGrob(traj_counts, rows=NULL)
# grid.arrange(gg_outcome, traj_counts, 
#              nrow = 1, ncol=2, widths = c(2, 0.5))
}
dev.off()

## Fit basic growth curve model without controlling for potential mediators.
## Outcomes: BMI, depression, SRH, count of chronic (negative binomial), one 0/1 of health limitations measures (any reported)
##    -- cds_bmi, cds_depression, cds_srhfairpoor, chronic_count, health_limit
# all[, cds_parenting_strain := scale(cds_parenting_strain)]
# all[, cds_behind_bills := scale(cds_behind_bills)]



#######################################################
## FIT MODELS
#######################################################
lag_var <- function(d,lag_vars) {
  d <- as.data.table(d)
  d <- d[order(id,age)]
  ## Lag any necessary variables
  # for(v in lag_vars) d[, (paste0('l.',v)) := data.table::shift(get(v)), by=c('id')]
  ## Just merge on lagged wealth from "all" above - don't need to re-impute anything (there are 66 observations missing wealth)
  d <- merge(d, all[,c('id','age','year','cds_year','cds_child_weight',
                       'below1k','education_exp','childcare_exp','healthcare_exp','housing_exp',
                       'cds_economic_strain')], by=c('id','age'), all.x=T)
  ## Make age-specific SD BMI
  d[age<6, agegrp := 3]
  d[age>=6 & age <10, agegrp := 8]
  d[age>=10 & age <14, agegrp := 12]
  d[age>=14, agegrp := 16]
  d[, cds_bmi_sd := scale(cds_bmi), by='agegrp']
  ## Fix imputed binomial variables
  # d[, cds_srhfairpoor := as.numeric(as.character(cds_srhfairpoor))]
  # d[, cds_pcg_srhfairpoor := as.numeric(as.character(cds_pcg_srhfairpoor))]
  d[, college := as.numeric(as.character(college))]
  ## Create spending burden variables
  d[, fam_wealth := (fam_wealth_weq_sd*all_wealth_sd) + all_wealth_mean]
  d[, fam_income := (fam_income_sd*all_income_sd) + all_income_mean]
  # for(s in c("education_exp","childcare_exp","healthcare_exp","housing_exp")) {
  #   d[, (paste0('b_',s)) := exp(get(s))/fam_income]
  #   d[get(paste0('b_',s))>1 | is.infinite(get(paste0('b_',s))), (paste0('b_',s)) := 1]
  #   d[get(paste0('b_',s))<0, (paste0('b_',s)) := 0]
  #   d[, (paste0('b_',s)) := get(paste0('b_',s))*100]
  #   # d[, (paste0('b_',s)) := scale(log(get(paste0('b_',s))))]
  # }
  ## Add new variables
  # d[, college := ifelse(edu_highest_parent_1=='college',1,0)]
  # d[, below1k := ifelse(fam_wealth<1000,1,0)]
  ## Merge back on weights
  # d <- merge(d, weights, by=c('id','age'))
  
  ## Make some variables NA is age where wasn't asked (rather than 0, as it is right now, in contrast to true 0s)
  d[age<10, exercise_minutes := NA]
  d[age<10, sleephours := NA]
  d[age<10, fruitveg_days := NA]
  d[, cds_home_own := ifelse(own_rent=='owns',1,0)]
  
  return(d)
}
# imp_master <- readRDS(paste0(in_dir,'/imputed_input_data5.RDS'))
imp_master <- readRDS(paste0(in_dir,'/imputed_input_data_march16.RDS'))
imp_list <- imp_master[[1]]
mi_list <- imp_master[[2]]
imp_list[[1]] <- lapply(imp_list[[1]], lag_var, lag_vars=c('fam_wealth_weq_sd','fam_income_sd'))
# saveRDS(list(imp_list,mi_list), paste0(in_dir,'/imputed_input_data_for_secure_v4.RDS'))
saveRDS(list(imp_list,mi_list), paste0(in_dir,'/imputed_input_data_for_secure_MARCH16.RDS'))

basic_controls <- 'as.factor(male) + as.factor(head_race) + age + birth_order + as.factor(married) + as.factor(edu_highest_parent_1) + fam_income_sd'
iv_vars <- c('fam_wealth_weq_sd')
use_weights <- TRUE
res <- TRUE
file_tag <- 'inc_tas_res'
# missing <- sapply(imp_list[[1]][[1]], function(x) sum(is.na(x)))
outcome_vars <- c("cds_bmi","cds_bmi_sd","cds_pcg_srhfairpoor","cds_srhfairpoor")
# outcome_vars <- 'cds_parenting_strain'
# outcome_vars <- c("cds_pcg_srhfairpoor","cds_srhfairpoor")
## Lumley on weights and REs: https://stats.stackexchange.com/questions/89204/fitting-multilevel-models-to-complex-survey-data-in-r
mech_vars <- c('b_education_exp','b_childcare_exp','b_healthcare_exp','b_housing_exp',
               'cds_dr_visits','cds_hospital_visits','cds_parenting_strain','cds_economic_strain')

for(outcome in outcome_vars) {
for(iv in iv_vars) {
message(outcome)

if(outcome %in% c('cds_bmi','cds_bmi_sd','cds_parenting_strain','cds_depression','chronic_count')) {
  run_model <- function(mech) {
    message(mech)
    if(mech=='controls') f1 <- paste0(outcome, ' ~  ', basic_controls)
    if(mech!='controls') f1 <- paste0(outcome, ' ~  ', basic_controls, ' + ', iv)
    if(!(mech %in% c('controls','wealth'))) f1 <- paste0(f1,' + ',mech)
    if(!use_weights) m1 <- pool(with(mi_list, glm(as.formula(f1))))
    # m1 <- pool(with(mi_list, lme4::lmer(as.formula(paste0(f1,' + (1|id)')),weights = cds_child_weight)))
    if(use_weights) {
      fix_weights <- function(d) {
        d <- as.data.table(d)
        # d[is.na(cds_child_weight), cds_child_weight := 0]
        d <- d[!is.na(cds_child_weight) & cds_child_weight!=0,]
        return(d)
      }
      imp_list[[1]] <- lapply(imp_list[[1]], fix_weights)
      if(!res) {
        des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list)
        message(f1)
        m1 <- with(des, svyglm(formula=as.formula(f1),
                               family='gaussian',
                               design=des))
        beta_list <- lapply(m1, function(x){coef(x)})
        vcov_list <- lapply(m1, function(x){vcov(x,complete=T)})
        m1 <- miceadds::pool_mi(qhat=beta_list, u=vcov_list)
        # des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list[[1]][[1]])
        # m1 <- svyglm(formula=as.formula(f1),family='gaussian',design=des)
        # m2 <- lmerTest::lmer(as.formula(paste0(f1,' + (1|id)')),data=imp_list[[1]][[1]],weights=cds_child_weight)
        # m2 <- pool(with(imp_list, lme4::lmer(as.formula(paste0(f1,' + (1|id)')),weights=cds_child_weight)))
      }
      if(res) {
        # m1 <- pool(with(mi_list, lme4::lmer(as.formula(paste0(f1,' + (1|id)')),weights = cds_child_weight)))
        m1 <- pool(with(imp_list, lme4::lmer(as.formula(paste0(f1,' + (1|id)')))))
      }
    }
    return(m1)
  }
  model_list <- lapply(c('controls','wealth',mech_vars), run_model)
}
if(outcome %in% c('cds_pcg_srhfairpoor','cds_srhfairpoor')) {
  run_model <- function(mech) {
    message(mech)
    if(mech=='controls') f1 <- paste0(outcome, ' ~  ', basic_controls)
    if(mech!='controls') f1 <- paste0(outcome, ' ~  ', basic_controls, ' + ', iv)
    if(!(mech %in% c('controls','wealth'))) f1 <- paste0(f1,' + ',mech)
    if(!use_weights) m1 <- pool(with(mi_list, glm(as.formula(f1), family='binomial')))
    if(use_weights) {
      fix_weights <- function(d) {
        d <- as.data.table(d)
        # d[is.na(cds_child_weight), cds_child_weight := 0]
        d <- d[!is.na(cds_child_weight) & cds_child_weight!=0,]
        return(d)
      }
      imp_list[[1]] <- lapply(imp_list[[1]], fix_weights)
      if(!res) {
        des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list)
        m1 <- with(des, svyglm(formula=as.formula(f1),
                               family='binomial',
                               design=des))
        beta_list <- lapply(m1, function(x){coef(x)})
        vcov_list <- lapply(m1, function(x){vcov(x,complete=T)})
        m1 <- miceadds::pool_mi(qhat=beta_list, u=vcov_list)
      }
      if(res) {
        # m1 <- pool(with(mi_list, lme4::glmer(as.formula(paste0(f1,' + (1|id)')),family='binomial',weights = cds_child_weight)))
        # m1 <- pool(with(mi_list, lme4::glmer(as.formula(paste0(f1,' + (1|id)')),family='binomial'))) 
        m1 <- pool(with(imp_list, lme4::glmer(as.formula(paste0(f1,' + (1|id)')),family=binomial, control=glmerControl(optimizer="bobyqa"),nAGQ = 10)))
        # m1 <- lme4::glmer(as.formula(paste0(f1,' + (1|id)')),data=imp_list[[1]][[1]],family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ = 10)
        # m2 <- with(des, svyglm(formula=as.formula(f1),
        #                        family='binomial',
        #                        design=des))
        # beta_list <- lapply(m2, function(x){coef(x)})
        # vcov_list <- lapply(m2, function(x){vcov(x,complete=T)})
        # m2 <- miceadds::pool_mi(qhat=beta_list, u=vcov_list)
      }
    }
    return(m1)
  }
  model_list <- lapply(c('controls','wealth',mech_vars), run_model)
}

library(lmerTest)
library(MuMIn)
get_coefs <- function(i, model_list) {
  x <- model_list[[i]]
  this_sum <- summary(x)
  coefs <- as.data.table(this_sum)
  coefs[, variable := rownames(summary(x))]
  # if('Pr(>|z|)' %in% names(coefs)) coefs <- coefs[, c('Estimate','Std. Error','Pr(>|z|)','variable')]
  # if('Pr(>|t|)' %in% names(coefs)) coefs <- coefs[, c('Estimate','Std. Error','Pr(>|t|)','variable')]
  if(!use_weights) coefs <- coefs[, c('estimate','std.error','p.value','variable')]
  if(use_weights & !res) coefs <- coefs[, c('results','se','p','variable')]
  if(use_weights & res) coefs <- coefs[, c('estimate','std.error','p.value','variable')]
  setnames(coefs, c('mean','se','pvalue','variable'))
  coefs[, p := '']
  coefs[pvalue<=0.05, p := paste0(p,'*')]
  coefs[pvalue<=0.01, p := paste0(p,'*')]
  coefs[pvalue<=0.001, p := paste0(p,'*')]
  coefs[pvalue<=0.10 & is.na(p), p := paste0(p,'.')]
  coefs[, pvalue := NULL]
  # rs <- r.squaredGLMM(x)
  # rs <- data.table(variable=c('mar_rsq','cond_rsq'),mean=c(rs[1],rs[2]))
  # if('AICtab' %in% names(this_sum)) {
  #   bic <- data.table(variable=c('bic'),mean=round(this_sum$AICtab['BIC']))
  #   py <- data.table(variable=c('N'),mean=this_sum$devcomp$dims['N'])
  #   people <- data.table(variable=c('people'),mean=this_sum$ngrps)
  # }
  # if(!('AICtab' %in% names(this_sum))) {
  #   bic <- data.table(variable=c('aic'),mean=round(this_sum$aic))
  #   py <- data.table(variable=c('N'),mean=dim(df_imp[.imp==1,])[1])
  #   people <- data.table(variable=c('people'),mean=length(unique(df_imp[,id])))
  # }
  # coefs <- rbindlist(list(coefs, rs, bic, py, people), fill=T)
  py <- data.table(variable=c('N'),mean=dim(df_imp[.imp==1,])[1])
  people <- data.table(variable=c('people'),mean=length(unique(df_imp[,id])))
  coefs <- rbindlist(list(coefs, py, people), fill=T)
  coefs[, model := paste0('model',i)]
  return(coefs)
}
options(scipen = 999)
coefs <- rbindlist(lapply(1:length(model_list), get_coefs, model_list))
coefs[is.na(p), p := '']
for(v in c('mean','se')) coefs[, (v) := format(round(get(v),2), nsmall=0, big.mark=",")]
coefs[se=='     NA', se := '']

coefs <- merge(coefs, clean_names, by='variable', all.x=T)
coefs <- coefs[!is.na(name), ]
coefs <- dcast(coefs, cov_sort+name~model, value.var = c('mean','se','p'))
coefs <- coefs[order(cov_sort)]
coefs[, c('cov_sort') := NULL]
for(v in names(coefs)[names(coefs)!='name']) coefs[is.na(get(v)), (v) := '']
for(v in paste0('se_model',c(1:length(model_list)))) {
  coefs[get(v)=='  NA', (v) := '']
  coefs[get(v)=='NA', (v) := '']
  coefs[get(v)!='', (v) := paste0('(',get(v),')')]
}
table_names <- expand.grid(c('mean_model','se_model','p_model'),1:length(model_list))
table_names <- paste0(table_names[,1],table_names[,2])
setcolorder(coefs, c('name',table_names))

first_header <- as.list(c('',rep(c('Coef.','SE',''),length(model_list))))
second_header <- as.list(c('',rep('Model 1',3),rep('Model 2',3),rep('Model 3',3),rep('Model 4',3)))
second_header <- c('')
for(m in 1:length(model_list)) second_header <- c(second_header, rep(paste0('Model ',m),3))
second_header <- as.list(second_header)
names(first_header) <- names(coefs)
names(second_header) <- names(coefs)

library(flextable)
library(officer)
library(dplyr)
ft <- flextable(coefs, theme_fun = theme_booktabs) %>%
  delete_part(part = "header") %>%
  add_header(values=first_header) %>%
  add_header(values=second_header) %>%
  merge_h(part = "header") %>%
  align(align = 'right', part='body') %>%
  align(j=1, align = 'left', part='body') %>%
  align(i=1, align = 'right', part='header') %>%
  align(i=2, align = 'right', part='header') %>%
  hline_top(part = 'body', border = fp_border(color='black',width=2)) %>%
  hline_top(part = 'header', border = fp_border(color='black',width=2)) %>%
  hline(j=2:dim(coefs)[2],part = 'header', border = fp_border(color='black',width=2)) %>%
  hline(j=1:dim(coefs)[2],i=dim(coefs)[1]-2,part = 'body', border = fp_border(color='black',width=2)) %>%
  padding(padding = 0.0001, part = 'all') %>%
  padding(padding.top = 0, part='all') %>%
  # width(j=2:8, width=0.4) %>%
  # width(j=1, width=2.23) %>%
  add_footer_lines(c('* p < 0.05; ** p < 0.01; *** p < 0.001')) %>%
  fontsize(size = 8, part='all') %>%
  font(fontname = 'Times New Roman', part='all') %>%
  autofit()

setwd('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/')
pgwidth=11
ft_out <- width(ft, width = dim(ft)$widths*10/(flextable_dim(ft)$widths))
doc <- read_docx() %>%
  body_add_flextable(value = ft_out) %>%
  body_end_section_landscape() %>% # a landscape section is ending here
  print(target = paste0("models_", file_tag, "_OUTCOME", outcome, "_IV", gsub('[*]','',iv), ".docx"))

}
}

## TRY MEDIATION PACKAGE
library(mediation)
outcome <- 'cds_bmi'
mech <- 'below1k'
iv <- 'head_race'
basic_controls <- 'as.factor(male) + age + birth_order + as.factor(married) + as.factor(edu_highest_parent_1) + fam_income_sd'

if(mech=='controls') f1 <- paste0(outcome, ' ~  ', basic_controls)
if(mech!='controls') f1 <- paste0(outcome, ' ~  ', basic_controls, ' + ', iv)
mech_f <- paste0(mech, ' ~  ', basic_controls, ' + ', iv,' + (1|id)')
if(!(mech %in% c('controls','wealth'))) f1 <- paste0(f1,' + ',mech,' + (1|id)')

med.fit <- glmer(mech_f, data=imp_list[[1]][[1]], weights=cds_child_weight)
out.fit <- glmer(f1, data=imp_list[[1]][[1]], weights=cds_child_weight)
med.out <- mediate(med.fit, out.fit, treat = iv, mediator = mech, sims = 100)
summary(med.out)


basic_controls <- 'as.factor(male) + age + birth_order + head_race'
if(mech=='controls') f1 <- paste0(outcome, ' ~  ', basic_controls)
if(mech!='controls') f1 <- paste0(outcome, ' ~  ', basic_controls, ' + ', iv)
mech_f <- paste0(mech, ' ~  ', basic_controls, ' + ', iv,' + (1|id)')
if(!(mech %in% c('controls','wealth'))) f1 <- paste0(f1,' + ',mech,' + (1|id)')
des <- svydesign(id=~id, weight=~cds_child_weight, data=imp_list)
m1 <- with(des, svyglm(formula=as.formula(f1),
                       family='gaussian',
                       design=des))
beta_list <- lapply(m1, function(x){coef(x)})
vcov_list <- lapply(m1, function(x){vcov(x,complete=T)})
m1 <- miceadds::pool_mi(qhat=beta_list, u=vcov_list)
summary(m1)
