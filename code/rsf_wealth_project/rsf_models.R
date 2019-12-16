library(data.table)
library(ggplot2)
library(lme4)
library(lmerTest)

repo <- 'C:/Users/ngraetz/Documents/repos/psid/'
source(paste0(repo,'code/relative_functions.R'))
in_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/'

## Load clean, merged data with all variables for analyses.
all <- fread(paste0(in_dir,'merged_MAIN_CDS.csv'))
all <- all[head_race %in% c('white','black'),]
all[, head_race := factor(head_race, levels = c('white','black'))]

## Adjust wealth for inflation using 2019 CPI https://cpiinflationcalculator.com/historical-tables/
all[, fam_wealth_weq := as.double(fam_wealth_weq)]
all[year==1999, fam_wealth_weq := fam_wealth_weq*166.6/244.979]
all[year==2001, fam_wealth_weq := fam_wealth_weq*177.1/244.979]
all[year==2007, fam_wealth_weq := fam_wealth_weq*207.342/244.979]
all[year==2009, fam_wealth_weq := fam_wealth_weq*214.537/244.979]
all[year==2011, fam_wealth_weq := fam_wealth_weq*224.939/244.979]
all[year==2013, fam_wealth_weq := fam_wealth_weq*232.957/244.979]
# fam_wealth_weq = fam_wealth_weq/(fusize^0.55)

## Figure out how to normalize wealth
## Use a top and lower bound and then standardize?
length(all[fam_wealth_weq>=-150000 & fam_wealth_weq<=500000, fam_wealth_weq]) / dim(all)[1]
all[fam_wealth_weq < -150000, fam_wealth_weq := -150000]
all[fam_wealth_weq > 500000, fam_wealth_weq := 500000]
all[fam_wealth_weq>=-150000 & fam_wealth_weq<=500000, fam_wealth_weq_sd := scale(fam_wealth_weq)]
full_dist <- density(all[, fam_wealth_weq])
full_dist.dt <- data.table(fam_wealth_weq=full_dist$x, fam_wealth_weq_density=full_dist$y)

pdf('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/wealth_distribution.pdf',width=8,height=5)
ggplot() + 
  geom_density(data=all,
                 aes(x=fam_wealth_weq,
                     fill=head_race,
                     color=head_race),
                 alpha=0.5) + 
  # geom_density(data=all,
  #              aes(x=fam_wealth_weq),
  #              color='black',
  #              size=1)
  geom_line(data=full_dist.dt,
            aes(x=fam_wealth_weq,
                y=fam_wealth_weq_density),
            size=1, color='black') + 
  scale_fill_discrete(name='Race') + 
  guides(color=FALSE) + 
  lims(x=c(-150000,500000)) + 
  theme_minimal()
dev.off()

## Standardized continuous variables
# scale_vars <- c('fam_wealth_weq')
# all[, (scale_vars) := lapply(.SD, scale), .SDcols=scale_vars]

## Other variable transforms
all[!is.na(cds_srh), cds_srhfairpoor := ifelse(cds_srh %in% c('fair','poor'), 1, 0)]
all[edu_highest_parent_1=='0', edu_highest_parent_1 := NA]
all[, chronic_count := cds_asthma + cds_anemia + cds_allergies + cds_ear]
all[edu_highest_parent_1=='higher', edu_highest_parent_1 := 'college']
all[!is.na(cds_missed_school) & !is.na(cds_missed_social), health_limit := ifelse(cds_missed_school==1 | cds_missed_social==1, 1, 0)]

all[, n := 1]
traj <- all[, list(n=sum(n)), by='ego_id']
traj3 <- traj[n==3, ego_id]

## OUTCOME TRAJECTORIES BY RACE/WEALTH
## Make age trajectories of BMI from 0 to 18
traj_data <- copy(all)
traj_data[fam_wealth_weq_sd >= 0.5, wealth_cat := '>= 0.5 SD']
traj_data[fam_wealth_weq_sd <= -0.5, wealth_cat := '<= -0.5 SD']
traj_data[is.na(wealth_cat), wealth_cat := '-0.5 to 0.5 SD']
traj_data[, n := 1]
traj_data[age<6, agegrp := 3]
traj_data[age>=6 & age <10, agegrp := 8]
traj_data[age>=10 & age <14, agegrp := 12]
traj_data[age>=14, agegrp := 16]
traj_data_agg <- traj_data[, list(cds_bmi=mean(cds_bmi,na.rm=T),cds_depression=mean(cds_depression,na.rm=T),
                              cds_srhfairpoor=mean(cds_srhfairpoor,na.rm=T),chronic_count=mean(chronic_count,na.rm=T),
                              health_limit=mean(health_limit,na.rm=T),
                              n=sum(n)), by=c('agegrp','wealth_cat','head_race')]
library(gridExtra)
pdf('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/outcome_trajectories.pdf',width=15,height=8)
for(v in c('cds_bmi','cds_depression','cds_srhfairpoor','chronic_count','health_limit')) {
traj_counts <- traj_data[!is.na(get(v)), list(personyears=sum(n)), by=c('wealth_cat','head_race')]
traj_counts <- traj_counts[order(head_race,wealth_cat)]
gg_outcome <- ggplot() + 
  geom_line(data=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), ],
              aes(x=agegrp,
                  y=get(v),
                  color=wealth_cat,
                  linetype=head_race), size=1) + 
  # annotation_custom(tableGrob(traj_counts, rows=NULL), 
  #                   xmin=16, xmax=6, ymin=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), quantile(get(v),.15)],
  #                   ymax=traj_data_agg[!is.na(get(v)) & !is.nan(get(v)), quantile(get(v),.15)]) + 
  labs(x='Age',y=v) + 
  theme_minimal()
traj_counts <- tableGrob(traj_counts, rows=NULL)
grid.arrange(gg_outcome, traj_counts, 
             nrow = 1, ncol=2, widths = c(2, 0.5))
}
dev.off()

## Fit basic growth curve model without controlling for potential mediators.
## Outcomes: BMI, depression, SRH, count of chronic (negative binomial), one 0/1 of health limitations measures (any reported)
##    -- cds_bmi, cds_depression, cds_srhfairpoor, chronic_count, health_limit
all[, cds_parenting_strain := scale(cds_parenting_strain)]
all[, cds_behind_bills := scale(cds_behind_bills)]

basic_controls <- 'as.factor(male) + birth_order + as.factor(edu_highest_parent_1) + as.factor(head_race) + age'
mechanisms <- c('cds_parenting_strain + cds_behind_bills')
linear_outcomes <- c('cds_bmi')
binary_outcomes <- c('cds_srhfairpoor','health_limit')
count_outcomes <- c('chronic_count','cds_depression')
file_tag <- 'preliminary'
for(outcome in c(linear_outcomes,binary_outcomes,count_outcomes)) {

message(outcome)
f1 <- paste0(outcome, ' ~  ', basic_controls, ' + fam_wealth_weq_sd*as.factor(head_race) + (1|ego_id)')
f2 <- paste0(outcome, ' ~  ', basic_controls, ' + ', mechanisms, ' + fam_wealth_weq_sd*as.factor(head_race) + (1|ego_id)')
if(outcome %in% linear_outcomes) {
  model1 <- lmerTest::lmer(f1, data=all, REML=FALSE)
  model2 <- lmerTest::lmer(f2, data=all, REML=FALSE)
}
if(outcome %in% binary_outcomes) {
  # f1 <- paste0(outcome, ' ~  ', basic_controls, ' + fam_wealth_weq_sd*as.factor(head_race)')
  # f2 <- paste0(outcome, ' ~  ', basic_controls, ' + ', mechanisms, ' + fam_wealth_weq_sd*as.factor(head_race)')
  # model1 <- glm(f1, data=all, family='binomial')
  # model2 <- glm(f2, data=all, family='binomial')
  model1 <- glmer(f1, data=all, family='poisson')
  model2 <- glmer(f2, data=all, family='poisson')
}
if(outcome %in% count_outcomes) {
  model1 <- glmer(f1, data=all, family='poisson')
  model2 <- glmer(f2, data=all, family='poisson')
}

# f2 <- paste0('cds_depression ~  ', basic_controls, ' + fam_wealth_weq_sd + (1|ego_id)')
# m2 <- lmerTest::lmer(f2, data=all[ego_id %in% traj3, ])
# f3 <- paste0('cds_srhfairpoor ~  ', basic_controls, ' + fam_wealth_weq_sd + (1|ego_id)')
# m3 <- glmer(f3, data=all[ego_id %in% traj3, ], family='binomial')
# f4 <- paste0('chronic_count ~  ', basic_controls, ' + fam_wealth_weq_sd + (1|ego_id)')
# m4 <- glmer(f4, data=all[ego_id %in% traj3, ], family='poisson')
# f5 <- paste0('health_limit ~  ', basic_controls, ' + fam_wealth_weq_sd + (1|ego_id)')
# m5 <- glmer(f5, data=all[ego_id %in% traj3, ], family='binomial')

library(lmerTest)
library(MuMIn)
get_coefs <- function(x) {
  this_sum <- summary(get(x))
  coefs <- as.data.table(this_sum$coefficients)
  coefs[, variable := rownames(this_sum$coefficients)]
  if('Pr(>|z|)' %in% names(coefs)) coefs <- coefs[, c('Estimate','Std. Error','Pr(>|z|)','variable')]
  if('Pr(>|t|)' %in% names(coefs)) coefs <- coefs[, c('Estimate','Std. Error','Pr(>|t|)','variable')]
  setnames(coefs, c('mean','se','pvalue','variable'))
  coefs[, p := '']
  coefs[pvalue<=0.05, p := paste0(p,'*')]
  coefs[pvalue<=0.01, p := paste0(p,'*')]
  coefs[pvalue<=0.001, p := paste0(p,'*')]
  coefs[pvalue<=0.10 & is.na(p), p := paste0(p,'.')]
  coefs[, pvalue := NULL]
  rs <- r.squaredGLMM(get(x))
  rs <- data.table(variable=c('mar_rsq','cond_rsq'),mean=c(rs[1],rs[2]))
  bic <- data.table(variable=c('bic'),mean=round(this_sum$AICtab['BIC']))
  # if(outcome %in% linear_outcomes) bic <- data.table(variable=c('bic'),mean=round(this_sum$AICtab['BIC']))
  # if(outcome %in% binary_outcomes) bic <- data.table(variable=c('bic'),mean=round(this_sum$aic))
  py <- data.table(variable=c('N'),mean=this_sum$devcomp$dims['N'])
  people <- data.table(variable=c('people'),mean=this_sum$ngrps)
  coefs <- rbindlist(list(coefs, rs, bic, py, people), fill=T)
  coefs[, model := x]
  coefs[variable=="center_age:edu_years", variable := "edu_years:center_age"]
  coefs[variable=="center_age:edu_years:raceblack", variable := "edu_years:center_age:raceblack"]
  coefs[variable=='center_age:raceblack:edu_years_mother', variable := 'center_age:edu_years_mother:raceblack']
  coefs[variable=="raceblack:edu_years_mother", variable := 'edu_years_mother:raceblack"']
  return(coefs)
}
options(scipen = 999)
coefs <- rbindlist(lapply(c('model1','model2'), get_coefs))
coefs[is.na(p), p := '']
for(v in c('mean','se')) coefs[, (v) := format(round(get(v),2), nsmall=0, big.mark=",")]
coefs[se=='     NA', se := '']

clean_names <- data.table(variable=c("(Intercept)","as.factor(male)1","birth_order",
                                     "age","as.factor(edu_highest_parent_1)less than hs","as.factor(edu_highest_parent_1)hs",
                                     "as.factor(edu_highest_parent_1)some college",
                                     "as.factor(head_race)black","fam_wealth_weq_sd",
                                     "as.factor(head_race)black:fam_wealth_weq_sd",
                                     "cds_parenting_strain","cds_behind_bills",
                                     "mar_rsq","cond_rsq","bic","N","people"),
                          name=c('Intercept','Male','Birth order',
                                 "Age","Less than HS","HS",
                                 "Some college",
                                 "Black","Wealth",
                                 "Black*Wealth",
                                 "Parenting strain","Behind on bills",
                                 'Marginal R2','Conditional R2','BIC','Person-years','Individuals'))
clean_names[, cov_sort := 1:.N]
coefs <- merge(coefs, clean_names, by='variable', all.x=T)
coefs <- coefs[!is.na(name), ]
coefs <- dcast(coefs, cov_sort+name~model, value.var = c('mean','se','p'))
coefs <- coefs[order(cov_sort)]
coefs[, c('cov_sort') := NULL]
for(v in names(coefs)[names(coefs)!='name']) coefs[is.na(get(v)), (v) := '']
for(v in paste0('se_model',c(1:2))) {
  coefs[get(v)=='  NA', (v) := '']
  coefs[get(v)!='', (v) := paste0('(',get(v),')')]
}
setcolorder(coefs, c('name','mean_model1','se_model1','p_model1',
                     'mean_model2','se_model2','p_model2'))

first_header <- as.list(c('',rep(c('Coef.','SE',''),4)))
second_header <- as.list(c('',rep('Model 1',3),rep('Model 2',3),rep('Model 3',3),rep('Model 4',3)))
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
  hline(j=1:dim(coefs)[2],i=dim(coefs)[1]-5,part = 'body', border = fp_border(color='black',width=2)) %>%
  padding(padding = 0.0001, part = 'all') %>%
  padding(padding.top = 0, part='all') %>%
  # width(j=2:8, width=0.4) %>%
  # width(j=1, width=2.23) %>%
  add_footer_lines(c('* p < 0.05; ** p < 0.01; *** p < 0.001')) %>%
  fontsize(size = 12, part='all') %>%
  font(fontname = 'Times New Roman', part='all') %>%
  autofit()

setwd('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/')
doc <- read_docx() %>%
  body_add_flextable(value = ft, split = TRUE) %>%
  body_end_section_landscape() %>% # a landscape section is ending here
  print(target = paste0("models_", file_tag, "_", outcome, ".docx"))

}
