library(data.table)
library(ggplot2)
repo <- 'C:/Users/ngraetz/Documents/repos/psid/'
source(paste0(repo,'code/relative_functions.R'))
in_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/'

## Merge TAS and individual-family main file by master ids: ER30001, ER30002
main <- fread(paste0(in_dir,'individual_family3.csv'))
# main <- main[!is.na(fam_wealth_weq), ] # I thiiiink this subsets to HoH, with virtually no missingness on wealth variables.
main[, ref_id := ER30001*1000+ER30002]
main[, id := ER30001*1000+ER30002]
## Create lagged wealth
main <- main[order(id,year)]
for(v in c('fam_wealth_noeq','fam_wealth_weq','fam_income','fam_debt','own_rent')) main[, (paste0('l.',v)) := data.table::shift(get(v)), by=c('id')]

## Grab reference person ids and keep only reference people
rel <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/clean_relations.csv")
rel <- rel[rel_alter_ref=='self', ]
rel[, id := ego_68_id*1000+ego_pid]
rel[, ref_id := alter_68_id*1000+alter_pid]
# main <- main[ref_id %in% rel[, ref_id], ]

## Read CDS, tack on TAS to age 18, and merge by reference person to main file
cds <- fread(paste0(in_dir,"cds_v3.csv"))
cds_vars <- names(cds)[!(names(cds) %in% c('ER30001','ER30002','year','cds_year'))]
setnames(cds, cds_vars, paste0('cds_', cds_vars))
cds[, id := ER30001*1000+ER30002]
original_cds_cohort <- cds[cds_year==1997, id]
cds <- cds[id %in% original_cds_cohort, ]
cds <- cds[year != 2013, ]
tas <- fread('C:/Users/ngraetz/Downloads/merged_MAIN_TAS.csv')
tas[, id := ER30001*1000+ER30002]
tas <- tas[id %in% original_cds_cohort, ]
tas <- tas[, c('ER30001','ER30002','year','id','tas_bmi','tas_srh')]
setnames(tas, c('tas_srh','tas_bmi'), c('cds_srh','cds_bmi'))
tas <- tas[!(year %in% c(2005,2007)), ]
cds_tas <- rbind(cds, tas, fill=T)
## This will also subset out years where people weren't in the sample.
# cds_tas <- merge(rel[, c('id','ref_id','year')], cds_tas, by=c('id','year'), all.x=T)
cds_tas <- merge(cds_tas, rel[, c('id','ref_id','year')], by=c('id','year'), all.x=T)
## Merge reference person variables (e.g. wealth) from main file to each CDS individual
main_ref_vars <- c("fam_wealth_noeq","fam_wealth_weq","head_race","fam_income",
                   'fam_debt','l.fam_debt','own_rent','l.own_rent',
                   "l.fam_wealth_noeq","l.fam_wealth_weq","l.fam_income",
                   "childcare_exp","education_exp","healthcare_exp","housing_exp","marital_status","family_size")
# all <- merge(main[, c('year','ref_id',main_ref_vars), with=F], cds_tas, by=c('year','ref_id'), all.x=T)
all <- merge(cds_tas, main[, c('year','ref_id',main_ref_vars), with=F], by=c('year','ref_id'), all.x=T)
## Merge any other ego variables we need for the CDS individual from the main file (age, gender)
main_cds_vars <- c('age','male')
## Keep all main records, lag income and wealth, and then subset
all <- merge(all, main[, c('year','id',main_cds_vars), with=F], by=c('year','id'), all.x=T)
## Correct age variable (only variable merging from main that we DONT want to be lagged value).
all[, age := age + (cds_year-year)]
table(all[, c('head_race','age')])
## Subset to just those children interviewed in 1997
original_cds_cohort <- all[cds_year==1997 & !is.na(age), id]
all <- all[id %in% original_cds_cohort, ]
all <- all[age<=18, ]
# all[, mean(cds_depression,na.rm=T), by='year']
## CHECK OVERLAP WITH TAS IDS
# tas <- fread("C:/Users/ngraetz/Downloads/clean_TAS.csv")
# tas[, id := ER30001*1000+ER30002]
# length(unique(all[id %in% tas$id,id])) / length(unique(all[,id]))
# tas[, mean(bmi,na.rm=T), by='year']
# all <- all[year!=2013, ]

## Merge in variables by relative from individual-main file
# valid_relations() # This lists all coded relations in clean relationship matrix - these are all valid for "relation" below.

## Merge mother's education 
all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
                           relation='parent', # Can be any value in "rel_ego_alter" in the clean relations matrix
                           relation_male=0,
                           relation_vars=c('edu_highest')) 

## INCOME IS NOT COLLECTED IN 1999, 2001
## Calculate total taxable income by summing over parents' taxable incomes
# all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
#                            relation='parent', # Can be any value in "rel_ego_alter" in the clean relations matrix
#                            relation_vars=c('income_taxable')) 

## Calculate child birth order by checking ages of siblings in family
all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
                           relation='sibling', # Can be any value in "rel_ego_alter" in the clean relations matrix
                           relation_vars=c('age')) 
all[, total_children := 0]
for(a in 1:10) all[, total_children := ifelse(!is.na(get(paste0('age_sibling_',a))), total_children+1, total_children)]
all[, younger_than_ego := 0]
for(a in 1:10) all[, younger_than_ego := ifelse(get(paste0('age_sibling_',a))<age & !is.na(get(paste0('age_sibling_',a))), younger_than_ego+1, younger_than_ego)]
all[, birth_order := total_children - younger_than_ego + 1]

## Use relationships matrix to calculate family structure (binary single-parent?)
# all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
#                            relation='parent', # Can be any value in "rel_ego_alter" in the clean relations matrix
#                            relation_vars=c('age')) 

## Save final dataset
write.csv(all, paste0(in_dir,'merged_MAIN_CDS5.csv'))
all <- fread(paste0(in_dir,'merged_MAIN_CDS5.csv'))

## Plots/tables
# all <- fread(paste0(in_dir,'merged_MAIN_CDS.csv'))
d <- all[head_race %in% c('black','white')]
p1 <- d[, list(wealth=median(fam_wealth_weq)), by=c('year','head_race')]
gg1 <- ggplot(data=p1) +
  geom_line(aes(color=head_race,
                x=year,
                y=wealth),
            size=2) +
  scale_x_continuous(breaks=c(1999, 2001, 2007)) +
  labs(x='Year', y='Wealth (in thousands $)', color='Population') +
  theme_bw()

pdf('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results/plots.pdf', height=6, width=6)
print(gg1)
for(v in unique(d[, cds_srh])) d[, (paste0('cds_srh_',v)) := ifelse(cds_srh==v, 1, 0)]
d[, cds_fairpoor_srh := ifelse(cds_srh_poor==1 | cds_srh_fair==1, 1, 0)]
p2 <- d[, lapply(.SD, mean, na.rm=TRUE), by=c('year','head_race'), .SDcols=c('fam_wealth_weq','cds_bmi','cds_fairpoor_srh','cds_depression')]
for(v in c('fam_wealth_weq','cds_fairpoor_srh','cds_bmi','cds_depression')) {
  gg2 <- ggplot(data=p2) +
    geom_line(aes(color=head_race,
                  x=year,
                  y=get(v)),
              size=2) +
    labs(y=v, x='') +
    scale_x_continuous(breaks=c(1999, 2001, 2007)) +
    theme_bw()
  print(gg2)
}
dev.off()

## Make comparison table (wide on four years, long on variable and two races)
## Includes means, standard deviations, % missing
table_vars <- c('head_race','age','fam_wealth_weq','edu_highest_parent_1',
                'cds_bmi','cds_srh','cds_depression','cds_allergies',
                'cds_anemia',"cds_asthma","cds_emotional","cds_ear","cds_pcg_srh","cds_child_medicaid",
                "cds_dr_visits","cds_sleephours","cds_missed_school","cds_missed_social",
                "cds_college_savings","cds_college_based_on_cost",'cds_money_left','cds_behind_bills',
                'cds_hospital_visits','cds_parenting_strain','cds_family_calm_discuss')
tab1 <- all[head_race %in% c('white','black'), c('id','year',table_vars), with=F]
## Drop top 1% wealth
# top1 <- quantile(tab1[, fam_wealth_weq],.999)
# dim(tab1[fam_wealth_weq>top1, ])
# sum_vars <- c('fam_wealth_weq','age','cds_bmi','cds_fairpoor_srh','cds_depression')
for(v in unique(tab1[, cds_srh])) tab1[, (paste0('cds_srh_',v)) := ifelse(cds_srh==v, 1, 0)]
tab1[, cds_fairpoor_srh := ifelse(cds_srh_poor==1 | cds_srh_fair==1, 1, 0)]
for(v in unique(tab1[, cds_pcg_srh])) tab1[, (paste0('cds_pcg_srh_',v)) := ifelse(cds_pcg_srh==v, 1, 0)]
tab1[, cds_fairpoor_pcg_srh := ifelse(cds_pcg_srh_poor==1 | cds_pcg_srh_fair==1, 1, 0)]
for(v in unique(tab1[, edu_highest_parent_1])) tab1[, (paste0('edu_highest_parent_1_',v)) := ifelse(edu_highest_parent_1==v, 1, 0)]
tab1[edu_highest_parent_1_higher==1, edu_highest_parent_1_college := 1]
for(v in unique(tab1[, cds_money_left])) tab1[, (paste0('cds_money_left_',v)) := ifelse(cds_money_left==v, 1, 0)]
for(v in unique(tab1[, cds_money_left])) tab1[cds_money_left_NA==1, (paste0('cds_money_left_',v)) := NA]
tab1 <- tab1[order(year,head_race)]
# tab1_means <- tab1[, lapply(.SD, mean, na.rm=T), .SDcols=sum_vars, by=c('year','head_race')]
# tab1_sds <- tab1[, lapply(.SD, sd, na.rm=T), .SDcols=sum_vars, by=c('year','head_race')]
# tab1_missing <- tab1[, lapply(.SD, mean, na.rm=T), .SDcols=paste0(sum_vars,'_missing'), by=c('year','head_race')]

d <- copy(tab1)
d <- melt(d, id.vars = c('id','year','head_race'), measure.vars=c('age','fam_wealth_weq','edu_highest_parent_1_college',
                                                                  'edu_highest_parent_1_some college',
                                                                  'edu_highest_parent_1_less than hs',
                                                                  'edu_highest_parent_1_hs',
                                                                  'cds_bmi','cds_fairpoor_srh','cds_depression','cds_allergies',
                                                                  'cds_anemia',"cds_asthma","cds_emotional","cds_ear",
                                                                  "cds_fairpoor_pcg_srh","cds_child_medicaid","cds_dr_visits",
                                                                  "cds_sleephours","cds_missed_school","cds_missed_social",
                                                                  "cds_college_savings","cds_college_based_on_cost",
                                                                  'cds_money_left_not_enough',
                                                                  'cds_money_left_just_enough','cds_behind_bills',
                                                                  'cds_hospital_visits','cds_parenting_strain',
                                                                  'cds_family_calm_discuss'))
d[, value := as.numeric(value)]
d[, n := 1]
d_all <- d[variable!='fam_wealth_weq', list(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T), missing=sum(is.na(value)), n=sum(n)), by=c('year','head_race','variable')]
d_wealth <- d[variable=='fam_wealth_weq', list(mean=median(value,na.rm=T), sd=sd(value,na.rm=T), missing=sum(is.na(value)), n=sum(n)), by=c('year','head_race','variable')]
d <- rbind(d_all,d_wealth)
d[, missing := missing/n]
n <- unique(d[, c('year','head_race','n')])
n <- dcast(n, year ~ head_race)
setnames(n, c('year','mean_black','mean_white'))
d <- dcast(d, year + variable ~ head_race, value.var=c('mean','sd','missing'))
setcolorder(d, c('year','variable','mean_black','sd_black','missing_black','mean_white','sd_white','missing_white'))
d <- d[order(variable,year)]
for(v in c('mean_black','sd_black','missing_black','mean_white','sd_white','missing_white')) d[, (v) := as.character(round(get(v), 2))]
for(v in c('sd_black','sd_white')) d[, (v) := paste0('(',get(v),')')]
d <- rbind(d,n,fill=T)

## CLEAN NAMES
clean_names <- data.table(variable=c('age','fam_wealth_weq',
                                     'edu_highest_parent_1_college','edu_highest_parent_1_some college',
                                     'edu_highest_parent_1_hs','edu_highest_parent_1_less than hs',
                                     'cds_college_based_on_cost','cds_child_medicaid','cds_dr_visits','cds_hospital_visits',
                                     'cds_sleephours','cds_missed_school','cds_missed_social',
                                     'cds_money_left_not_enough','cds_money_left_just_enough','cds_behind_bills',
                                     'cds_parenting_strain','cds_family_calm_discuss',
                                     'cds_college_savings',
                                     'cds_bmi','cds_fairpoor_srh','cds_depression','cds_allergies',
                                     'cds_anemia','cds_asthma','cds_emotional','cds_ear','cds_fairpoor_pcg_srh'),
                          name=c('Age','Median wealth','College (mother)','Some college','High school','Less HS',
                                 'College based on cost','Child Medicaid','Child doctor visits','Child hospital visits',
                                 'Child sleep hours','Missed school (health)','Missed social (health)',
                                 'Money left (not enough)','Money left (just enough)','Behind bills',
                                 'Parenting strain','No family conflict',
                                 'College savings',
                                 'BMI','Fair/poor SRH','Depression','Allergies',
                                 'Anemia','Asthma','Emotional','Ear infections','Fair/poor SRH (PCG report)'))
clean_names[, cov_sort := 1:.N]
d <- merge(d, clean_names, by='variable', all.x=T)
d <- d[order(cov_sort)]
setcolorder(d, 'name')
d[, cov_sort := NULL]
d[, variable := NULL]
d[sd_black=='(NA)', sd_black := '(--)']
d[sd_white=='(NA)', sd_white := '(--)']
d[mean_black=='NaN', mean_black := '--']
d[mean_white=='NaN', mean_white := '--']

library(flextable)
library(officer)

first_header <- as.list(c('','',rep(c('Mean','SD','Missing'),2)))
names(first_header) <- names(d)
second_header <- as.list(c('Year','Variable',rep('Black',3),rep('White',3)))
names(second_header) <- names(d)

ft <- flextable(d, theme_fun = theme_booktabs) %>%
  delete_part(part = "header") %>%
  add_header(values=first_header) %>%
  add_header(values=second_header) %>%
  merge_h(part = "header") %>%
  align(align = 'right', part='body') %>%
  align(j=1, align = 'left', part='body') %>%
  # align(j=grep('p_value', names(d)), align = 'left', part='body') %>%
  align(i=1, align = 'center', part='header') %>%
  align(i=2, align = 'right', part='header') %>%
  hline_top(part = 'body', border = fp_border(color='black',width=2)) %>%
  hline_top(part = 'header', border = fp_border(color='black',width=2)) %>%
  hline(j=3:8,part = 'header', border = fp_border(color='black',width=2)) %>%
  padding(padding = 0.0001, part = 'all') %>%
  padding(padding.top = 0, part='all') %>%
  # width(j=2:8, width=0.4) %>%
  width(j=1, width=1) %>%
  # add_footer_lines(c('* p < 0.05; ** p < 0.01; *** p < 0.001')) %>%
  font(fontname = 'Times New Roman', part='all') %>%
  fontsize(size = 12, part='all') %>%
  set_formatter_type(fmt_double="%.02f")


setwd('C:/Users/ngraetz/Dropbox/Penn/papers/psid/results')
doc <- read_docx() %>%
  body_add_flextable(value = autofit(ft), split = TRUE) %>%
  # body_end_section_landscape() %>% # a landscape section is ending here
  print(target = "Table1_CDS_wealth_v2.docx" )

