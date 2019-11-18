library(data.table)
library(ggplot2)
source("//sas/psc/dept/cboen_Proj/PSID/code/relative_functions.R")

## Merge TAS and individual-family main file by master ids: ER30001, ER30002
main <- fread('//sas/psc/dept/cboen_Proj/PSID/CLEAN_DATA/individual_family.csv')
tas <- fread("//sas/psc/dept/cboen_Proj/PSID/TAS/clean_TAS.csv")
tas_vars <- names(tas)[!(names(tas) %in% c('ER30001','ER30002','year'))]
setnames(tas, tas_vars, paste0('tas_', tas_vars))
all <- merge(main, tas, by=c('ER30001','ER30002','year'))
all[, id := ER30001*1000+ER30002]

## Get father ids for all TAS ids
tas_ids <- unique(all[, id])
parent_ids <- get_relation_ids(tas_ids, relation='parent')
write.csv(parent_ids, '//sas/psc/dept/cboen_Proj/PSID/CLEAN_DATA/parent_ids.csv', row.names = F)

## Merge in variables by relative from individual-main file
valid_relations() # This lists all coded relations in clean relationship matrix - these are all valid for "relation" below.
all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
                           relation='sibling', # Can be any value in "rel_ego_alter" in the clean relations matrix
                           relation_vars=c('hlth_rating','edu_highest','age','male','income_assets_lastyr','income_labor_lastyr','income_taxable_lastyr','income_childsupp_lastyr','income_relhelp_lastyr','income_ssi_lastyr','income_tanf_lastyr','income_unemp_lastyr','income_welfare_lastyr','emp_status','ever_institutionalized')) # Can be any variable in the clean individual-family file
all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
                           relation='parent', # Can be any value in "rel_ego_alter" in the clean relations matrix
                           relation_vars=c('hlth_rating','edu_highest','age','male','income_assets_lastyr','income_labor_lastyr','income_taxable_lastyr','income_childsupp_lastyr','income_relhelp_lastyr','income_ssi_lastyr','income_tanf_lastyr','income_unemp_lastyr','income_welfare_lastyr','emp_status','ever_institutionalized')) # Can be any variable in the clean individual-family file


## Save final dataset
write.csv(all, '//sas/psc/dept/cboen_Proj/PSID/merged_data/merged_MAIN_TAS.csv')

## Plots/tables 
all <- fread('//sas/psc/dept/cboen_Proj/PSID/merged_data/merged_MAIN_TAS.csv')
for(v in paste0('hlth_rating_sibling_',1:10)) all[, (v) := ifelse(get(v)=='excellent',1,0)]
all[, sibling_srh := apply(.SD, 1, mean, na.rm=T), .SDcols=paste0('hlth_rating_sibling_',1:10)]
test <- all[head_race %in% c('black','white') & !is.na(tas_incar_ever), lapply(.SD, mean, na.rm=TRUE), by=c('year','head_race','tas_incar_ever'), .SDcols=c('sibling_srh')] 
test[, cat := paste0(head_race,'_',ifelse(tas_incar_ever==1,'incarerated','not_incarcerated'))]
all[, n := 1]
counts <- all[head_race %in% c('black','white') & !is.na(tas_incar_ever), list(N=sum(n)), by=c('year','head_race','tas_incar_ever')]
write.csv(counts, '//sas/psc/dept/cboen_Proj/PSID/merged_data/tas_incar_counts.csv', row.names=F)
counts <- all[head_race %in% c('black','white') & !is.na(tas_incar_ever) & !is.na(sibling_srh), list(N=sum(n)), by=c('year','head_race','tas_incar_ever')]
write.csv(counts, '//sas/psc/dept/cboen_Proj/PSID/merged_data/tas_incar_counts_siblings.csv', row.names=F)
write.csv(test, '//sas/psc/dept/cboen_Proj/PSID/merged_data/tas_incar_sibling_srh.csv', row.names=F)
ggplot(data=test) + 
  geom_line(aes(x=year,
                y=sibling_srh,
                color=cat),
            size=2) + 
  theme_bw()
