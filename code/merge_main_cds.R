library(data.table)
library(ggplot2)
source("//sas/psc/dept/cboen_Proj/PSID/code/relative_functions.R")

## Merge TAS and individual-family main file by master ids: ER30001, ER30002
main <- fread('//sas/psc/dept/cboen_Proj/PSID/CLEAN_DATA/individual_family.csv')
main <- main[!is.na(fam_wealth_weq), ] # I thiiiink this subsets to HoH, with virtually no missingness on wealth variables.
cds <- fread("//sas/psc/dept/cboen_Proj/PSID/CLEAN_DATA/cds.csv")
cds_vars <- names(cds)[!(names(cds) %in% c('ER30001','ER30002','year'))]
setnames(cds, cds_vars, paste0('cds_', cds_vars))
all <- merge(main, cds, by=c('ER30001','ER30002','year'))
all[, id := ER30001*1000+ER30002]

## Merge in variables by relative from individual-main file
# valid_relations() # This lists all coded relations in clean relationship matrix - these are all valid for "relation" below.
# all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
#                            relation='sibling', # Can be any value in "rel_ego_alter" in the clean relations matrix
#                            relation_vars=c('hlth_rating','edu_highest','age','male','income_assets_lastyr','income_taxable_lastyr','income_tanf_lastyr','income_welfare_lastyr','emp_status')) # Can be any variable in the clean individual-family file

## Save final dataset
write.csv(all, '//sas/psc/dept/cboen_Proj/PSID/merged_data/merged_MAIN_CDS.csv')

## Plots/tables
d <- all[head_race %in% c('black','white')]
p1 <- d[, list(wealth=mean(fam_wealth_weq)), by=c('year','head_race')]
gg1 <- ggplot(data=p1) +
  geom_line(aes(color=head_race,
                x=year,
                y=wealth),
            size=2) + 
  scale_x_continuous(breaks=c(1999, 2001, 2007, 2013)) + 
  labs(x='Year', y='Wealth (in thousands $)', color='Population') + 
  theme_bw()

pdf('//sas/psc/dept/cboen_Proj/PSID/Project_RSF/results/plots.pdf', height=6, width=6)
print(gg1)
p2 <- d[, lapply(.SD, mean, na.rm=TRUE), by=c('year','head_race'), .SDcols=c('cds_depression','cds_bmi','cds_srh')] 
for(v in c('cds_depression','cds_bmi','cds_srh')) {
  gg2 <- ggplot(data=p2) + 
    geom_line(aes(color=head_race,
                  x=year,
                  y=get(v)),
              size=2) + 
    labs(y=v, x='') + 
    theme_bw()
  print(gg2)
}
dev.off()  
  
## Make comparison table (wide on four years, long on variable and two races)
## Includes means, standard deviations, % missing
table_vars <- c('head_race','fam_wealth_weq','cds_bmi','cds_srh','cds_depression')
tab1 <- all[head_race %in% c('white','black'), c('id','year',table_vars), with=F]
## Drop top 1% wealth
# top1 <- quantile(tab1[, fam_wealth_weq],.999)
# dim(tab1[fam_wealth_weq>top1, ])
for(t in table_vars) tab1[, (paste0(t,'_missing')) := ifelse(is.na(get(t)), 1, 0)]
for(v in unique(tab1[, cds_srh])) tab1[, (paste0('cds_srh_',v)) := ifelse(cds_srh==v, 1, 0)]
sum_vars <- c('fam_wealth_weq','cds_bmi',paste0('cds_srh_',unique(tab1[, cds_srh])),'cds_depression')
tab1_means <- tab1[, lapply(.SD, mean, na.rm=T), .SDcols=sum_vars, by=c('year','head_race')]
tab1_sds <- tab1[, lapply(.SD, sd, na.rm=T), .SDcols=sum_vars, by=c('year','head_race')]

