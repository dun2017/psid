library(data.table)
library(ggplot2)
repo <- 'C:/Users/ngraetz/Documents/repos/psid/'
source(paste0(repo,'code/relative_functions.R'))
in_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/'

## Load main individual/family file
main <- fread(paste0(in_dir,'individual_family2.csv'))
main[, ref_id := ER30001*1000+ER30002]
main[, id := ER30001*1000+ER30002]

## Create lagged wealth
main <- main[order(id,year)]
for(v in c('fam_wealth_noeq','fam_wealth_weq','fam_income')) main[, (paste0('l.',v)) := data.table::shift(get(v)), by=c('id')]

## Grab reference person ids and keep only reference people
rel <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/clean_relations.csv")
rel <- rel[rel_alter_ref=='self', ]
rel[, id := ego_68_id*1000+ego_pid]
rel[, ref_id := alter_68_id*1000+alter_pid]

## Read TAS, and merge by reference person to main file
tas <- fread("C:/Users/ngraetz/Downloads/clean_TAS_feb3.csv")
tas[, id := ER30001*1000+ER30002]
setnames(tas, names(tas)[!(names(tas) %in% c('ER30001','ER30002','year','id'))],
         paste0('tas_',names(tas)[!(names(tas) %in% c('ER30001','ER30002','year','id'))]))

## Only keep a couple TAS variables for now
tas <- tas[, c('ER30001','ER30002','year','id','tas_bmi','tas_srh')]

## This will also subset out years where people weren't in the sample.
tas <- merge(rel[, c('id','ref_id','year')], tas, by=c('id','year'), all.x=T)

## Merge reference person variables (e.g. wealth) from main file to each TAS individual
main_ref_vars <- c("fam_wealth_noeq","fam_wealth_weq","head_race","fam_income",
                   "l.fam_wealth_noeq","l.fam_wealth_weq","l.fam_income")
all <- merge(main[, c('year','ref_id',main_ref_vars), with=F], tas, by=c('year','ref_id'), all.x=T)
## Merge any other ego variables we need for the TAS individual from the main file (age, gender)
main_tas_vars <- c('age','male')
## Keep all main records, lag income and wealth, and then subset
all <- merge(all, main[, c('year','id',main_tas_vars), with=F], by=c('year','id'), all.x=T)

## Keep only original TAS cohort (which tries to follow-up with every individual originally in CDS)
cds <- fread(paste0(in_dir,"cds.csv"))
cds[, id := ER30001*1000+ER30002]
original_tas_cohort <- cds[year==1999, id]
all <- all[id %in% original_tas_cohort & year>=2005, ]

## Merge some relative variables
all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
                           relation='sibling', # Can be any value in "rel_ego_alter" in the clean relations matrix
                           relation_vars=c('ever_institute')) # Can be any variable in the clean individual-family file
all <- merge_relative_data(all, # Can be any dataset that is long on "year", "ER30001", "ER30002"
                           relation='parent', # Can be any value in "rel_ego_alter" in the clean relations matrix
                           relation_vars=c('edu_highest')) # Can be any variable in the clean individual-family file

