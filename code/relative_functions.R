# relation <- 'sibling'
# relation_vars <- c('head_heartdis','fam_wealth')
valid_relations <- function() {
  rel <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/clean_relations.csv")
  print(unique(rel$rel_ego_alter))
}

get_relation_ids <- function(ind_ids, relation) {
  rel <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/clean_relations.csv")
  ego_rel <- rel[rel_ego_alter==relation,]
  ego_rel[, ego_id := ego_68_id*1000+ego_pid]
  ego_rel[, alter_id := alter_68_id*1000+alter_pid]
  relation_ids <- unique(ego_rel[ego_id %in% ind_ids, c('ego_id','alter_id')])
  setnames(relation_ids, c('id',paste0('id_',relation)))
  return(relation_ids)
}

# get_ref_ids <- function(ind_ids) {
#   rel <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/clean_relations.csv")
#   rel <- rel[rel_alter_ref=='self', ]
# }

merge_relative_data <- function(d,relation,relation_vars,relation_male=NULL) {
  
  ## Load clean relationship matrix
  rel <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/clean_relations.csv")
  
  relation_vars <- relation_vars[relation_vars!='male']
  ## Load full clean individual-family file
  all <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/individual_family_v2.csv')
  setnames(all, c('ER30001','ER30002'), c('ego_68_id','ego_pid'))
  all <- all[, c('year','ego_68_id','ego_pid','male',relation_vars), with=F]
  setnames(all, relation_vars, paste0(relation_vars,'_',relation))
  all[, ego_id := ego_68_id*1000+ego_pid]
  
  ## Subset relations to target
  ego_rel <- rel[rel_ego_alter==relation,]
  ego_rel[, ego_id := ego_68_id*1000+ego_pid]
  ego_rel[, alter_id := alter_68_id*1000+alter_pid]
  
  ## Merge ego id to full data in terms of alter
  # all <- merge(all, ego_rel[, c('year','ego_id','alter_id')], by=c('year','alter_id'))
  
  ## I think the above is wrong... we want to merge ego id (the parent) to full data and keep alter id to merge on to target data
  all <- merge(all, ego_rel[, c('year','ego_id','alter_id')], by=c('year','ego_id'))
  if(!is.null(relation_male)) all <- all[male==relation_male, ]
  # all[, male := NULL]
  relation_vars <- c(relation_vars,'male')
  setnames(all, 'male', paste0('male_',relation))
  
  ## Reshape wide on number of relations (in case many siblings)
  rel_numbers <- unique(all[, c('ego_id','alter_id')])
  rel_numbers[, rel_number := 1:.N, by='alter_id']
  all <- merge(all, rel_numbers, by=c('ego_id','alter_id'))
  unique_rels <- unique(all[, rel_number])
  all <- all[order(year,ego_id,alter_id)]
  all <- dcast(all, alter_id + year ~ rel_number, value.var = paste0(relation_vars,'_',relation))
  # setnames(all, c('ego_id','year',paste0(paste0(relation_vars,'_',relation,'_'), unique_rels)))
  setnames(all, 'alter_id', 'ego_id')
  
  ## Merge target variables to provided data and return
  if(!('ego_id' %in% names(d))) d[, ego_id := ER30001*1000+ER30002]
  d <- merge(d, all, by=c('year','ego_id'), all.x=T)
  
  return(d)
  
}

## Pretend the full data below are our target data, and merge on a couple sibling variables
## (for now, just a couple things in the family data, even though this will not vary)
## Can replace "d" below with any dataset that is long on "year", "ER30001", "ER30002"
# d <- fread('//sas/psc/dept/cboen_Proj/PSID/CLEAN_DATA/individual_family.csv')
# d <- merge_relative_data(d, # Can be any dataset that is long on "year", "ER30001", "ER30002"
#                          relation='sibling', # Can be any value in "rel_ego_alter" in the clean relations matrix
#                          relation_vars=c('head_heartdis','fam_wealth')) # Can be any variable in the clean individual-family file
# 

