library(haven)
library(data.table)
library(ggplot2)
library(naniar)
library(mice)
library(survey)

in_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/'
out_dir <- 'C:/Users/ngraetz/Dropbox/Penn/papers/psid/data/'

## Function to apply codebook and keep only those clean extracted variables.
pull_main_wave <- function(y, cb) {
  message(paste0('Extracting ', y, '...'))
  ## Load full data and codebook.
  full_cb <- as.data.table(readxl::read_excel(paste0(in_dir,"cds_crossyear_cb.xlsx"),
                                              sheet = 'Sheet1')) 
  # full_cb <- full_cb[!(var_rename %in% c('wealth','debt')) & wave==y, ]
  full_cb <- full_cb[wave==y, ]
  setnames(full_cb, 'wave', 'year')
  # d <- fread(paste0('//sas/psc/dept/cboen_Proj/PSID/Main File/data',y,'.csv'))
  missing_vars <- full_cb[!(var %in% names(d)), var_rename]
  message(paste('Missing var:', missing_vars, collapse = ', '))
  full_cb <- full_cb[var %in% names(d) | !is.na(sum_values), ]
  ## Recode any raw variables that need recoding.
  for(v in full_cb[, var]) {
    if(!(v %in% names(d))) message(paste0('...', v, ' is missing from main file.'))
    if(v %in% names(d) | full_cb[var==v, sum_values==1]) {
      v <- gsub(' ','',v)
      ## Keep codebook where we are using this raw variable.
      raw_var_cb <- full_cb[var==v, ]
      ## In case this raw variable is being used to construct multiple clean variables.
      raw_var_cb[, n := 1:.N] 
      ## Loop over and process each clean variable constructured using this raw variable.
      for(i in raw_var_cb[, n]) {
        cb <- raw_var_cb[n==i, ]
        if(!is.na(cb[var==v, sum_values])) {
          if(!is.na(cb[var==v, sum_letters])) all_vars <- paste0(v, LETTERS[1:cb[var==v, sum_letters]])
          if(!is.na(cb[var==v, sum_numbers])) all_vars <- paste0(v, '_', 1:cb[var==v, sum_numbers])
        }
        if(is.na(cb[var==v, sum_values])) all_vars <- v
        # all_vars <- v
        message(paste0('Recoding ', v, '...'))
        for(sv in all_vars) {
          if(is.na(cb[var==v, sum_values])) {
            this_rename <- cb[var==v, var_rename]
            d[, (this_rename) := as.character(get(v))]
          }
          if(!is.na(cb[var==v, sum_values])) {
            this_rename <- sv
            d[, (this_rename) := as.character(get(sv))]
          }
          for(val in 1:6) {
            this_val <- cb[var==v, get(paste0('value_',val))]
            ## Split up multiple values if provided (",") or numeric range ("_").
            if(grepl(',',this_val)) this_val <- unlist(strsplit(this_val, ','))
            handle_seq <- function(x) {
              if(grepl('_',x)) {
                x <- unlist(strsplit(x, '_'))
                x <- as.character(x[1]:x[2])
              }
              return(as.character(x))
            }
            this_val <- unlist(lapply(this_val, handle_seq))
            ## Grab recode value.
            this_recode <- cb[var==v, get(paste0('recode_',val))]
            ## Do the recode.
            if(!is.na(this_val[1])) {
              d[get(sv) %in% this_val, (this_rename) := this_recode] 
            }
          }
          ## After recoding, make numeric if numeric specified.
          if(!is.na(cb[var==v, numeric])) d[, (this_rename) := as.numeric(as.character(get(this_rename)))]
        }
        ## Sum over multiple columns if needed (scales variables).
        if(!is.na(cb[var==v, sum_values])) d[, (cb[var==v, var_rename]) := rowSums(.SD, na.rm = TRUE), .SDcols = all_vars] 
      }
    }
  }
  ## Return cleaned dataset.
  d[, year := y]
  d <- d[, c('ER30001','ER30002','year',full_cb[, var_rename]), with=FALSE]
  return(d)
}

## Extract each year of TAS survey.
main_years <- c(1997,2002,2007)
d <- fread(paste0(in_dir,'cds_crossyear.csv'))
full_cb <- as.data.table(readxl::read_excel(paste0(in_dir,"cds_crossyear_cb.xlsx"),
                                            sheet = 'Sheet1')) 

################################################################
## ADD ADDENDUMS
################################################################
d2 <- fread(paste0(in_dir,'cds_crossyear_update_dec1.csv'))
update_vars <- full_cb[var_rename %in% c('hospital_visits','parenting_strain','family_calm_discuss'), var]
d2 <- d2[, c('ER30001','ER30002',update_vars), with=F]
d <- merge(d, d2, by=c('ER30001','ER30002'))
## ADDENDUM 2: 
d2 <- fread(paste0(in_dir,'J269057.csv'))
update_vars <- full_cb[var_rename %in% c('economic_strain'), var]
update_vars <- c(names(d2)[grepl(update_vars[1], names(d2))],
                 names(d2)[grepl(update_vars[2], names(d2))],
                 names(d2)[grepl(update_vars[3], names(d2))])
d2 <- d2[, c('ER30001','ER30002',update_vars), with=F]
d <- merge(d, d2, by=c('ER30001','ER30002'))
## ADDENDUM 3: weights
d3 <- fread(paste0(in_dir,'J269640.csv'))
update_vars <- full_cb[var_rename %in% c('child_weight'), var]
d3 <- d3[, c('ER30001','ER30002',update_vars), with=F]
d <- merge(d, d3, by=c('ER30001','ER30002'))
################################################################

all <- rbindlist(lapply(main_years, pull_main_wave), fill=T)
## FIX CDS YEAR DISCREPANCIES
all[year==1997, year := 1999]
all[year==2002, year := 2001]
all[year==2014, year := 2013]
write.csv(all, paste0(out_dir, 'cds.csv'), row.names = F)

# all <- fread('//sas/psc/dept/cboen_Proj/PSID/CLEAN_DATA/individual_family.csv')
# all[, id := ER30001*1000+ER30002]
# all[, n := .N, by='id']
table(all[, c('year','bmi')], useNA = 'always')
all[, mean(economic_strain,na.rm=T), by='year']
