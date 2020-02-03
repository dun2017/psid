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
  full_cb <- as.data.table(readxl::read_excel(paste0(in_dir,"codebook_main_file_v5.xlsx"),sheet = 'Sheet1')) 
  # full_cb <- full_cb[!(var_rename %in% c('wealth','debt')) & wave==y, ]
  full_cb <- full_cb[wave==y, ]
  setnames(full_cb, 'wave', 'year')
  # d <- fread(paste0('//sas/psc/dept/cboen_Proj/PSID/Main File/data',y,'.csv'))
  missing_vars <- full_cb[!(var %in% names(d)), var_rename]
  message(paste('Missing var:', missing_vars, collapse = '\n'))
  full_cb <- full_cb[var %in% names(d), ]
  ## Recode any raw variables that need recoding.
  for(v in full_cb[, var]) {
    if(!(v %in% names(d))) message(paste0('...', v, ' is missing from main file.'))
    if(v %in% names(d)) {
    v <- gsub(' ','',v)
    ## Keep codebook where we are using this raw variable.
    raw_var_cb <- full_cb[var==v, ]
    ## In case this raw variable is being used to construct multiple clean variables.
    raw_var_cb[, n := 1:.N] 
    ## Loop over and process each clean variable constructured using this raw variable.
    for(i in raw_var_cb[, n]) {
      cb <- raw_var_cb[n==i, ]
      all_vars <- v
      message(paste0('Recoding ', v, '...'))
      for(sv in all_vars) {
        this_rename <- cb[var==v, var_rename]
        d[, (this_rename) := as.character(get(v))]
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
      if(!is.na(cb[var==v, sum_values])) d[, (this_rename) := rowSums(.SD, na.rm = TRUE), .SDcols = all_vars] 
      }
    }
  }
  ## Return cleaned dataset.
  d[, year := y]
  d <- d[, c('ER30001','ER30002','year',full_cb[, var_rename]), with=FALSE]
  return(d)
}

## Extract each year of TAS survey.
main_years <- c(1994,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017)
d <- fread(paste0(in_dir,'crossyear.csv'))
full_cb <- as.data.table(readxl::read_excel(paste0(in_dir,"codebook_main_file_v5.xlsx"), sheet = 'Sheet1')) 
full_cb[, var := gsub(' ','',var)]

################################################################################################################
## ADD ADDENDUMS TO RAW MAIN FILE HERE
## Indicate specific variables you want to add from each new file, to avoid duplicating variables in the merge.
################################################################################################################
## ADDENDUM 1: 'childcare_exp','childcare_exp','childcare_exp','marital_status','family_size'
d_add <- fread(paste0(in_dir, '/J269055.csv'))
update_vars <- full_cb[var_rename %in% c('childcare_exp','healthcare_exp','education_exp','housing_exp','marital_status','family_size'), var]
d <- merge(d, d_add[, c('ER30001','ER30002',update_vars), with=F], by=c('ER30001','ER30002'))
## ADDENDUM 2: 'fam_income'
d_add <- fread(paste0(in_dir, '/J268860.csv'))
update_vars <- full_cb[var_rename %in% c('fam_income'), var]
d <- merge(d, d_add[, c('ER30001','ER30002',update_vars), with=F], by=c('ER30001','ER30002'))
## ADDENDUM 3: 'fam_wealth_noeq'
d_add <- fread(paste0(in_dir, '/J268894.csv'))
update_vars <- full_cb[var_rename %in% c('fam_wealth_noeq'), var]
d <- merge(d, d_add[, c('ER30001','ER30002',update_vars), with=F], by=c('ER30001','ER30002'))
################################################################################################################

## EXTRACT ALL
all <- rbindlist(lapply(main_years, pull_main_wave), fill=T)
## Adjust income for inflation and wealth for inflation + family size
## 2019 CPI https://cpiinflationcalculator.com/historical-tables/
## With equity
all[, fam_wealth_weq := as.double(fam_wealth_weq)]
all[year==1994, fam_wealth_weq := fam_wealth_weq*160.5/244.979]
all[year==1999, fam_wealth_weq := fam_wealth_weq*166.6/244.979]
all[year==2001, fam_wealth_weq := fam_wealth_weq*177.1/244.979]
all[year==2007, fam_wealth_weq := fam_wealth_weq*207.342/244.979]
all[year==2009, fam_wealth_weq := fam_wealth_weq*214.537/244.979]
all[year==2011, fam_wealth_weq := fam_wealth_weq*224.939/244.979]
all[year==2013, fam_wealth_weq := fam_wealth_weq*232.957/244.979]
all[year==2015, fam_wealth_weq := fam_wealth_weq*236.525/244.979]
all[year==2017, fam_wealth_weq := fam_wealth_weq*244.979/244.979]
all[, fam_wealth_weq := fam_wealth_weq/(family_size^0.55)]
## Without equity
all[, fam_wealth_noeq := as.double(fam_wealth_noeq)]
all[year==1994, fam_wealth_noeq := fam_wealth_noeq*160.5/244.979]
all[year==1999, fam_wealth_noeq := fam_wealth_noeq*166.6/244.979]
all[year==2001, fam_wealth_noeq := fam_wealth_noeq*177.1/244.979]
all[year==2007, fam_wealth_noeq := fam_wealth_noeq*207.342/244.979]
all[year==2009, fam_wealth_noeq := fam_wealth_noeq*214.537/244.979]
all[year==2011, fam_wealth_noeq := fam_wealth_noeq*224.939/244.979]
all[year==2013, fam_wealth_noeq := fam_wealth_noeq*232.957/244.979]
all[year==2015, fam_wealth_noeq := fam_wealth_noeq*236.525/244.979]
all[year==2017, fam_wealth_noeq := fam_wealth_noeq*244.979/244.979]
all[, fam_wealth_noeq := fam_wealth_noeq/(family_size^0.55)]
## Family income
all[, fam_income := as.double(fam_income)]
all[year==1994, fam_income := fam_income*160.5/244.979]
all[year==1999, fam_income := fam_income*166.6/244.979]
all[year==2001, fam_income := fam_income*177.1/244.979]
all[year==2007, fam_income := fam_income*207.342/244.979]
all[year==2009, fam_income := fam_income*214.537/244.979]
all[year==2011, fam_income := fam_income*224.939/244.979]
all[year==2013, fam_income := fam_income*232.957/244.979]
all[year==2015, fam_income := fam_income*236.525/244.979]
all[year==2017, fam_income := fam_income*244.979/244.979]
## SAVE CLEAN DATASET
write.csv(all, paste0(out_dir, 'individual_family2.csv'), row.names = F)

