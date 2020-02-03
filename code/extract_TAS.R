library(haven)
library(data.table)
library(ggplot2)
library(naniar)
library(mice)
library(survey)

## Function to apply codebook and keep only those clean extracted variables.
pull_tas_wave <- function(y, cb) {
  message(paste0('Extracting ', y, '...'))
  ## Load full data and codebook.
  full_cb <- as.data.table(readxl::read_excel('//sas/psc/dept/cboen_Proj/Hannah/code/psid_tas_cb_v2_2019-09-16_heo.xlsx',
                                         sheet = as.character(y))) 
  d <- fread(paste0('//sas/psc/dept/cboen_Proj/Hannah/Data/ta',y,'/tas',gsub('20','',as.character(y)),'.csv'))
  ## Recode any raw variables that need recoding.
  for(v in full_cb[, var]) {
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
        for(val in 1:11) {
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
          this_rename <- cb[var==v, var_rename]
          d[, (this_rename) := as.character(get(v))]
          if(!is.na(this_val[1])) {
            d[get(sv) %in% this_val, (this_rename) := this_recode] 
          }
        }
        ## After recoding, make numeric if numeric specified.
        if(!is.na(cb[var==v, numeric])) d[, (var_rename) := as.numeric(as.character(get(sv)))]
      }
      ## Sum over multiple columns if needed (scales variables).
      if(!is.na(cb[var==v, sum_value])) d[, (var_rename) := rowSums(.SD, na.rm = TRUE), .SDcols = all_vars] 
    }
  }
  ## Return cleaned dataset.
  d[, year := y]
  d <- d[, c('year',full_cb[, var_rename]), with=FALSE]
  return(d)
}

## Extract each year of TAS survey.
tas_years <- c(2005,2007,2009,2011,2013,2015,2017)
all <- rbindlist(lapply(tas_years, pull_tas_wave), fill=T)
write.csv(all, '//sas/psc/dept/cboen_Proj/Hannah/Data/clean_TAS.csv', row.names = F)


