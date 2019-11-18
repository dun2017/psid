rel <- fread("//sas/psc/dept/cboen_Proj/PSID/MX17REL/mx17rel.csv")
rel_cb <- as.data.table(readxl::read_excel("//sas/psc/dept/cboen_Proj/PSID/MX17REL/mx17_cb.xlsx"))

for(v in rel_cb[, var]) {
  v <- gsub(' ','',v)
  ## Keep codebook where we are using this raw variable.
  raw_var_cb <- rel_cb[var==v, ]
  ## In case this raw variable is being used to construct multiple clean variables.
  raw_var_cb[, n := 1:.N] 
  ## Loop over and process each clean variable constructured using this raw variable.
  for(i in raw_var_cb[, n]) {
    cb <- raw_var_cb[n==i, ]
    all_vars <- v
    message(paste0('Recoding ', v, '...'))
    for(sv in all_vars) {
      this_rename <- cb[var==v, var_rename]
      rel[, (this_rename) := as.character(get(v))]
      for(val in 1:12) {
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
          rel[get(sv) %in% this_val, (this_rename) := this_recode] 
        }
      }
      ## After recoding, make numeric if numeric specified.
      if(!is.na(cb[var==v, numeric])) rel[, (var_rename) := as.numeric(as.character(get(sv)))]
    }
    ## Sum over multiple columns if needed (scales variables).
    # if(!is.na(cb[var==v, sum_values])) rel[, (var_rename) := rowSums(.SD, na.rm = TRUE), .SDcols = all_vars] 
  }
}

rel <- rel[, c(rel_cb[, var_rename]), with=FALSE]
write.csv(rel, "//sas/psc/dept/cboen_Proj/PSID/MX17REL/clean_relations.csv", row.names = F)

