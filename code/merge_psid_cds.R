library(haven)
library(data.table)
library(ggplot2)

# ## Load CDS and master PSID file.
# psid <- read_dta("//sas/psc/dept/cboen_Proj/PSID/PSID_workingdata_long.dta")
# psid <- as.data.table(psid)
# cb <- fread("//sas/psc/dept/cboen_Proj/PSID/CDS/codebook.csv")
# 
# extract_cds <- function(y) {
#   
#   ## Extract both "child" and "assessment" files from CDS, merge together.
#   extract_file <- function(f) {
#     cds <- fread(paste0("//sas/psc/dept/cboen_Proj/PSID/CDS/CDS_", y, "_", f, ".csv"))
#     ## Apply codebook renames and keep only relevant variables.
#     all_vars <- cb[year==y & file==f, var]
#     cds <- cds[, all_vars, with=F]
#     for(v in all_vars) setnames(cds, v, cb[var==v, rename])
#     return(cds)
#   }
#   cds_child <- extract_file('child')
#   cds_assess <- extract_file('assess')
#   cds <- merge(cds_child, cds_assess, by=c('childID_','famID_'))
#   
#   ## According to the doc, these are the years to match each of these files to family ids in the main file.
#   if(y==2002) cds[, year := 2001] 
#   if(y==2007) cds[, year := 2007]
#   
#   ## Merge PSID year-specific family information to CDS file.
#   message(paste0('Successfully merged ', round(sum(cds[, famID_] %in% psid[, famID_]) / length(cds[, famID_]), 2)*100, '% of ', length(cds[, famID_]),' records.'))
#   all <- merge(cds, psid, by=c('famID_','year'))
#   
#   return(all)
#   
# }
# 
# cds <- rbindlist(lapply(c(2002,2007), extract_cds), use.names = T, fill = F)
# write.csv(cds, '//sas/psc/dept/cboen_Proj/PSID/CDS/merged_data/psid_cds_merged.csv')

## Use CDS file with all years
psid <- read_dta("//sas/psc/dept/cboen_Proj/PSID/PSID_workingdata_long.dta")
psid <- as.data.table(psid)
psid <- psid[reltnhead==10, ] ## There are two records for each family-year. I'm restricting here to what seems to be the code for head of household.
cb <- fread("//sas/psc/dept/cboen_Proj/PSID/CDS/codebook.csv")
cb[year==1997, year := 1999] ## There is no 1997 family records in this master PSID file - only 1994 and 1999.
full_cds <- fread(paste0("//sas/psc/dept/cboen_Proj/PSID/CDS/CDS_1997_2002_2007.csv"))

extract_cds <- function(y) {
  
  ## Apply codebook renames and keep only relevant variables for this survey year.
  all_vars <- cb[year==y, var]
  cds <- full_cds[, all_vars, with=F]
  for(v in all_vars) setnames(cds, v, cb[var==v & year==y, rename])
  cds[, year := y]
  cds <- cds[!is.na(child_age), ]
  
  ## Merge PSID year-specific family information to CDS file.
  merged <- merge(cds, psid, by=c('famID_','year'))
  head(unique(cds[!(famID_ %in% merged$famID_), famID_]))
  message(paste0('Successfully merged ', round(length(merged[, famID_]) / length(cds[, famID_]), 2)*100, '% of ', length(cds[, famID_]),' records.'))
  return(merged)
  
}

cds <- rbindlist(lapply(c(1999,2001,2007), extract_cds), use.names = T, fill = T)
## SOME FIXES
## BMI missing is being coded to 99 apparently.
cds[bmi==99, bmi := NA]
## 0 is missing, 9 is too young (only covers ages 12+)
cds[psych_wellbeing %in% c(0,9), psych_wellbeing := NA] 
## 0 is missing, 9 is too young (only covers ages 9+)
cds[self_rated_health %in% c(0,9), self_rated_health := NA] 
## 0 is missing, 99 is too young (only covers ages 12+)
cds[depression %in% c(0,99), depression := NA] 
write.csv(cds, '//sas/psc/dept/cboen_Proj/PSID/CDS/merged_data/psid_cds_merged.csv')

## Descriptives
cds <- fread('//sas/psc/dept/cboen_Proj/PSID/CDS/merged_data/psid_cds_merged.csv')
## It's a square dataset, so drop missing child-years.
cds <- cds[!is.na(child_age), ]

## From Courtney:
## 1 figure showing trends in household wealth among the CDS sample from 1997 to 2005, including trend lines for the total sample and by race (Blacks and Whites only)
## In the other, trends in child health outcomes, again for the whole sample and by race.
## Use "wealth2i" (adjusted for inflation).
cds_wealth <- cds[, list(wealth=mean(wealth2i)), by=c('year')]
cds_wealth[, group := 'Total']
cds_wealth_race <- cds[, list(wealth=mean(wealth2i)), by=c('race','year')]
cds_wealth_race[race==1, group := 'White']
cds_wealth_race[race==2, group := 'Black']
cds_wealth <- rbind(cds_wealth[, c('year','group','wealth')], cds_wealth_race[, c('year','group','wealth')])
pdf('J:/cboen_Proj/PSID/CDS/results/abstract_figures_by_year.pdf', height=8, width=10)
fig1 <- ggplot() + 
  geom_line(data=cds_wealth,
            aes(x=year,
                y=wealth/1000,
                color=group),
            size=2) + 
  scale_color_manual(values=c('red','black','blue')) + 
  scale_x_continuous(breaks=c(1999, 2001, 2007)) + 
  labs(x='Year', y='Wealth (in thousands $)', color='Population') + 
  theme_bw()
print(fig1)
health_indicators <- c('bmi','self_rated_health','psych_wellbeing','depression')
for(i in health_indicators) {
if(i=='bmi') y_title <- 'Mean BMI'
if(i=='self_rated_health') y_title <- 'Mean score on self-rated health (1=Excellent, 5=Poor)'
if(i=='psych_wellbeing') y_title <- 'Mean count on inventory for psychological wellbeing'
if(i=='depression') y_title <- 'Mean count on Child Depression Inventory (18 items)'
i_total <- cds[, list(i=mean(get(i), na.rm=T)), by=c('year')]
i_total[, group := 'Total']
i_race <- cds[, list(i=mean(get(i), na.rm=T)), by=c('race','year')]
i_race[race==1, group := 'White']
i_race[race==2, group := 'Black']
i_total <- rbind(i_total[, c('year','group','i')], i_race[, c('year','group','i')])
fig <- ggplot() + 
  geom_line(data=i_total,
            aes(x=year,
                y=i,
                color=group),
            size=2) + 
  scale_color_manual(values=c('red','black','blue')) + 
  scale_x_continuous(breaks=c(1999, 2001, 2007)) + 
  labs(x='Year', y=y_title, color='Population') + 
  theme_bw()
print(fig)
}
dev.off()

cds[, child_age_years := child_age]
cds[year>1999, child_age_years := round(child_age/12)]
cds[child_age_years<5, age_group := 1]
cds[child_age_years>=5 & child_age_years<10, age_group := 2]
cds[child_age_years>=10 & child_age_years<15, age_group := 3]
cds[child_age_years>=15, age_group := 4]
cds_wealth <- cds[, list(wealth=mean(wealth2i)), by=c('age_group')]
cds_wealth[, group := 'Total']
cds_wealth_race <- cds[, list(wealth=mean(wealth2i)), by=c('race','age_group')]
cds_wealth_race[race==1, group := 'White']
cds_wealth_race[race==2, group := 'Black']
cds_wealth <- rbind(cds_wealth[, c('age_group','group','wealth')], cds_wealth_race[, c('age_group','group','wealth')])
pdf('J:/cboen_Proj/PSID/CDS/results/abstract_figures_by_child_age.pdf', height=8, width=10)
fig1 <- ggplot() + 
  geom_line(data=cds_wealth,
            aes(x=age_group,
                y=wealth/1000,
                color=group),
            size=2) + 
  scale_color_manual(values=c('red','black','blue')) + 
  scale_x_continuous(breaks=c(1,2,3,4), labels=c('0-4','5-9','10-14','15+')) + 
  labs(x='Child age', y='Wealth (in thousands $)', color='Population') + 
  theme_bw()
print(fig1)
health_indicators <- c('bmi','self_rated_health','psych_wellbeing','depression')
for(i in health_indicators) {
  if(i=='bmi') y_title <- 'Mean BMI'
  if(i=='self_rated_health') y_title <- 'Mean score on self-rated health (1=Excellent, 5=Poor)'
  if(i=='psych_wellbeing') y_title <- 'Mean count on inventory for psychological wellbeing'
  if(i=='depression') y_title <- 'Mean count on Child Depression Inventory (18 items)'
  i_total <- cds[, list(i=mean(get(i), na.rm=T)), by=c('age_group')]
  i_total[, group := 'Total']
  i_race <- cds[, list(i=mean(get(i), na.rm=T)), by=c('race','age_group')]
  i_race[race==1, group := 'White']
  i_race[race==2, group := 'Black']
  i_total <- rbind(i_total[, c('age_group','group','i')], i_race[, c('age_group','group','i')])
  if(i!='bmi') i_total <- i_total[age_group >= 3, ]
  fig <- ggplot() + 
    geom_line(data=i_total,
              aes(x=age_group,
                  y=i,
                  color=group),
              size=2) + 
    scale_color_manual(values=c('red','black','blue')) + 
    labs(x='Child age', y=y_title, color='Population') + 
    theme_bw()
  if(i=='bmi') fig <- fig + scale_x_continuous(breaks=c(1,2,3,4), labels=c('0-4','5-9','10-14','15+'))
  if(i!='bmi') fig <- fig + scale_x_continuous(breaks=c(3,4), labels=c('10-14','15+'))
  print(fig)
}
dev.off()



