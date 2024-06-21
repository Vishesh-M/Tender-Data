# ~~~~~~~~~~~~~~~~~~~~~~
# Clear Environment ####
# ~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~
# 1 - Import Packages ####
# ~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(reshape2)
library(fuzzyjoin)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2 - Define Parameters ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
startDate <- ymd('2014-01-01')
endDate <- ymd('2019-11-13')
rep_period <- interval(startDate, endDate)
options(scipen = 999) # disable scientific notation

# schemeList <- c('G3201', 'G3286', 'G3287', 'G3340', 'G3345') # Qantas
 #schemeList <- c('G1303','G1304','G1188')#,'G0316') # NABGSF

# SPS250 schemes ( ** CHECK IF NULIS IS REQUIRED **)
#schemeList <- c('G3361','G3399','G3348','G3434','G1160','G1162','G1161','G1163','G3331','G3432','G3201','G3284','G3285','G3286','G3287','G3340','G3345','G3218','G3219','G3112','G3461','G2935','G2986','G3439','G1140','G1141','G0721','G3502','G3484','G3485','G3486','G3487','G3488','G3489','G3490','G3495','G3496','G3600','G3608','G3431','G3410','G3375','G0389','G0387','G3149','G3148','G3421','G3330')


# ~~~~~~~~~~~~~~~~~~~~
# 3 - Data Import ####
# ~~~~~~~~~~~~~~~~~~~~
cv <- read_csv('inputs/cv_benefits.csv', guess_max = 100000)
report_run_date <- dmy(str_match(cv[nrow(cv)-1,1], '.* (\\d+/\\d+/\\d+)')[2])
cv <- cv %>% head(-5)

cv_claimants <- read_csv('Inputs/cv_claimants.csv', guess_max = 100000) %>% head(-5)
cv_policies <- read_csv('Inputs/cv_policies.csv', guess_max = 100000) %>% head(-5)
cv_schemes <- read_csv('Inputs/cv_schemes.csv', guess_max = 100000) %>% head(-5)
cv_pol_changes <- read_csv('Inputs/group_policies_changes.csv', guess_max = 10000)

cv_payments <- read_csv('Inputs/cv_payments.csv', guess_max = 100000) %>% head(-5)
cv_payments_contr <- read_csv('inputs/cv_payment_contributions.csv', guess_max = 100000) %>% head(-5)
cv_cases_payments <- read_csv('inputs/cases plus payments plus contributions.csv', guess_max = 100000) %>% head(-5)

cv_diagnosis <- read_csv('inputs/cv_diagnosis.csv', guess_max = 1000000)


# ~~~~~~~~~~~~~~~~~~~~
# 4 - CV Payments ####
# ~~~~~~~~~~~~~~~~~~~~
date_cols_payments <- c('Payment Processed Date', 'Start', 'Through', 'Created Date')
valid_status_payments <- c('Processed', 'Approved', 'Released')

cv_payments_clean <- cv_payments_contr %>% 
  # join info from payments
  left_join(cv_payments %>% 
              select(`Payment Name`, Status, `Payment Processed Date`, `Paid Start`, `Paid Through`, `Payment Date: From`, `Payment Date: To`, `Created Date`) %>% 
              rename(PaymentProcessedDateAux = `Payment Processed Date`), 
            by=c('Payment: Payment Name' = 'Payment Name')) %>% 
  # add in decisiondate/notifiedDate to supplement blank processed dates
  left_join(cv %>% 
              select(`Benefit Claimed: Benefit Claimed ID`, `Date of Notification`, `Initial Decision`),
            by = c('Benefit Claimed ID' = 'Benefit Claimed: Benefit Claimed ID')) %>% 
  # clean data
  mutate(Start = coalesce(Start, `Payment Date: From`),
         Through = coalesce(Through, `Payment Date: To`),
         # for linked claims, sometimes Payment Processed Date is blank, using Created date as proxy
         `Payment Processed Date` = coalesce(`Payment Processed Date`, PaymentProcessedDateAux, `Initial Decision`, `Date of Notification`)) %>% 
  # data types
  mutate_at(vars(date_cols_payments), funs(dmy)) %>% 
  # scope
  filter(Status %in% valid_status_payments,
         # Only consider payments after cut-over to be added to FLARE
         `Payment Processed Date` %within% interval(ymd('2019-06-08'), endDate)) %>% 
  group_by(`Benefit Claimed ID`) %>% 
  summarise(AmountPaid = sum(Amount),
            FirstEffPaymentCV = min(Start, na.rm = T),
            LastEffPaymentCV = max(Through, na.rm = T),
            LastProcessedDate = max(`Payment Processed Date`))

# ~~~~~~~~~~~~~~~~~~
# 5 - Diagnosis ####
# ~~~~~~~~~~~~~~~~~~
cv_diagnosis <- cv_diagnosis %>% 
  mutate_at(vars('Start Date of Diagnosis', 'Created Date'), funs(dmy)) %>% 
  filter(Type %in% c('Primary', 'Cause of Death')) %>% 
  mutate(aux = coalesce(`Start Date of Diagnosis`, `Created Date`),
         Cause = coalesce(`Medical Code`, `Medical Code Description`)) %>% 
  group_by(`Case Number`) %>% 
  slice(which.max(aux)) %>% 
  ungroup() %>% 
  select(`Case Number`, Cause)

# ~~~~~~~~~~~~~~~~~~~~~
# 6 - Benefits (1) ####
# ~~~~~~~~~~~~~~~~~~~~~
# *6.1 - Parameters ####
names(cv_claimants) <- c('MLC Case: Case Number', 'Salutation', 'First Name', 'Middle Name', 'Last Name', 'BirthDate', 'Gender')
names(cv_policies) <- c('MLC Case: Case Number', 'Policy', 'Scheme', 'SuperFundMemb', 'LegacyPASMemb', 'DateJoinedFund')

undetermined_status <- c('Pending',
                         'Under Assessment', 
                         'Re-insurer Approval Required', 
                         'Submitted for Approval', 
                         'Trustee Referral Required', 
                         'Trustee Referral Completed',
                         'Approval Rejected')

invalid_status_benefits <- c() 
invalid_closed_reasons <- c('Not a Legitimate Benefit', 'Duplicate Claim in the Administration System')
date_cols <- c('Initial Decision', 'Date of Notification', 'Date of Event', 'MLC Case: Created Date', 
               'End of Benefit Period', 'Effective Date of Claim Closure')

# *6.2 - First Scoping ####
cv <- cv %>% 
  rename('BCID' = `Benefit Claimed: Benefit Claimed ID`) %>% 
  filter(`Business Type` == 'Group') 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7 - CV Payments ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Using CV as source of truth for Payments
cv <- cv %>%
  left_join(cv_payments_clean, by=c('BCID' = 'Benefit Claimed ID'))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8 - Adhoc/Finance Payments ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filepath <- 'inputs/external payments datasets/JUL-AUG-SEP Adhoc & Finance payments with BCs.xlsx'

jul_aug_adhoc <- read_excel(filepath, sheet='July & Aug ADHOC', guess_max = 100000)
jul_aug_fin <- read_excel(filepath, sheet='July & Aug FINANCE', guess_max = 100000)
sep_adhoc <- read_excel(filepath, sheet='Sept ADHOC', guess_max = 100000)
sep_fin <- read_excel(filepath, sheet='Sept Finance', guess_max = 100000)

# *8.1 - Check BC links ####
jul_aug_adhoc_bcmis <- jul_aug_adhoc %>% 
  mutate(`Benefit Claimed ID` = str_remove_all(`Benefit Claimed ID`, '\\s')) %>% 
  filter(!`Benefit Claimed ID` %in% cv$BCID,
         !is.na(`Benefit Claimed ID`), !`Benefit Claimed ID` %in% c('N/A', 'NA'))

jul_aug_fin_bcmis <- jul_aug_fin %>% 
  mutate(`BC Number (Finance Payments)` = str_remove_all(`BC Number (Finance Payments)`, '\\s')) %>% 
  filter(!`BC Number (Finance Payments)` %in% cv$BCID,
         !is.na(`BC Number (Finance Payments)`), !`BC Number (Finance Payments)` %in% c('N/A', 'NA'))

sep_adhoc_bcmis <- sep_adhoc %>% 
  mutate(`Benefit Claimed ID` = str_remove_all(`Benefit Claimed ID`, '\\s')) %>% 
  filter(!`Benefit Claimed ID` %in% cv$BCID,
         !is.na(`Benefit Claimed ID`), !`Benefit Claimed ID` %in% c('N/A', 'NA'))

sep_fin_bcmis <- sep_fin %>% 
  mutate(`Benefit Claimed ID` = str_remove_all(`Benefit Claimed ID`, '\\s')) %>% 
  filter(!`Benefit Claimed ID` %in% cv$BCID,
         !is.na(`Benefit Claimed ID`), !`Benefit Claimed ID` %in% c('N/A', 'NA'))

# *8.2 - Save exceptions ####
# remove files
do.call(file.remove, list(list.files('intermediate exceptions/', full.names = TRUE)))

# save
if (nrow(jul_aug_adhoc_bcmis) > 0) write_csv(jul_aug_adhoc_bcmis, 'intermediate exceptions/jul_aug_adhoc_bcmis.csv', na='')
if (nrow(jul_aug_fin_bcmis) > 0) write_csv(jul_aug_fin_bcmis, 'intermediate exceptions/jul_aug_fin_bcmis.csv', na='')
if (nrow(sep_adhoc_bcmis) > 0) write_csv(sep_adhoc_bcmis, 'intermediate exceptions/sep_adhoc_bcmis.csv', na='')
if (nrow(sep_fin_bcmis) > 0) write_csv(sep_fin_bcmis, 'intermediate exceptions/sep_fin_bcmis.csv', na='')

# 8.3 - Merge manual payments ####
cv <- cv %>% 
  # July & August ADHOC
  left_join(jul_aug_adhoc %>% 
              mutate(Start = case_when(grepl('^\\d+$', Start) ~ as.Date(as.numeric(Start), origin = "1899-12-30"),
                                       TRUE ~ dmy(as.character(Start))),
                     Through = case_when(grepl('^\\d+$', Through) ~ as.Date(as.numeric(Through), origin = "1899-12-30"),
                                         TRUE ~ dmy(as.character(Through)))) %>% 
              group_by(`Benefit Claimed ID`) %>% 
              summarise(total_gross = sum(GROSS, na.rm=TRUE),
                        first_start = min(Start, na.rm=TRUE),
                        last_through = max(Through, na.rm=TRUE)) %>% 
              select(BCID = `Benefit Claimed ID`, jul_adhoc_gross = total_gross, 
                     jul_adhoc_start = first_start, jul_adhoc_through = last_through),
            by = c('BCID')) %>% 
  # July & August FINANCE
  left_join(jul_aug_fin %>% 
              mutate(Start = case_when(grepl('^\\d+$', Start) ~ as.Date(as.numeric(Start), origin = "1899-12-30"),
                                       TRUE ~ dmy(as.character(Start))),
                     Through = case_when(grepl('^\\d+$', Through) ~ as.Date(as.numeric(Through), origin = "1899-12-30"),
                                         TRUE ~ dmy(as.character(Through)))) %>% 
              group_by(`BC Number (Finance Payments)`) %>% 
              summarise(total_gross = sum(GROSS, na.rm=TRUE),
                        first_start = min(Start, na.rm=TRUE),
                        last_through = max(Through, na.rm=TRUE)) %>% 
              select(BCID = `BC Number (Finance Payments)`, jul_fin_gross = total_gross, 
                     jul_fin_start = first_start, jul_fin_through = last_through),
            by = c('BCID')) %>% 
  # September ADHOC
  left_join(sep_adhoc %>% 
              mutate(Start = `Payment Start`, Through = `Payment End`,
                     Start = case_when(grepl('^\\d+$', Start) ~ as.Date(as.numeric(Start), origin = "1899-12-30"),
                                       TRUE ~ dmy(as.character(Start))),
                     Through = case_when(grepl('^\\d+$', Through) ~ as.Date(as.numeric(Through), origin = "1899-12-30"),
                                         TRUE ~ dmy(as.character(Through)))) %>% 
              group_by(`Benefit Claimed ID`) %>% 
              summarise(total_gross = sum(`Gross Amount`, na.rm=TRUE),
                        first_start = min(Start, na.rm=TRUE),
                        last_through = max(Through, na.rm=TRUE)) %>% 
              select(BCID = `Benefit Claimed ID`, sep_adhoc_gross = total_gross, 
                     sep_adhoc_start = first_start, sep_adhoc_through = last_through),
            by = c('BCID')) %>% 
  # September FINANCE
  left_join(sep_fin %>% 
              mutate(Start = `Payment Start`, Through = `Payment End`,
                     Start = case_when(grepl('^\\d+$', Start) ~ as.Date(as.numeric(Start), origin = "1899-12-30"),
                                       TRUE ~ dmy(as.character(Start))),
                     Through = case_when(grepl('^\\d+$', Through) ~ as.Date(as.numeric(Through), origin = "1899-12-30"),
                                         TRUE ~ dmy(as.character(Through)))) %>% 
              group_by(`Benefit Claimed ID`) %>% 
              summarise(total_gross = sum(`Gross Amount`, na.rm=TRUE),
                        first_start = min(Start, na.rm=TRUE),
                        last_through = max(Through, na.rm=TRUE)) %>% 
              select(BCID = `Benefit Claimed ID`, sep_fin_gross = total_gross, 
                     sep_fin_start = first_start, sep_fin_through = last_through),
            by = c('BCID')) %>% 
  # Combine all payments
  mutate(AmountPaid = replace_na(AmountPaid, 0) + 
           replace_na(jul_adhoc_gross, 0) + replace_na(jul_fin_gross, 0) +
           replace_na(sep_adhoc_gross, 0) + replace_na(sep_fin_gross, 0),
         FirstEffPaymentCV = pmin(FirstEffPaymentCV, jul_adhoc_start, jul_fin_start, sep_adhoc_start, sep_fin_start, na.rm = T),
         LastEffPaymentCV = pmax(LastEffPaymentCV, jul_adhoc_through, jul_fin_through, sep_adhoc_through, sep_fin_through, na.rm = T))


# ~~~~~~~~~~~~~~~~~~~~~
# 9 - Benefits (2) ####
# ~~~~~~~~~~~~~~~~~~~~~
# *9.1 - Descoped BCs with Payments ####
# ~~~~~~~~~~~ (returns all BCs that will get excluded but have payments)
# ~~~~~~~~~~~ Need to reassign the payments to the proper BC (manual spreadsheet linking them)
exceptions_excl_payments <- cv %>% 
  # selects BCs that will be excluded that have payments associated to them
  filter(`Benefit Claimed Status` %in% invalid_status_benefits |
           `Closed Reason` %in% invalid_closed_reasons,
         AmountPaid > 0)

# save exceptions
if (nrow(exceptions_excl_payments) > 0) {
  exceptions_excl_payments %>% 
    select(`MLC Case: Case Number`, BCID, `Benefit: Benefit Name`, AmountPaid, `Benefit Claimed Status`, `Closed Reason`) %>% 
  write_csv('intermediate exceptions/descoped_bcs_with_payment.csv', na='')
}

# *9.2 - Main merges ####
cv <- cv %>% 
  # converts data types
  mutate_at(vars(date_cols), funs(dmy)) %>% 
  # adjusts columns for reconciliation
  mutate(APRAclaimType = case_when(grepl('Income Protection', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'DII',
                                   grepl('Business Expense', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Business Expense',
                                   grepl('Terminal Illness', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Death',
                                   grepl('Critical Illness', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Critical Illness',
                                   grepl('Total and Permanent Disability', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'TPD',
                                   grepl('Involuntary Unemployment', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Involuntary Unemployment',
                                   grepl('Disablement', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'TPD',
                                   grepl('Death Benefit', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Death',
                                   grepl('Life Cover', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Death',
                                   grepl('Death - MLCG', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Death',
                                   grepl('Accidental Death', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Accidental Death',
                                   grepl('Accidental Injury', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Accidental Injury',
                                   grepl('Premium Waiver', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Premium Waiver',
                                   grepl('Super Contribution', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Super Contribution',
                                   grepl('Hospital', `Benefit: Benefit Name`, ignore.case = TRUE) ~ 'Hospital Benefit',
                                   TRUE ~ `Benefit: Benefit Name`),
         `Benefit Amount` = replace_na(`Benefit Amount`, 0),
         BenefitStatusCV = case_when(`Benefit Claimed Status` != 'Closed' ~ 'Open',
                                     TRUE ~ `Benefit Claimed Status`),
         DateReported = `Date of Notification`,
         
         # flag to identify whether the benefit is 'core' or 'additional'
         f_add = case_when(grepl('Additional', `Benefit: Benefit Name`) ~ 'Additional',
                           TRUE ~ 'Core'),
         
         # key used to merge the Super Contributions
         sc_key = sprintf('%s-%s-%s-%s', `MLC Case: Case Number`, `Benefit: Benefit Category`, `Date of Event`, f_add),
         
         # key used to merge Additional Benefits (after SC were merged)
         add_key = sprintf('%s-%s-%s', `MLC Case: Case Number`, `Benefit: Benefit Category`, `Date of Event`),
         
         reportrundate = report_run_date,
         
         # roll back claims with decision after endDate
         `Benefit Claimed Status` = case_when(!is.na(`Initial Decision`) & `Initial Decision` > endDate ~ 'Pending',
                                              TRUE ~ `Benefit Claimed Status`),
         `Closed Reason` = case_when(!is.na(`Initial Decision`) & `Initial Decision` > endDate ~ NA_character_,
                                     TRUE ~ `Closed Reason`),
         `Closed Reason Category` = case_when(!is.na(`Initial Decision`) & `Initial Decision` > endDate ~ NA_character_,
                                              TRUE ~ `Closed Reason Category`),
         `Initial Decision` = case_when(!is.na(`Initial Decision`) & `Initial Decision` > endDate ~ ymd(NA),
                                        TRUE ~ `Initial Decision`),
         # BC Outcome
         Outcome = case_when(`Benefit Claimed Status` == 'Approved' ~ 'Admit',
                             `Benefit Claimed Status` == 'Denied' ~ 'Decline',
                             `Benefit Claimed Status` %in% undetermined_status ~ 'Under Assessment',
                             `Benefit Claimed Status` == 'Closed' ~ recode(`Closed Reason Category`, 
                                                                           'None' = 'Withdraw',
                                                                           'Approved' = 'Admit',
                                                                           'Denied' = 'Decline'),
                             
                             TRUE ~ 'Needs Investigation')) %>% 
  # identify possible duplicates
  group_by(Policy, APRAclaimType) %>% 
  mutate(PossibleDuplicate = n() > 1,
         dupl_cases = paste(`MLC Case: Case Number`, collapse = ',')) %>% 
  ungroup() %>% 
  # merge extra data
  left_join(cv_claimants,
            by = c('MLC Case: Case Number')) %>% 
  mutate_at(vars('BirthDate'), funs(dmy)) %>% 
  left_join(cv_policies %>% 
              select(-Policy),
            by = c('MLC Case: Case Number')) %>% 
  mutate_at(vars('DateJoinedFund'), funs(dmy)) %>% 
  left_join(cv_schemes %>% 
              distinct(`Scheme: Scheme Number`, .keep_all = TRUE),
            by = c('Scheme' = 'Scheme: Scheme Number')) %>% 
  left_join(cv_diagnosis, by=c('MLC Case: Case Number' = 'Case Number')) %>% 
  mutate_at(vars('First Name', 'Last Name'),
            funs(tolower)) %>% 
  rename('Legacy Claim Number' = LegacyPASMemb,
         'Benefit Closure Date' = `Effective Date of Claim Closure`)


# ~~~~~~~~~~~~~~~~~~~
# 10 - SC Rollup ####
# ~~~~~~~~~~~~~~~~~~~
# PV: This round of exceptions is separate than the rest, since it will affect the Super Contribution
#     and Additional Benefits merge rule. If this exception is NOT NULL, GI Claims should have the option
#     to fix them before looking at the claims-specific data fields exceptions. 
#     For Example: 
#       Income Protection BC  : $1 SI / Date of Notification = 01/02/2019 / DOE = 01/01/2019 / Initial Decition =  01/07/2019 / Outcome = 'Admitted'
#       Additional IP BC    : $1000 SI / Date of Notification = 15/03/2019 / DOE = 01/01/2019 / Initial Decition =  01/07/2019 / Outcome = 'Admitted'
#       
#       The merging logic will use the Core Benefit as the base, and sum their SI and AP, meaning that, if the correct
#       record is the Additional Benefit (Date of Notification = 15/03/2019), this information would be lost.
#       By adding the exception "<Additional Benefit, Core Benefit> pair with $1 Sum Insured", this possible scenario
#       will become clear in the dataset, and GI Claims can properly check the case before checking any other 
#       exceptions involving the other data fields (since they coudl have been sourced from the wrong BC).

# *10.1 - SC/Additional Exceptions ####
cv <- cv %>% 
  # identify all <Benefit/SC> or <AddBenefit/AddSC> pairs with different DOE
  group_by(sc_key) %>% 
  mutate(f_sc_doe_misaligned = (n() == 1 &
                                  grepl('Super Contribution', `Benefit: Benefit Name`) &
                                  `Benefit: Benefit Category` == 'Group GSC')) %>% 
  # identify claims with same SI
  group_by(`MLC Case: Case Number`, `Benefit Amount`) %>% 
  mutate(f_same_si = n() > 1) %>% 
  # identify <Benefit/AddBenefit> where one of them has $1 SI
  group_by(`MLC Case: Case Number`, `Date of Event`) %>% 
  mutate(f_core_add_1SI = case_when(length(`Benefit Amount`[!grepl('Super Contribution', `Benefit: Benefit Name`) & 
                                                              `Benefit Amount` == 1]) > 0 &
                                      length(`Benefit Amount`[!grepl('Super Contribution', `Benefit: Benefit Name`) & 
                                                                `Benefit Amount` != 1]) > 0 ~ TRUE,
                                    TRUE ~ FALSE)) %>% 
  ungroup() %>% 
  mutate(SC_Additional_Exceptions = character(1)) %>% 
  mutate(SC_Additional_Exceptions = case_when(f_sc_doe_misaligned ~ paste0(SC_Additional_Exceptions, 'SC DoE misaligned with Core/Additional Benefit - '),
                                              TRUE ~ SC_Additional_Exceptions),
         SC_Additional_Exceptions = case_when(f_same_si ~ paste0(SC_Additional_Exceptions, 'Multiple BCs with the same Benefit Amount in the same Case - '),
                                              TRUE ~ SC_Additional_Exceptions),
         SC_Additional_Exceptions = case_when(f_core_add_1SI ~ paste0(SC_Additional_Exceptions, '<Additional Benefit, Core Benefit> pair with $1 Sum Insured - '),
                                              TRUE ~ SC_Additional_Exceptions)) 

# *10.2 - Rollup ####
cv <- cv %>% 
  # remove all Super Contribution (including Additional SC)
  filter(!grepl('Super Contribution', `Benefit: Benefit Name`)) %>% 
  # merge back the SC -> Core Benefit
  left_join(
    cv %>%
      filter(grepl('^Super Contribution', `Benefit: Benefit Name`)) %>%
      rename(BenefitSC = `Benefit Amount`, AmountPaidSC = AmountPaid, SumInsuredSC = `Sum Insured`) %>%
      # distinct prevents rows to be duplicated when a case has multiple GSC + SC pairs
      distinct(`MLC Case: Case Number`, .keep_all = TRUE) %>%
      select(sc_key, BenefitSC, AmountPaidSC, SumInsuredSC),
    by=c('sc_key')
  ) %>% 
  mutate(#store original values for recon purposes,
         BenefitAmountCore = `Benefit Amount`,
         SumInsuredCore = `Sum Insured`,
         AmountCore = AmountPaid,
         # consolidate SC amounts
         BenefitAmoutCoreSC = replace_na(`Benefit Amount`, 0) + replace_na(BenefitSC, 0),
         SumInsuredCoreSC = replace_na(`Sum Insured`, 0) + replace_na(SumInsuredSC, 0),
         AmountPaidCoreSC = replace_na(AmountPaid, 0) + replace_na(AmountPaidSC, 0)) %>% 
  # merge back the AdditionalSC -> AdditionalBenefit
  left_join(
    cv %>%
      filter(grepl('^Additional Super Contribution', `Benefit: Benefit Name`)) %>%
      rename(BenefitAddSC = `Benefit Amount`, AmountPaidAddSC = AmountPaid, SumInsuredAddSC = `Sum Insured`) %>%
      # distinct prevents rows to be duplicated when a case has multiple GSC + SC pairs
      distinct(`MLC Case: Case Number`, .keep_all = TRUE) %>%
      select(sc_key, BenefitAddSC, AmountPaidAddSC, SumInsuredAddSC),
    by=c('sc_key')
  ) %>% 
  mutate(#store original values for recon purposes,
         BenefitAmountCore = `Benefit Amount`,
         SumInsuredCore = `Sum Insured`,
         AmountCore = AmountPaid,
         # consolidate SC amounts
         BenefitAmoutCoreSC = replace_na(BenefitAmoutCoreSC, 0) + replace_na(BenefitAddSC, 0),
         SumInsuredCoreSC = replace_na(SumInsuredCoreSC, 0) + replace_na(SumInsuredAddSC, 0),
         AmountPaidCoreSC = replace_na(AmountPaidCoreSC, 0) + replace_na(AmountPaidAddSC, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 11 - Additional Benefits Rollup ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cv <- cv %>% 
  group_by(`MLC Case: Case Number`, add_key, Outcome, `Initial Decision`) %>% 
  # removes the Additional Benefits that have the same Outcome as the core benefits
  filter(!(f_add == 'Additional' & 
             n_distinct(f_add) > 1 &
             Outcome %in% c('Admit', 'Decline', 'Withdraw')) # don't merge Under Assessment and claims that the Outcome needs Investigation
         ) %>% 
  ungroup() %>% 
  # merge them back
  fuzzy_left_join(cv %>% 
                    group_by(`MLC Case: Case Number`, add_key, Outcome, `Initial Decision`) %>% 
                    filter(f_add == 'Additional' & 
                             n_distinct(f_add) > 1 &
                             Outcome %in% c('Admit', 'Decline', 'Withdraw')) %>% 
                    ungroup() %>% 
                    select(add_key, OutcomeAdd = Outcome, f_add, BenefitAmountAdd = `Benefit Amount`, SumInsuredAdd = `Sum Insured`, 
                           AmountPaidAdd = AmountPaid),
                  by=c('add_key', 'Outcome' = 'OutcomeAdd', 'f_add'),
                  match_fun = list(`==`, `==`, `!=`)) %>% 
  # sum the SI / Payments from Addition back to core benefit
  mutate(BenefitAmountTotal = replace_na(BenefitAmoutCoreSC, 0) + replace_na(BenefitAmountAdd, 0),
         SumInsuredTotal = replace_na(SumInsuredCoreSC, 0) + replace_na(SumInsuredAdd, 0),
         AmountPaidTotal = replace_na(AmountPaidCoreSC, 0) + replace_na(AmountPaidAdd, 0))

uat_add_merge <- cv %>% 
  group_by(`MLC Case: Case Number`, `Benefit: Benefit Category`) %>% 
  select(add_key.x, BCID, `Benefit: Benefit Name`, `Benefit Amount`, BenefitAmountAdd, BenefitAmountTotal, `Benefit Claimed Status`, `Closed Reason`, Outcome, OutcomeAdd, SC_Additional_Exceptions, everything()) %>% 
  arrange(add_key.x) %>% 
  filter(BenefitAmountAdd > 0)

# ~~~~~~~~~~~~~~~~~~~~~~
# 12 - Last Scoping ####
# ~~~~~~~~~~~~~~~~~~~~~~
cv <- cv %>% 
  filter(`Case Type` == 'Claim',
         `Case Status` != 'Incomplete',
         `Date of Notification` <= endDate)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 13 - Save Cleaned Files ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(cv, 'Outputs/clean CV.csv', na = '')
saveRDS(cv, 'Outputs/clean CV.rds')
