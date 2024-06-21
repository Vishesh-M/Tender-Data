# ~~~~~~~~~~~~~~~~~~~~~~
# Clear Environment ####
# ~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 - Import Packages ####
# ~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(tidylog)
library(lubridate)
library(readxl)

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 - Set Parameters ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~
startDate <- ymd('2014-01-01')
endDate <- ymd('2019-11-13')
rep_period <- interval(startDate, endDate)

# schemeList <- c('G1303','G1304','G1188','G0316') # NABGSF
#schemeList <- c('G1303','G1304','G1188')#,'G0316')

# SPS250 schemes ( ** CHECK IF NULIS IS REQUIRED **)
#schemeList <- c('G3361','G3399','G3348','G3434','G1160','G1162','G1161','G1163','G3331','G3432','G3201','G3284','G3285','G3286','G3287','G3340','G3345','G3218','G3219','G3112','G3461','G2935','G2986','G3439','G1140','G1141','G0721','G3502','G3484','G3485','G3486','G3487','G3488','G3489','G3490','G3495','G3496','G3600','G3608','G3431','G3410','G3375','G0389','G0387','G3149','G3148','G3421','G3330')


#'G3601','G1284' were missing from initial scheme list


# ~~~~~~~~~~~~~~~~~~~~~~
# 3.0 - Data Import ####
# ~~~~~~~~~~~~~~~~~~~~~~
ocr.clean <- read_csv('outputs/ocr.clean.csv', guess_max = 100000)
cv <- read_csv('outputs/clean CV.csv', guess_max=100000)
keys <- read_excel('inputs/cv_compass_master_final.xlsx', sheet=1)

# cleaning keys
keys <- keys %>% 
  separate(KEY1, c('MemberNumber', 'ClaimType'), sep=',') %>% 
  mutate(BCID = coalesce(`NEW BC`, `KEY2 (BC)`),
         ClaimType = recode(ClaimType, 
                            'DTH' = 'Death with TI',
                            'SC/IP' = 'DII')) %>% 
  select(MemberNumber, ClaimType, BCID)

# cleaning CV
cv <- cv %>% 
  select(BCID, Scheme, `Scheme Name`, BirthDate, `Date of Event`, `Date of Notification`, DateReported, `Initial Decision`, `Waiting/Qualifying Units`, `Waiting/Qualifying Period`,
         `Benefit Claimed Status`, `Closed Reason`, BenefitStatusCV, BenefitAmountTotal, AmountPaidTotal, `First Name`, `Middle Name`, `Last Name`, Cause,
         `Case Type`, `Case Status`, APRAclaimType, `MLC Case: Created Date`, FirstEffPaymentCV, LastEffPaymentCV, LastProcessedDate, `Benefit Period`, `End of Benefit Period`, Gender,
         DateJoinedFund, reportrundate, `Benefit Closure Date`, `Closed Reason Category`, Outcome, `Legacy Claim Number`,
         SC_Additional_Exceptions)

reportrundate <- cv$reportrundate[1]


# ~~~~~~~~~~~~~~~~~~~~~~
# 4.0 - Integration ####
# ~~~~~~~~~~~~~~~~~~~~~~
# *4.1 - Treating cases where OCR is empty ####
if (nrow(ocr.clean) == 0) {
  num_cols <- c('MemberNumber', 'AmountPaid', 'SumInsured', 'WaitingPeriod')
  date_cols <- c('DateOfBirth', 'DateJoinedFund', 'EndofBenefitPeriod', 'ExpectedClosureDate', 'reportrundate', 'DateofClaim',
                 'DateNotified', 'DecisionDate', 'FinalisedDate', 'ProcessedDate', 'FirstEffPaymentDate', 'LastEffPaymentDate')
  
  ocr.clean <- ocr.clean %>% 
    mutate_at(vars(num_cols), funs(as.numeric)) %>% 
    mutate_at(vars(date_cols), funs(as.Date))
}

# *4.2 - Merging COMPASS and CV ####
ocr.clean.cv <- ocr.clean %>% 
  mutate_at(vars('MemberNumber'), funs(as.character)) %>% 
  # align WP with CV
  mutate(`Waiting/Qualifying Units` = 'Days') %>%
  rename(`Waiting/Qualifying Period` = WaitingPeriod) %>% 
  # join CV data
  left_join(keys, by=c('MemberNumber', 'ClaimType')) %>% 
  left_join(cv, by=c('BCID'))

ocr.clean.cv <- ocr.clean.cv %>% 
  mutate(SchemeNumber = coalesce(Scheme, as.character(SchemeNumber)), # CV as priority
         SchemeName = coalesce(`Scheme Name`, SchemeName), # CV as priority
         FirstName = coalesce(`First Name`), # CV only
         LastName = coalesce(`Last Name`), # CV only
         DateOfBirth = coalesce(BirthDate, DateOfBirth), # CV as priority
         DateOfEvent = coalesce(`Date of Event`, DateofClaim), # CV as priority
         DateNotified = coalesce(DateNotified, `Date of Notification`), # COMPASS as priority
         DateReported = coalesce(DateNotified),
         BenefitAmount = coalesce(BenefitAmountTotal, SumInsured), # CV as priority
         DateOfDecision = coalesce(DecisionDate, `Initial Decision`), # COMPASS as priority
         `Waiting/Qualifying Units` = coalesce(`Waiting/Qualifying Units.y`, `Waiting/Qualifying Units.x`), # CV as priority
         `Waiting/Qualifying Period` = coalesce(`Waiting/Qualifying Period.y`, `Waiting/Qualifying Period.x`), # CV as priority
         AmountPaid = replace_na(AmountPaid, 0) + replace_na(AmountPaidTotal, 0), # sums COMPASS and CV
         FirstEffPaymentDate = pmin(as.Date(FirstEffPaymentDate), as.Date(FirstEffPaymentCV), na.rm = T), # MIN between CV and COMPASS
         LastEffPaymentDate = pmax(as.Date(LastEffPaymentDate), as.Date(LastEffPaymentCV), na.rm = T), # MAX between CV and COMPASS
         Outcome = coalesce(Outcome.y, Outcome.x), # CV as priority
         ClosedReason = coalesce(`Closed Reason`, ReasonforClosure), # cV as priority
         ClosedDate = pmax(dmy(`Benefit Closure Date`), FinalisedDate, na.rm = TRUE), # CV as priority
         Gender = coalesce(Gender.x, Gender.y), # COMPASS as priority
         DateJoinedFund = coalesce(as.Date(DateJoinedFund.x)), # COMPASS only 
         CauseofClaim = coalesce(Cause, CauseofClaim), # CV as priority
         reportrundate = reportrundate) %>% 
  # recode fields to ensure consistency (coming from different systems)
  mutate(Outcome = recode(Outcome,
                          'Admitted' = 'Admit',
                          'Declined' = 'Decline',
                          'Withdrawn' = 'Withdraw',
                          # open could be admitted with ongoing payments or under assessment
                          'Open' = 'Needs Investigation',
                          'Contradictory Outcome' = 'Needs Investigation'),
         # add placeholders for columns just in CV dataframe
         LegacyClaimNumber_CV = NA_character_,
         SC_Additional_Exceptions = character(1)) %>%  
  select(SchemeNumber, SchemeName, MemberNumber, BCID, LegacyClaimNumber_CV, Name, Gender, DateOfBirth, ClaimType, 
         DateOfEvent, DateNotified, DateReported, DecisionDate = DateOfDecision, 
         `Waiting/Qualifying Period`, `Waiting/Qualifying Units`,
         BenefitAmount, Outcome, ClosedReason, ClosedDate, 
         ClaimCause = CauseofClaim, GSCBenefitPeriod, EndofBenefitPeriod, DateJoinedFund,
         AmountPaid, FirstEffPaymentDate, LastEffPaymentDate, ProcessedDate, 
         reportrundate, SC_Additional_Exceptions)

# *4.3 - Appending CV Only ####
cv.clean <- cv %>% 
  filter(#Scheme %in% schemeList,
         # exclude all BCs already included
         !BCID %in% ocr.clean.cv$BCID) %>% 
  mutate(Name = tolower(paste(`First Name`, `Last Name`, sep=',')),
         MemberNumber = NA_character_) %>% 
  # mutate_at(vars('Waiting/Qualifying Period_Standard', 'Sum Insured', 'AmountPaid'), funs(as.numeric)) %>% 
  select(Scheme, `Scheme Name`, MemberNumber, BCID, LegacyClaimNumber_CV = `Legacy Claim Number`, Name, Gender, BirthDate, APRAclaimType, 
         `Date of Event`, `Date of Notification`, DateReported, `Initial Decision`, 
         `Waiting/Qualifying Period`, `Waiting/Qualifying Units`, 
         BenefitAmountTotal, Outcome, `Closed Reason`, `Benefit Closure Date`,
         Cause, `Benefit Period`, `End of Benefit Period`, DateJoinedFund, 
         AmountPaidTotal, FirstEffPaymentCV, LastEffPaymentCV, LastProcessedDate,
         reportrundate, SC_Additional_Exceptions) %>% 
  setNames(colnames(ocr.clean.cv))

ocr.clean.cv <- ocr.clean.cv %>% 
  rbind(cv.clean) %>% 
  # make ClaimType consistent
  mutate(ClaimType = case_when(ClaimType == 'Death' ~ 'Death with TI',
                               TRUE ~ ClaimType))  
  #filter(#interval(DecisionDate, endDate)/years(1) <= 5 | <- filter out claims greater than 5 years (for tender data only)
           # Outcome %in% c('Under Assessment', 'Needs Investigation')) (Unsure what this is for (MG (?)))

# save R object for import into spreadsheet population script
saveRDS(ocr.clean.cv, 'outputs/ocr.cv.clean_all.rds')
write_csv(ocr.clean.cv, 'outputs/ocr.cv.clean_all.csv', na = '')


