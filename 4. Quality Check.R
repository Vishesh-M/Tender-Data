#-----------------------
# Clear Environment ####
#-----------------------

rm(list=ls())

# = = = = = = = = = = = = =
# 1.0 Import Packages ####
#= = = = = = = = = = = = =

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(magrittr)

# ocr.clean <- read_rds('outputs/ocr.cv.clean.rds')
# ocr.clean <- read_csv('updates2.csv') 
# ocr.clean %>% 
#   mutate_at(vars('DateOfBirth', 'DateOfEvent', 'DateNotified', 'DateReported', 'DateOfDecision', 'FinalisedDate',
#                  'DateJoinedFund', 'FirstEffPaymentDate', 'LastEffPaymentDate','ProcessedDate', 'reportrundate'),
#             funs(dmy)) %>% write_rds('outputs/ocr.cv.clean.rds')

ocr.clean <- read_rds('outputs/ocr.cv.clean.rds') 

admitReasons <- c('Admit')
exgratiaReasons <- c('Ex-Gratia')
undeterminedReasons <- c('Open', 'Pending', 'Under Assessment')
withdrawReasons <- c('Withdraw')
declineReasons <- c('Decline')

copy_excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names, na='',...)
}

# clean up output
ocr.clean <- ocr.clean %>% 
  select(-one_of('BenefitAmount', 'AmountPaid'), one_of('BenefitAmount', 'AmountPaid'))


# add Ali NAB exception file

 
# identify claims to exclude pending investigation
ocr.clean.exceptions <- ocr.clean %>%
  mutate(StateOfResidence = NA_character_,
         MemberCategory = NA_character_,
         SalaryAtDateOfClaim = NA_real_) %>% 
  rename(DateIncurred = DateOfEvent) %>%
  mutate(TenderExceptions = character(length = 1)) %>% # initialise description field
  mutate(
    # Missing value Rules
    TenderExceptions = case_when(is.na(BCID) & is.na(MemberNumber) ~ paste0(TenderExceptions, 'Missing Claim Number - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when((is.na(`Waiting/Qualifying Period`) | is.na(`Waiting/Qualifying Units`)) & 
                                   ClaimType == 'DII' ~ paste0(TenderExceptions, 'Missing Waiting Period information - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(ClaimType) | ClaimType == '' ~ paste0(TenderExceptions, 'Missing Claim Type - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(ClaimType == 'DII' &
                                is.na(GSCBenefitPeriod) | GSCBenefitPeriod == '' ~ paste0(TenderExceptions, 'Missing Benefit Period - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(Gender) | Gender == '' ~ paste0(TenderExceptions, 'Missing Gender - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(DateOfBirth) ~ paste0(TenderExceptions, 'Missing Date of Birth - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(StateOfResidence) | StateOfResidence == '' ~ paste0(TenderExceptions, 'Missing State Of Residence - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(DateJoinedFund) ~ paste0(TenderExceptions, 'Missing Date Joined Fund - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(MemberCategory) | MemberCategory == '' ~ paste0(TenderExceptions, 'Missing Member Category - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(SalaryAtDateOfClaim) ~ paste0(TenderExceptions, 'Missing Salary at Date of Claim - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(DateIncurred) ~ paste0(TenderExceptions, 'Missing Date Incurred - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(DateNotified) ~ paste0(TenderExceptions, 'Missing Date Notified - '),
                              TRUE ~ TenderExceptions),
    # Missing Decision Date
    TenderExceptions = case_when((is.na(Outcome) | Outcome %in% undeterminedReasons) &
                                !is.na(DecisionDate) ~ paste0(TenderExceptions, 'Decision Date without Outcome - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(!is.na(Outcome) &
                                !Outcome %in% c(undeterminedReasons, 'Needs Investigation') &
                                is.na(DecisionDate) ~ paste0(TenderExceptions, 'Outcome without Decision Date - '),
                              TRUE ~ TenderExceptions),
    # 'Payment {from,to} Date'
    TenderExceptions = case_when(is.na(FirstEffPaymentDate) &
                                ClaimType == 'DII' &
                                AmountPaid > 0 ~ paste0(TenderExceptions, 'Missing Payment Start Date - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(is.na(LastEffPaymentDate) &
                                ClaimType == 'DII' &
                                AmountPaid > 0 ~ paste0(TenderExceptions, 'Missing Payment End Date - '),
                              TRUE ~ TenderExceptions),
    
    # Outcome
    TenderExceptions = case_when(grepl('Contradictory Outcome|CHECK RULE|Needs Investigation', Outcome) ~ paste0(TenderExceptions, 'Claim Outcome Requires Investigation - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when((is.na(Outcome) | Outcome == 0) ~ paste0(TenderExceptions, 'No claim Outcome - '),
                              TRUE ~ TenderExceptions),
    
    TenderExceptions = case_when(is.na(ClaimCause) | ClaimCause == '' ~ paste0(TenderExceptions, 'Missing Source of Claim - '),
                              TRUE ~ TenderExceptions),
    
    # Missing Total Paid Benefit
    TenderExceptions = case_when(!is.na(Outcome) &
                                (Outcome %in% admitReasons | Outcome %in% exgratiaReasons) &
                                is.na(AmountPaid) ~ paste0(TenderExceptions, 'Admit no Payment - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(!is.na(Outcome) & 
                                (Outcome %in% declineReasons | Outcome %in% withdrawReasons | Outcome %in% undeterminedReasons) &
                                (!is.na(AmountPaid) & AmountPaid != 0) ~ paste0(TenderExceptions, 'Non-Admit with Payment - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(ClaimType != 'DII' &
                                !is.na(AmountPaid) & AmountPaid != 0 & !is.na(BenefitAmount) &
                                AmountPaid > (BenefitAmount * 1) ~ paste0(TenderExceptions, 'Amount Paid over SI - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(ClaimType != 'DII' &
                                !is.na(AmountPaid) & AmountPaid != 0 & !is.na(BenefitAmount) &
                                AmountPaid < (BenefitAmount * 1) ~ paste0(TenderExceptions, 'Amount Paid under SI - '),
                              TRUE ~ TenderExceptions),
    
    TenderExceptions = case_when((is.na(BenefitAmount) | BenefitAmount == '' | BenefitAmount < 0) &
                                !Outcome %in% undeterminedReasons ~ paste0(TenderExceptions, 'Missing Sum Insured - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(!is.na(DateJoinedFund) & !is.na(DateIncurred) &
                                DateJoinedFund > DateIncurred &
                                grepl('Admit|Investigation|Contradictory|Check', Outcome, ignore.case = T)  
                                  ~ paste0(TenderExceptions, 'Date Joined Fund after Date of Event - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(!is.na(DateIncurred) & !is.na(DateNotified) & 
                                DateNotified < DateIncurred ~ paste0(TenderExceptions, 'Date Notified prior to Date Incurred - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(!is.na(DecisionDate) & !is.na(DateNotified) & 
                                DecisionDate < DateNotified ~ paste0(TenderExceptions, 'Decision Date prior to Date Notified - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(!is.na(FirstEffPaymentDate) & !is.na(DateIncurred) & 
                                FirstEffPaymentDate < DateIncurred ~ paste0(TenderExceptions, 'Payment Start Date prior to Date Incurred - '),
                              TRUE ~ TenderExceptions),
    
    # Benefit Period preprocessing
    wp_add = case_when(`Waiting/Qualifying Units` %in% c('Day', 'Days') ~ days(`Waiting/Qualifying Period`),
                       `Waiting/Qualifying Units` %in% c('Week', 'Weeks') ~ weeks(`Waiting/Qualifying Period`),
                       `Waiting/Qualifying Units` %in% c('Month', 'Months') ~ months(`Waiting/Qualifying Period`),
                       `Waiting/Qualifying Units` %in% c('Year', 'Years') ~ years(`Waiting/Qualifying Period`),
                       TRUE ~ days(`Waiting/Qualifying Period`)),
    StartBenefitPeriod_aux = DateIncurred + wp_add,
    EndBenefitPeriod_aux = case_when(grepl('to age', GSCBenefitPeriod) ~ DateOfBirth + years(as.numeric(str_match(GSCBenefitPeriod, 'to age (\\d+)')[,2])),
                                     grepl('^\\d+$', GSCBenefitPeriod) ~ StartBenefitPeriod_aux + years(as.numeric(GSCBenefitPeriod)),
                                     TRUE ~ as.Date(NA)),
    BenefitPeriod_aux = interval(StartBenefitPeriod_aux, EndBenefitPeriod_aux) / months(1),
    APvSI = AmountPaid / BenefitAmount,
    TenderExceptions = case_when(ClaimType == 'DII' & !is.na(AmountPaid) & !is.na(BenefitAmount) &
                                APvSI > 1.1 * BenefitPeriod_aux ~ paste0(TenderExceptions, 'Duration implied by Payment/SI longer Benefit Period over 10% - '),
                              TRUE ~ TenderExceptions),

    # APRA rules leftover
    TenderExceptions = case_when(DateIncurred < ymd('1990-01-01') ~
                                paste0(TenderExceptions, 'Incorrect Date Incurred - '),
                              TRUE ~ TenderExceptions),
    TenderExceptions = case_when(DateNotified < ymd('1990-01-01') ~
                                paste0(TenderExceptions, 'Incorrect Date Notified - '),
                              TRUE ~ TenderExceptions)
  ) %>% 
  mutate(ClaimsExceptions = character(length = 1)) %>% # initialise description field 
  mutate(
    # Missing value Rules
    ClaimsExceptions = case_when(is.na(BCID) & is.na(MemberNumber) ~ paste0(ClaimsExceptions, 'Missing Claim Number - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when((is.na(`Waiting/Qualifying Period`) | is.na(`Waiting/Qualifying Units`)) & 
                                   ClaimType == 'DII' ~ paste0(ClaimsExceptions, 'Missing Waiting Period information - '),
                                 TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(is.na(ClaimType) | ClaimType == '' ~ paste0(ClaimsExceptions, 'Missing Claim Type - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(ClaimType == 'DII' &
                                is.na(GSCBenefitPeriod) | GSCBenefitPeriod == '' ~ paste0(ClaimsExceptions, 'Missing Benefit Period - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(is.na(DateOfBirth) ~ paste0(ClaimsExceptions, 'Missing Date of Birth - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(is.na(DateIncurred) ~ paste0(ClaimsExceptions, 'Missing Date Incurred - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(is.na(DateNotified) ~ paste0(ClaimsExceptions, 'Missing Date Notified - '),
                              TRUE ~ ClaimsExceptions),
    # Missing Decision Date
    ClaimsExceptions = case_when((is.na(Outcome) | Outcome %in% undeterminedReasons) &
                                !is.na(DecisionDate) ~ paste0(ClaimsExceptions, 'Decision Date without Outcome - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(!is.na(Outcome) &
                                !Outcome %in% c(undeterminedReasons, 'Needs Investigation') &
                                is.na(DecisionDate) ~ paste0(ClaimsExceptions, 'Outcome without Decision Date - '),
                              TRUE ~ ClaimsExceptions),
    # 'Payment {from,to} Date'
    ClaimsExceptions = case_when(is.na(FirstEffPaymentDate) &
                                ClaimType == 'DII' &
                                AmountPaid > 0 ~ paste0(ClaimsExceptions, 'Missing Payment Start Date - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(is.na(LastEffPaymentDate) &
                                ClaimType == 'DII' &
                                AmountPaid > 0 ~ paste0(ClaimsExceptions, 'Missing Payment End Date - '),
                              TRUE ~ ClaimsExceptions),
    
    # Missing Claim Stats (CHECK) ####
    ClaimsExceptions = case_when(grepl('Contradictory Outcome|CHECK RULE|Needs Investigation', Outcome) ~ paste0(ClaimsExceptions, 'Claim Outcome Requires Investigation - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when((is.na(Outcome) | Outcome == 0) ~ paste0(ClaimsExceptions, 'No claim Outcome - '),
                              TRUE ~ ClaimsExceptions),
    
    ClaimsExceptions = case_when(is.na(ClaimCause) | ClaimCause == '' ~ paste0(ClaimsExceptions, 'Missing Source of Claim - '),
                              TRUE ~ ClaimsExceptions),
    
    # Missing Total Paid Benefit
    ClaimsExceptions = case_when(!is.na(Outcome) &
                                ClaimType != 'DII' &
                                (Outcome %in% admitReasons | Outcome %in% exgratiaReasons) &
                                is.na(AmountPaid) ~ paste0(ClaimsExceptions, 'Admit no Payment - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(!is.na(Outcome) & 
                                (Outcome %in% declineReasons | Outcome %in% withdrawReasons | Outcome %in% undeterminedReasons) &
                                (!is.na(AmountPaid) & AmountPaid != 0) ~ paste0(ClaimsExceptions, 'Non-Admit with Payment - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(ClaimType != 'DII' &
                                !is.na(AmountPaid) & AmountPaid != 0 & !is.na(BenefitAmount) &
                                AmountPaid > (BenefitAmount + 1) ~ paste0(ClaimsExceptions, 'Amount Paid over SI - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(ClaimType != 'DII' &
                                !is.na(AmountPaid) & AmountPaid != 0 & !is.na(BenefitAmount) &
                                AmountPaid < (BenefitAmount - 1) ~ paste0(ClaimsExceptions, 'Amount Paid under SI - '),
                              TRUE ~ ClaimsExceptions),
    
    ClaimsExceptions = case_when((is.na(BenefitAmount) | BenefitAmount == '' | BenefitAmount < 0) &
                                !Outcome %in% undeterminedReasons ~ paste0(ClaimsExceptions, 'Missing Sum Insured - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(!is.na(DateJoinedFund) & !is.na(DateIncurred) &
                                DateJoinedFund > DateIncurred & 
                                  grepl('Admit|Investigation|Contradictory|Check', Outcome, ignore.case = T)
                                ~ paste0(ClaimsExceptions, 'Date Joined Fund after Date of Event - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(!is.na(DecisionDate) & !is.na(DateNotified) & 
                                DecisionDate < DateNotified ~ paste0(ClaimsExceptions, 'Decision Date prior to Date Notified - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(!is.na(FirstEffPaymentDate) & !is.na(DateIncurred) & 
                                FirstEffPaymentDate < DateIncurred ~ paste0(ClaimsExceptions, 'Payment Start Date prior to Date Incurred - '),
                              TRUE ~ ClaimsExceptions),
    
    # Benefit Period preprocessing
    wp_add = case_when(`Waiting/Qualifying Units` %in% c('Day', 'Days') ~ days(`Waiting/Qualifying Period`),
                       `Waiting/Qualifying Units` %in% c('Week', 'Weeks') ~ weeks(`Waiting/Qualifying Period`),
                       `Waiting/Qualifying Units` %in% c('Month', 'Months') ~ months(`Waiting/Qualifying Period`),
                       `Waiting/Qualifying Units` %in% c('Year', 'Years') ~ years(`Waiting/Qualifying Period`),
                       TRUE ~ days(`Waiting/Qualifying Period`)),
    StartBenefitPeriod_aux = DateIncurred + wp_add,
    EndBenefitPeriod_aux = case_when(grepl('to age', GSCBenefitPeriod) ~ DateOfBirth + years(as.numeric(str_match(GSCBenefitPeriod, 'to age (\\d+)')[,2])),
                                     grepl('^\\d+$', GSCBenefitPeriod) ~ StartBenefitPeriod_aux + years(as.numeric(GSCBenefitPeriod)),
                                     TRUE ~ as.Date(NA)),
    BenefitPeriod_aux = interval(StartBenefitPeriod_aux, EndBenefitPeriod_aux) / months(1),
    APvSI = AmountPaid / BenefitAmount,
    ClaimsExceptions = case_when(ClaimType == 'DII' & !is.na(AmountPaid) & !is.na(BenefitAmount) &
                                APvSI > 1.1 * BenefitPeriod_aux ~ paste0(ClaimsExceptions, 'Duration implied by Payment/SI longer Benefit Period over 10% - '),
                              TRUE ~ ClaimsExceptions),
    
    # APRA rules leftover
    ClaimsExceptions = case_when(DateIncurred < ymd('1990-01-01') ~
                                paste0(ClaimsExceptions, 'Incorrect Date Incurred - '),
                              TRUE ~ ClaimsExceptions),
    ClaimsExceptions = case_when(DateNotified < ymd('1990-01-01') ~
                                paste0(ClaimsExceptions, 'Incorrect Date Notified - '),
                              TRUE ~ ClaimsExceptions)
  ) %>% 
  # bring SC_Add_Exceptions from CV to the final dataset
  mutate(TenderExceptions = paste0(replace_na(SC_Additional_Exceptions,''), TenderExceptions),
         ClaimsExceptions = paste0(replace_na(SC_Additional_Exceptions,''), ClaimsExceptions))


# clean up output
ocr.clean.exceptions <- ocr.clean.exceptions %>% 
  select(-one_of('MultiplePayments', 'StateOfResidence', 'MemberCategory', 'SalaryAtDateOfClaim', 'wp_add', 'APvSI',
                 'StartBenefitPeriod_aux', 'EndBenefitPeriod_aux')) %>% 
  mutate(ClaimsReviewedFlag = NA_character_)

# export
ocr.clean.exceptions %>% 
  mutate(Exception = str_split(str_remove(ClaimsExceptions, '\\s-\\s$'), ' - ')) %>% 
  unnest(Exception) %>% 
  count(Exception) %>% 
  rename(`Number of cases` = n) %>%
  filter(Exception != '') %>% 
  arrange(-`Number of cases`) %T>% 
  copy_excel()


# export list of claims requiring investigation
ocr.clean.exceptions %>% 
  select(-one_of('StartBenefitPeriod_aux', 'EndBenefitPeriod_aux', 'APvSI', 'BenefitPeriod_aux')) %>% 
  write_csv('outputs/ocr_clean_exceptions.csv', na = '')
# write_csv(exc_counts, 'outputs/ocr_clean_exceptions_counts.csv', na = '')

