library(naniar)
library(dplyr)
library(ggplot2)
library(outliers)
library(mice)
library(Amelia)
library(finalfit)
library(VIM)
library(rmdformats)
library(validate)
library(validatetools)
library(dcmodify)
library(errorlocate)
library(deductive)
library(simputation)
library(lumberjack)
library(ISLR) 
library(dlookr)
library(xts)
library(quantmod)
library(ROCR)
library(Information)
library(scorecard)

Kredyty <- read.csv("application_data_new.csv", sep = ";")
set.seed(13)
Data <- sample_n(Kredyty, 2500)

NA_count <- n_miss(Data)
complete_values <- n_complete(Data)
NA_proportion <- prop_miss(Data)
NA_percentage <- pct_miss(Data)
NA_summary <- miss_var_summary(Data)
NA_summary_case <- miss_case_table(Data)

vis_miss(Data, warn_large_data=FALSE)

mean(NA_summary$n_miss)

target_miss <- Data %>%
  group_by(TARGET) %>%
  miss_var_summary

target_heat_mapa <- gg_miss_fct(Data, fct = TARGET)
print(target_heat_mapa)
ncol(Data)

NA_filter <- NA_summary %>%
  filter(n_miss>0)

gg_miss_upset(Data,
              nsets = 122)

Data <- Data %>%
  mutate(INCOME_LOG = log(AMT_INCOME_TOTAL)) %>%
           mutate(CREDIT_LOG = log(AMT_CREDIT))

ggplot(data = Data, aes(x = INCOME_LOG, y = CREDIT_LOG)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("#CE4257","#1982C4")) +
  theme_minimal()

is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

sapply(Data, is.special)

for (n in colnames(Data)){
  is.na(Data[[n]]) <- is.special(Data[[n]])
}
summary(Data)

data_numeric <- Data %>%
  select(where(is.numeric))

test_grubbs <- function(column) {
  if (length(column) > 2) { 
    result <- grubbs.test(column)
    return(result)
  } else {
    return(NA) 
  }
}

results_grubbs <- lapply(data_numeric, test_grubbs)

Data_implications <- Data
VIM::aggr(Data_implications[,1:20])

Data_implications_kNN <- kNN(Data_implications)

Data_implications_kNN <- Data_implications_kNN %>%
  mutate(DAYS_BIRTH = as.Date(DAYS_BIRTH)) %>%
  mutate(DAYS_EMPLOYED = as.Date(DAYS_EMPLOYED)) 

rules_birth <- validator(DAYS_BIRTH >= as.Date("1850-01-01"),
                         DAYS_BIRTH < as.Date("2024-12-08"))
results_birth <- confront(Data_implications_kNN, rules_birth)
summary(results_birth)

rules_employed <- validator(DAYS_EMPLOYED >= as.Date("1865-01-01"),
                            DAYS_EMPLOYED < as.Date("2024-12-08"))
results_employed <- confront(Data_implications_kNN, rules_employed)
summary(results_employed)

rules_target <- validator(TARGET == 1 | TARGET == 0)
results_target <- confront(Data_implications_kNN, rules_target)
summary(results_target)

rules_name_contract_type <- validator(NAME_CONTRACT_TYPE == "Cash loans" | NAME_CONTRACT_TYPE == "Revolving loans")
results_name_contract_type <- confront(Data_implications_kNN, rules_name_contract_type)
summary(results_name_contract_type)

rules_code_gender <- validator(CODE_GENDER == "M" | CODE_GENDER == "F")
results_code_gender <- confront(Data_implications_kNN, rules_code_gender)
summary(results_code_gender)

rules_flag_own_realty <- validator(FLAG_OWN_REALTY == "Y" | FLAG_OWN_REALTY == "N")
results_flag_own_realty <- confront(Data_implications_kNN, rules_flag_own_realty)
summary(results_flag_own_realty)

rules_cnt_children <- validator(CNT_CHILDREN >= 0,
                                CNT_CHILDREN < 30)
results_cnt_children <- confront(Data_implications_kNN,rules_cnt_children)
summary(results_cnt_children)

rules_amt_income_total <- validator(AMT_INCOME_TOTAL >= 0)
results_amt_income_total <- confront(Data_implications_kNN,rules_amt_income_total)
summary(results_amt_income_total)

rules_amt_credit <- validator(AMT_CREDIT > 0)
results_amt_credit <- confront(Data_implications_kNN,rules_amt_credit)
summary(results_amt_credit)
