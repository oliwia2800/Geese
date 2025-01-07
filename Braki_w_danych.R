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

Data_hotdeck<- Data_implications %>%
  group_by(TARGET) %>%
  group_modify(~ hotdeck(.x)) %>%
  ungroup()

Data_hotdeck<- Data_hotdeck[,1:20]

table(Data_hotdeck$NAME_INCOME_TYPE)
table(Data_hotdeck$NAME_EDUCATION_TYPE)
table(Data_hotdeck$NAME_FAMILY_STATUS)
table(Data_hotdeck$NAME_HOUSING_TYPE)
table(Data_hotdeck$ORGANIZATION_TYPE)
table(Data_hotdeck$OCCUPATION_TYPE)

rules <- validator(TARGET == 1 | TARGET == 0, 
                   NAME_CONTRACT_TYPE == "Cash loans" | NAME_CONTRACT_TYPE == "Revolving loans",
                   CODE_GENDER == "M" | CODE_GENDER == "F",
                   FLAG_OWN_REALTY == "Y" | FLAG_OWN_REALTY == "N",
                   CNT_CHILDREN >= 0,
                   CNT_CHILDREN < 30,
                   AMT_INCOME_TOTAL >= 0,
                   AMT_CREDIT > 0,
                   REGION_RATING_CLIENT >= 1,
                   REGION_RATING_CLIENT <= 3,
                   APARTMENTS_AVG >= 0, 
                   BASEMENTAREA_AVG >= 0, 
                   YEARS_BUILD_AVG >= 0, 
                   COMMONAREA_AVG >= 0, 
                   ENTRANCES_AVG >= 0, 
                   LANDAREA_AVG >= 0,
                   APARTMENTS_AVG <= 1, 
                   BASEMENTAREA_AVG <= 1,
                   YEARS_BUILD_AVG <= 1, 
                   COMMONAREA_AVG <= 1, 
                   ENTRANCES_AVG <= 1,
                   LANDAREA_AVG <= 1, 
                   ORGANIZATION_TYPE != "XNA")
                   
results <- confront(Data_hotdeck, rules, key="TARGET")
summary(results)
barplot(results, main="Data_hotdeck")

Data_hotdeck <- Data_hotdeck %>%
  mutate(BASEMENTAREA_AVG = ifelse(BASEMENTAREA_AVG == "5,00E-04", "0,0005", BASEMENTAREA_AVG)) %>%
  mutate(BASEMENTAREA_AVG = ifelse(BASEMENTAREA_AVG == "1,00E-04", "0,0001", BASEMENTAREA_AVG)) %>%
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "9,00E-04", "0,0009", COMMONAREA_AVG)) %>%
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "8,00E-04", "0,0008", COMMONAREA_AVG)) %>%            
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "7,00E-04", "0,0007", COMMONAREA_AVG)) %>%
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "5,00E-04", "0,0005", COMMONAREA_AVG)) %>%   
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "2,00E-04", "0,0002", COMMONAREA_AVG)) %>%
  mutate(LANDAREA_AVG = ifelse(LANDAREA_AVG == "9,00E-04", "0,0009", LANDAREA_AVG))    

rules <- simplify_rules(rules)

Data_hotdeck <- replace_errors(Data_hotdeck, rules)
sum(is.na(Data_hotdeck))

