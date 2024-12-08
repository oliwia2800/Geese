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
VIM::pbox(Data_implications[,1:20], pos=1, las=2)

Data_implications_kNN <- kNN(Data_implications)

