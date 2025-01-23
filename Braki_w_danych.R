Data <- read.csv("application_data_new.csv")

install.packages("naniar")
library(naniar)
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
library(ggthemes)
library(ggforce)
library(DescTools)
library(corrplot)

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
library(dplyr)

target_miss <- Data %>%
  group_by(TARGET) %>%
  miss_var_summary

target_heat_mapa <- gg_miss_fct(Data, fct = TARGET)
print(target_heat_mapa)
ncol(Data)

NA_filter <- NA_summary %>%
  filter(n_miss>0)

library(ggplot2)

  
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

Data_hotdeck<- Data_hotdeck[,1:22]

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
  mutate(BASEMENTAREA_AVG = ifelse(BASEMENTAREA_AVG == "5,00E-04", 0.0005, BASEMENTAREA_AVG)) %>%
  mutate(BASEMENTAREA_AVG = ifelse(BASEMENTAREA_AVG == "1,00E-04", 0.0001, BASEMENTAREA_AVG)) %>%
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "9,00E-04", 0.0009, COMMONAREA_AVG)) %>%
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "8,00E-04", 0.0008, COMMONAREA_AVG)) %>%            
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "7,00E-04", 0.0007, COMMONAREA_AVG)) %>%
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "5,00E-04", 0.0005, COMMONAREA_AVG)) %>%   
  mutate(COMMONAREA_AVG = ifelse(COMMONAREA_AVG == "2,00E-04", 0.0002, COMMONAREA_AVG)) %>%
  mutate(LANDAREA_AVG = ifelse(LANDAREA_AVG == "9,00E-04", 0.0009, LANDAREA_AVG))    

rules <- simplify_rules(rules)

Data_hotdeck <- replace_errors(Data_hotdeck, rules)
sum(is.na(Data_hotdeck))

NA_proportion2 <- prop_miss(Data_hotdeck)

Data_hotdeck <- kNN(Data_hotdeck)

Data_hotdeck<- Data_hotdeck[,1:22]

table(Data_hotdeck$ORGANIZATION_TYPE)

Data_hotdeck %>% 
  mutate(APARTMENTS_AVG_Z = transform(Data_hotdeck$APARTMENTS_AVG, method = "zscore"))  %>% 
  select(APARTMENTS_AVG_Z) %>% 
  boxplot()

find_skewness(Data_hotdeck, index=FALSE)      

find_skewness(Data_hotdeck, value=TRUE, thres=0.1)
hist(Data_hotdeck$AMT_INCOME_TOTAL) 
hist(Data_hotdeck$AMT_CREDIT)
hist(Data_hotdeck$APARTMENTS_AVG)
hist(Data_hotdeck$YEARS_BUILD_AVG)
hist(Data_hotdeck$ENTRANCES_AVG)
hist(Data_hotdeck$CREDIT_LOG)

AIT <- transform(Data_hotdeck$AMT_INCOME_TOTAL, method = "log")
plot(AIT)

AC <- transform(Data_hotdeck$AMT_CREDIT, method = "log")
plot(AC)

AA <- transform(Data_hotdeck$APARTMENTS_AVG, method = "log")
plot(AA)

YBA <- transform(Data_hotdeck$YEARS_BUILD_AVG, method = "log")
plot(YBA)

EA <- transform(Data_hotdeck$ENTRANCES_AVG, method = "log")
plot(EA)

CL <- transform(Data_hotdeck$CREDIT_LOG, method = "log")
plot(CL)

data_kat_income <- binning(Data_hotdeck$AMT_INCOME_TOTAL, nbins = 5, type = "equal")
summary(data_kat_income)
plot(data_kat_income)

data_kat_credit <- binning(Data_hotdeck$AMT_CREDIT, nbins = 5, type = "equal")
summary(data_kat_credit)
plot(data_kat_credit)

theme_set(theme_few())

Data_hotdeck <- Data_hotdeck %>%
  mutate(TARGET_2 = if_else(TARGET == 1, "Problemy ze spłatą", "Spłaca"))

pie_chart <- Data_hotdeck %>%
  count(TARGET_2)

ggplot(pie_chart) +
  aes(
    x0 = 0, y0 = 0, # position of pie center
    r0 = 0, r = 1,  # inner and outer radius
    amount = n, # size of pie slices
    fill = TARGET_2
  ) + 
  geom_arc_bar(stat = "pie") +  # from ggforce
  coord_fixed() +
  scale_fill_brewer(palette = "Set1")

ggplot(Data_hotdeck, aes(x = INCOME_LOG, fill = TARGET_2)) +
  geom_histogram() +
  xlab("Dochód") +
  ggtitle("Udział osób mających problem ze spłatą kredytu w grupie dochodowej") +
  scale_fill_brewer(palette = "Set1")

ggplot(Data_hotdeck, aes(x = INCOME_LOG, y = CREDIT_LOG, color = CODE_GENDER))+
  geom_point() +
  xlab("Dochody") +
  ylab("Wielkość kredytu") +
  ggtitle("Zależność wielkości kredytu od dochodów") +
  scale_fill_brewer(palette = "Set1")

ggplot(Data_hotdeck, aes(x = NAME_CONTRACT_TYPE, fill = TARGET_2)) +
  geom_bar() +
  xlab("Nazwa kontraktu") +
  ggtitle("Udział osób mających problem ze spłatą kredytu w zależności od rodzaju kontraktu") +
  scale_fill_brewer(palette = "Set1")

ggplot(Data_hotdeck, aes(x = TARGET_2, y = CREDIT_LOG, fill = TARGET_2)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Wykres skrzypcowy", x = "", y = "Wielkość kredytu") +
  scale_fill_brewer(palette = "Set1")

ggplot(Data_hotdeck, aes(x = INCOME_LOG, y = CREDIT_LOG)) +
  geom_point() +
  facet_wrap(TARGET_2 ~ NAME_EDUCATION_TYPE) +
  scale_fill_brewer(palette = "Set1")

pie_chart_g <- Data_hotdeck %>%
  count(CODE_GENDER)

ggplot(pie_chart_g) +
  aes(
    x0 = 0, y0 = 0,
    r0 = 0, r = 1,
    amount = n,
    fill = CODE_GENDER
  ) + 
  geom_arc_bar(stat = "pie") +
  coord_fixed() +
  scale_fill_brewer(palette = "Set1")

ggplot(Data_hotdeck, aes(x = INCOME_LOG, fill = NAME_FAMILY_STATUS)) +
  geom_boxplot() +
  xlab("Dochody") +
  ggtitle("Dochód całkowity w zależności od statusu rodziny") +
  scale_fill_brewer(palette = "Set1")
geom_boxplot()

ggplot(Data_hotdeck, aes(x = NAME_HOUSING_TYPE, fill = TARGET_2)) +
  geom_bar() +
  xlab("Typ nieruchomości") +
  ggtitle("Udział osób spłacających kredyt w zależności od typu nieruchomości") +
  scale_fill_brewer(palette = "Set1")

#Statystyki opisowe
str(Data_hotdeck)
Data_hotdeck$BASEMENTAREA_AVG <-as.numeric(Data_hotdeck$BASEMENTAREA_AVG)
Data_hotdeck$COMMONAREA_AVG <-as.numeric(Data_hotdeck$COMMONAREA_AVG)
Data_hotdeck$LANDAREA_AVG <-as.numeric(Data_hotdeck$LANDAREA_AVG)

options(scipen=999)

calculate_statistics_i <- function(data, columns) {
  stats <- lapply(columns, function(column) {
    col_data <- data[[column]]
    data.frame(
      średnia = mean(col_data),
      moda = Mode(col_data),
      mediana = median(col_data),
      odch_st = sd(col_data),
      wariancja = var(col_data),
      wsp_zm = sd(col_data) / mean(col_data),
      IQR = IQR(col_data),
      interquartile_deviation = IQR(col_data) / 2,
      IQR_coeff_var = (IQR(col_data) / 2) / median(col_data),
      min = min(col_data),
      max = max(col_data)
      )
  })
  names(stats) <- columns
  return(stats)
}

columns_to_analyze_i <- c("TARGET","CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "REGION_RATING_CLIENT", 
                        "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ENTRANCES_AVG", 
                        "LANDAREA_AVG")

statistics_i <- calculate_statistics_i(Data_hotdeck, columns_to_analyze_i)

print(statistics_i)

all_statistics_i <- do.call(rbind, statistics_i)
print(all_statistics_i)
  
corrplot(cor(Data_hotdeck[c("TARGET","CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "REGION_RATING_CLIENT", 
                            "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ENTRANCES_AVG", "LANDAREA_AVG")]), 
                          method = "number", type = "upper", diag =FALSE)

corr_matrix<-cor(Data_hotdeck[c("TARGET","CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "REGION_RATING_CLIENT", 
                                "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ENTRANCES_AVG", "LANDAREA_AVG")])
corrplot(corr_matrix, method="color")

