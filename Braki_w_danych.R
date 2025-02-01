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
library(ggstatsplot)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

#Baza danych
Kredyty <- read.csv("application_data_new.csv", sep = ";")
set.seed(13)
Data <- sample_n(Kredyty, 2500)

#Braki w danych
NA_count <- n_miss(Data)
complete_values <- n_complete(Data)
NA_proportion <- prop_miss(Data)
NA_percentage <- pct_miss(Data)
NA_summary <- miss_var_summary(Data)
NA_summary_case <- miss_case_table(Data)
mean(NA_summary$n_miss)
ncol(Data)

vis_miss(Data, warn_large_data=FALSE)

target_miss <- Data %>%
  group_by(TARGET) %>%
  miss_var_summary

target_heat_mapa <- gg_miss_fct(Data, fct = TARGET)
print(target_heat_mapa)

NA_filter <- NA_summary %>%
  filter(n_miss>0)
  
gg_miss_upset(Data,
              nsets = 122)

Data <- Data %>%
  mutate(INCOME_LOG = log(AMT_INCOME_TOTAL)) %>%
  mutate(CREDIT_LOG = log(AMT_CREDIT))

is.special <- function(x){if(is.numeric(x)) !is.finite(x) else is.na(x)}
sapply(Data, is.special)
for(n in colnames(Data)){is.na(Data[[n]]) <- is.special(Data[[n]])}
summary(Data)

data_numeric <- Data %>%
  select(where(is.numeric))

test_grubbs <- function(column) {
  if (length(column) > 2) { 
    result <- grubbs.test(column)
    return(result)} 
  else {return(NA)}}

results_grubbs <- lapply(data_numeric, test_grubbs)

Data_implications <- Data
VIM::aggr(Data_implications[,1:20])

#Implikacje braków danych
Data_hotdeck<- Data_implications %>%
  group_by(TARGET) %>%
  group_modify(~ hotdeck(.x)) %>%
  ungroup()

#Reguły walidacyjne
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
barplot(results, main="Dane niespełniające reguł")

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
  boxplot(main = "Wykres pudełkowy dla zmiennej APARTMENTS_AVG")

#Skośność, logarytmowanie danych
str(Data_hotdeck)
Data_hotdeck$BASEMENTAREA_AVG <-as.numeric(Data_hotdeck$BASEMENTAREA_AVG)
Data_hotdeck$COMMONAREA_AVG <-as.numeric(Data_hotdeck$COMMONAREA_AVG)
Data_hotdeck$LANDAREA_AVG <-as.numeric(Data_hotdeck$LANDAREA_AVG)

find_skewness(Data_hotdeck, index=FALSE)      
find_skewness(Data_hotdeck, value=TRUE, thres=0.1)

hist_vars <- c("AMT_INCOME_TOTAL", "AMT_CREDIT", "APARTMENTS_AVG", 
               "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", 
               "ENTRANCES_AVG", "LANDAREA_AVG")

data_long <- Data_hotdeck %>%
  select(all_of(hist_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(data_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Skośność zmiennych",
       x = "Wartość", 
       y = "Częstotliwość")+ 
  theme(plot.title = element_text(hjust = 0.5))

Data_hotdeck <- Data_hotdeck %>%
  mutate(AMT_INCOME_TOTAL = log1p(AMT_INCOME_TOTAL),
         AMT_CREDIT = log1p(AMT_CREDIT),
         APARTMENTS_AVG = log1p(APARTMENTS_AVG),
         BASEMENTAREA_AVG = log1p(BASEMENTAREA_AVG),
         YEARS_BUILD_AVG = log1p(YEARS_BUILD_AVG),
         COMMONAREA_AVG = log1p(COMMONAREA_AVG),
         ENTRANCES_AVG = log1p(ENTRANCES_AVG),
         LANDAREA_AVG = log1p(LANDAREA_AVG))

hist_vars2 <- c("AMT_INCOME_TOTAL", "AMT_CREDIT", "APARTMENTS_AVG", 
               "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", 
               "ENTRANCES_AVG", "LANDAREA_AVG")

data_long2 <- Data_hotdeck %>%
  select(all_of(hist_vars2)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(data_long2, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Skośność zmiennych",
       x = "Wartość", 
       y = "Częstotliwość")+ 
  theme(plot.title = element_text(hjust = 0.5))

#Wizualizacje
theme_set(theme_few())

Data_hotdeck <- Data_hotdeck %>%
  mutate(TARGET_2 = if_else(TARGET == 1, "Problemy ze spłatą", "Spłaca"))

pie_chart <- Data_hotdeck %>%
  count(TARGET_2)

ggplot(pie_chart) +
  aes(
    x0 = 0, y0 = 0,
    r0 = 0, r = 1,
    amount = n, 
    fill = TARGET_2
  ) + 
  geom_arc_bar(stat = "pie") +
  coord_fixed() +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Udział osób mających problemy ze spłatą kredytu i bez problemów ze spłatą kredytu w próbie") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data_hotdeck, aes(x = INCOME_LOG, fill = TARGET_2)) +
  geom_histogram(binwidth = 0.25) +
  xlab("Dochód") +
  ggtitle("Udział osób mających problem ze spłatą kredytu w grupie dochodowej") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data_hotdeck, aes(x = INCOME_LOG, y = CREDIT_LOG, color = TARGET_2))+
  geom_point() +
  xlab("Dochody") +
  ylab("Wielkość kredytu") +
  ggtitle("Zależność wielkości kredytu od dochodów") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

Data_hotdeck %>%
  group_by(NAME_CONTRACT_TYPE) %>%
  summarise(Liczba_obserwacji = n(),
            Spłaca = sum(TARGET_2 == "Spłaca"),
            Nie_spłaca = sum(TARGET_2 == "Problemy ze spłatą")) %>%
  mutate(Procent_nie_spłaca = (Nie_spłaca / Liczba_obserwacji) * 100)

ggplot(Data_hotdeck, aes(x = NAME_CONTRACT_TYPE, fill = TARGET_2)) +
  geom_bar() +
  xlab("Nazwa kontraktu") +
  ggtitle("Udział osób mających problem ze spłatą kredytu w zależności od rodzaju kontraktu") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data_hotdeck, aes(x = TARGET_2, y = CREDIT_LOG, fill = TARGET_2)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Wielkość kredytu ze względu na grupy z problemami oraz bez problemów ze spłatą kredytów", 
       x = "", y = "Wielkość kredytu") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

Data_hotdeck %>%
  group_by(NAME_EDUCATION_TYPE) %>%
  summarise(Liczba_obserwacji = n(),
            Spłaca = sum(TARGET_2 == "Spłaca"),
            Nie_spłaca = sum(TARGET_2 == "Problemy ze spłatą")) %>%
  mutate(Procent_nie_spłaca = (Nie_spłaca / Liczba_obserwacji) * 100) 

ggplot(Data_hotdeck, aes(x = INCOME_LOG, y = CREDIT_LOG)) +
  geom_point(color = "steelblue") +
  facet_wrap(TARGET_2 ~ NAME_EDUCATION_TYPE, ncol = 4) +
  ggtitle("Problemy oraz brak problemów ze spłatą kredytu w zależności od poziomu edukacji kredytobiorcy") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

#Próba badawcza
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
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Udział kobiet i mężczyzn w próbie badawczej") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data_hotdeck, aes(x = INCOME_LOG, fill = NAME_FAMILY_STATUS)) +
  geom_boxplot() +
  xlab("Dochody") +
  ggtitle("Dochód całkowity w zależności od statusu rodziny") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))
geom_boxplot()

table(Data_hotdeck$CNT_CHILDREN) 
Data_hotdeck <- Data_hotdeck %>%
  mutate(HAS_CHILDREN = case_when(
    CNT_CHILDREN == 0 ~ "Nie posiada dzieci",
    CNT_CHILDREN == 1 ~ "Posiada jedno dziecko",
    CNT_CHILDREN == 2 ~ "Posiada dwoje dzieci",
    CNT_CHILDREN > 2 ~ "Posiada więcej niż dwoje dzieci"))

ggplot(Data_hotdeck, aes(x = NAME_HOUSING_TYPE, fill = HAS_CHILDREN)) +
  geom_bar() +
  xlab("Typ nieruchomości") +
  ggtitle("Udział osób posiadających dzieci w zależności od typu zamieszkanej nieruchomości") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data_hotdeck, aes(x = OCCUPATION_TYPE, fill = NAME_EDUCATION_TYPE)) +
  geom_bar() +
  xlab("Typ wykonywanego zawodu") +
  ggtitle("Udział osób z danym wykształceniem w zależności od typu wykonywanego zawodu") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5))

apartment_vars <- c("APARTMENTS_AVG", "BASEMENTAREA_AVG", 
                    "YEARS_BUILD_AVG", "COMMONAREA_AVG", 
                    "ENTRANCES_AVG", "LANDAREA_AVG")

data_long_apartment <- Data_hotdeck %>%
  select(all_of(apartment_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(data_long_apartment, aes(x = Value)) +
  geom_density(fill = "steelblue", color = "black") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Dane o zamieszkiwanych nieruchomościach",
       x = "Wartość", 
       y = "Częstotliwość") + 
  theme(plot.title = element_text(hjust = 0.5))

#Statystyki opisowe
corrplot(cor(Data_hotdeck[c("TARGET","CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "REGION_RATING_CLIENT", 
                            "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ENTRANCES_AVG", "LANDAREA_AVG")]), 
         method = "number", type = "upper", diag =FALSE)

corr_matrix<-cor(Data_hotdeck[c("TARGET","CNT_CHILDREN", "AMT_INCOME_TOTAL", "AMT_CREDIT", "REGION_RATING_CLIENT", 
                                "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ENTRANCES_AVG", "LANDAREA_AVG")])
corrplot(corr_matrix, method="color")

Data_hotdeck %>%
  select(AMT_CREDIT, TARGET_2) %>%
  group_by(TARGET_2) %>%
  summarize(minimum=min(AMT_CREDIT),
            maksimum=max(AMT_CREDIT),
            średnia=mean(AMT_CREDIT),
            odchylenie=sd(AMT_CREDIT),
            mediana=median(AMT_CREDIT),
            Q1=quantile(AMT_CREDIT,0.25),
            Q3=quantile(AMT_CREDIT,0.75),
            skośność=Skew(AMT_CREDIT),
            kurtoza=Kurt(AMT_CREDIT)) %>%
  kbl() %>%
  kable_classic_2(full_width = F) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(1:2, bold = F) %>%
  row_spec(1, bold = F, color = "black", background = "white")

Data_hotdeck %>%
  select(AMT_INCOME_TOTAL, TARGET_2) %>%
  group_by(TARGET_2) %>%
  summarize(minimum=min(AMT_INCOME_TOTAL),
            maksimum=max(AMT_INCOME_TOTAL),
            średnia=mean(AMT_INCOME_TOTAL),
            odchylenie=sd(AMT_INCOME_TOTAL),
            mediana=median(AMT_INCOME_TOTAL),
            Q1=quantile(AMT_INCOME_TOTAL,0.25),
            Q3=quantile(AMT_INCOME_TOTAL,0.75),
            skośność=Skew(AMT_INCOME_TOTAL),
            kurtoza=Kurt(AMT_INCOME_TOTAL)) %>%
  kbl() %>%
  kable_classic_2(full_width = F) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(1:2, bold = F) %>%
  row_spec(1, bold = F, color = "black", background = "white")

Data_hotdeck %>%
  select(TARGET_2, APARTMENTS_AVG, BASEMENTAREA_AVG, YEARS_BUILD_AVG, COMMONAREA_AVG, ENTRANCES_AVG, LANDAREA_AVG) %>%
  pivot_longer(cols = -TARGET_2, names_to = "Zmienna", values_to = "VALUE") %>%
  group_by(Zmienna, TARGET_2) %>%
  summarize(minimum=min(VALUE),
            maksimum=max(VALUE),
            średnia=mean(VALUE),
            odchylenie=sd(VALUE),
            mediana=median(VALUE),
            Q1=quantile(VALUE,0.25),
            Q3=quantile(VALUE,0.75),
            skośność=Skew(VALUE),
            kurtoza=Kurt(VALUE)) %>%
  kbl() %>%
  kable_classic_2(full_width = F) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(1:2, bold = F) %>%
  row_spec(1, bold = F, color = "black", background = "white")

apartment_vars <- c("APARTMENTS_AVG", "BASEMENTAREA_AVG", 
                    "YEARS_BUILD_AVG", "COMMONAREA_AVG", 
                    "ENTRANCES_AVG", "LANDAREA_AVG")
 
data_long_apartment <- Data_hotdeck %>%
  select(all_of(apartment_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
 
ggplot(data_long_apartment, aes(x = Value)) +
  geom_density(fill = "steelblue", color = "black") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Dane o zamieszkiwanych nieruchomościach",
       x = "Wartość", 
       y = "Częstotliwość") +
  theme(plot.title = element_text(hjust = 0.5))
 
hist(Data_hotdeck$AMT_CREDIT)
ggstatsplot :: ggbetweenstats(
  data = Data_hotdeck,
  x = FLAG_OWN_REALTY,
  y = AMT_CREDIT,
  type = "np",
  palette = "Set1",
  title = "Porównanie kwot kredytów w zależności od posiadania nieruchomości") + 
  theme(plot.title = element_text(hjust = 0.5))

#Hipotezy statystyczne
Data_hotdeck <- Data_hotdeck %>%
  mutate(data_kat_income = binning(Data_hotdeck$AMT_INCOME_TOTAL, nbins = 5, type = "equal"))
Data_hotdeck <- Data_hotdeck %>%
  mutate(data_kat_credit = binning(Data_hotdeck$AMT_CREDIT, nbins = 5, type = "equal"))

data_kat_income <- binning(Data_hotdeck$AMT_INCOME_TOTAL, nbins = 5, type = "equal")
summary(data_kat_income)
plot(data_kat_income)

data_kat_credit <- binning(Data_hotdeck$AMT_CREDIT, nbins = 5, type = "equal")
summary(data_kat_credit)
plot(data_kat_credit)

ggbarstats(
  data = Data_hotdeck,
  x = NAME_EDUCATION_TYPE,
  y = TARGET_2,
  caption = "test",
  package = "ggsci",
  palette = "default_igv",
  title = "Rozkład poziomu edukacji w zależności od problemów ze spłatą kredytu"
)+ 
  theme(plot.title = element_text(hjust = 0.5))

ggpiestats(
  data = Data_hotdeck,
  x = data_kat_credit,
  y = NAME_FAMILY_STATUS,
  palette = "Set1",
  title = "Rozkład pięciu przedziałów kredytów w zależności od statusu rodziny"
)  + 
  theme(plot.title = element_text(hjust = 0.5))

mod <- aov(
  TARGET ~ OCCUPATION_TYPE * NAME_HOUSING_TYPE,
  data = Data_hotdeck
)
summary(mod)
ggcoefstats(mod, title = "Test Anova dla zmiennej TARGET w zależności od typu zawodu i typu nieruchomości")+ 
  theme(plot.title = element_text(hjust = 0.5))
