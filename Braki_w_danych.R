Data <- read.csv("application_data_new.csv")

install.packages("naniar")
library(naniar)

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
gg_miss_upset(Data,
              nsets = 122)