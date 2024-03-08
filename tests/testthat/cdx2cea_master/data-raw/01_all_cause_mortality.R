### code to prepare `all_cause_mortality` dataset goes here
library(dplyr)
file.init <- "data-raw/USA_bltper_1x1.csv" # Source: https://usa.mortality.org/uploads/lifetables/Nationals/USA/USA_bltper_1x1.csv

df_hmd_USA  <- read.csv(file = file.init, stringsAsFactors = F)

df_hmd_USA_2018 <- df_hmd_USA %>%
  dplyr::mutate(Age = as.numeric(Age)) %>%
  dplyr::filter(Year == 2018, Age <= 100) %>%
  dplyr::select(Year, Age, Total = mx)
df_hmd_USA_2018$Total[df_hmd_USA_2018$Age == 100] <- 1

all_cause_mortality <- df_hmd_USA_2018

# Create .rda object for initial set of parameters and store it in 'data' folder
usethis::use_data(all_cause_mortality, overwrite = TRUE)
