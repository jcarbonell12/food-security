library(ipumsr)
library(dplyr)
library(purrr)

# set IPUMS api key
set_ipums_api_key("59cba10d8a5da536fc06b59d28775af426b2401f984d3609c6bda512", save = TRUE)

# define CPS-Food Sec Extract
fs_extract <- define_extract_micro(
  collection = "cps",
  description = "CPS Food Security Supp Extract",
  samples = c("cps2022_12s", "cps2021_12s"),
  variables = c("YEAR", 
                "SERIAL", 
                "MONTH", 
                "HWTFINL", 
                "CPSID",
                "MISH",
                "NUMPREC",
                "REGION",
                "STATEFIP",
                "COUNTY",
                "METFIPS",
                "FAMINC",
                "FSSUPINT",
                "FSHWTSCALE",
                "FSSTATUS",
                "FSRAWSCR",
                "FSSTATUSD",
                "FSSTATUSA",
                "FSSTATUSC",
                "FSSTMPVALC",
                "FSSUPPWTH",
                "FSSUPPWT")) %>% 
  submit_extract() %>% 
  wait_for_extract() %>% 
  download_extract(download_dir = "ds-projects/food-security/data-raw",
                   overwrite = TRUE) %>% 
  read_ipums_micro() %>% 
  # filter for just respondents to food security supplement and dedup on household
  filter(FSSUPINT == 1 & YEAR == 2022) %>% 
  distinct(SERIAL, .keep_all=T)

fwrite(fs, "ds-projects/food-security/data-raw/fs-extract-2022.csv")
