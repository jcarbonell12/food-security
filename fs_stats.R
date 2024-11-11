library(data.table)
library(tidyveerse)

fs = fread("ds-projects/food-security/data-raw/fs-extract-2022.csv")

# overall household food sec status - Western Region
# --> % of food insec household should match last row of Table 2 in food sec report
#     https://www.ers.usda.gov/webdocs/publications/107703/err-325.pdf?v=601.6
food_sec_region = fs %>% 
  filter(FSSTATUSD != 98 & (REGION == 41 | REGION == 42)) %>% 
  group_by(FSSTATUSD) %>% 
  summarise(n_hh = sum(FSSUPPWT),
            n_respondent = n()) %>% 
  ungroup()

# check that % of food insecure households is 11.2% in Western Region
sum(food_sec_status$n_hh[food_sec_status$FSSTATUSD>=3]) / sum(food_sec_status$n_hh)


food_sec_CA = fs %>% 
  filter(FSSTATUSD != 98 & STATEFIP == 6) %>% 
  group_by(FSSTATUSD) %>% 
  summarise(n_hh = sum(FSSUPPWT),
            n_respondent = n()) %>% 
  ungroup()

n_CA_hh = sum(food_sec_CA$n_hh)

food_sec_CA = food_sec_CA %>% 
  mutate(pct = n_hh / n_CA_hh)

fs = fs %>% 
  mutate(
    fs_val = case_when(
      FSSTMPVALC >= 998 ~ 0,
      FSSTMPVALC == 997 ~ mean(fs$FSSTMPVALC[fs$FSSTMPVALC <= 900]) / 100,
      FSSTMPVALC == 996 ~ 0,
      .default = FSSTMPVALC / 100),
    fs_receive = ifelse(fs_val > 0, 1, 0),
    food_insecure = ifelse(FSSTATUSD==3 | FSSTATUSD==4, 1, 0),
    v_food_insecure = ifelse(FSSTATUSD==4, 1, 0))

fs = fs %>% 
  mutate(
    fam_income = case_when(
      FAMINC == 100 ~ runif(nrow(fs), 0, 4999),
      FAMINC == 210 ~ runif(nrow(fs), 5000, 7499),
      FAMINC == 300 ~ runif(nrow(fs), 7500, 9999),
      FAMINC == 430 ~ runif(nrow(fs), 10000, 12499),
      FAMINC == 470 ~ runif(nrow(fs), 12500, 14999),
      FAMINC == 500 ~ runif(nrow(fs), 15000, 19999),
      FAMINC == 600 ~ runif(nrow(fs), 20000, 24999),
      FAMINC == 710 ~ runif(nrow(fs), 25000, 29999),
      FAMINC == 720 ~ runif(nrow(fs), 30000, 34999),
      FAMINC == 730 ~ runif(nrow(fs), 35000, 39999),
      FAMINC == 740 ~ runif(nrow(fs), 40000, 49999),
      FAMINC == 820 ~ runif(nrow(fs), 50000, 59999),
      FAMINC == 830 ~ runif(nrow(fs), 60000, 74999),
      FAMINC == 841 ~ runif(nrow(fs), 75000, 99999),
      FAMINC == 842 ~ runif(nrow(fs), 100000, 149999),
      FAMINC == 843 ~ runif(nrow(fs), 150000, 1000000),
      .default = NA
    ))



mod = lm(food_insecure ~ fs_val + fam_income, data=fs)
summary(mod)
typeof(fs$FAMINC)
