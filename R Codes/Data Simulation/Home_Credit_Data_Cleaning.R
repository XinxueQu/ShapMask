#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# READ ME ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# This is the file for cleaning the original Home Credit Data from Kaggle.
# Users can skip this code and use the simulated data that we generated from this file
# The original data can be found using: 
## https://www.kaggle.com/competitions/home-credit-credit-risk-model-stability/data
# The final simulated data can be found here:
## https://www.kaggle.com/datasets/shawnqu/data-for-shapmask?select=home_credit_synthetic_data_for_sensitivity.csv

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("../install_load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Columns of interest
selected_columns <- c("case_id", "postype_4733339M", "actualdpd_943P",
                      "annuity_853A", "credacc_actualbalance_314A",
                      "credamount_590A", "mainoccupationinc_437A", "instlamount_768A",
                      "instlamount_852A", "purposeofcred_426M", "description_351M",
                      "debtoverdue_47A",
                      "contractsum_5085717L", "debtoutstand_525A",
                      "monthlyinstlamount_332A", "numberofcontrsvalue_358L",
                      "numberofoutstandinstls_59L", "outstandingamount_362A",
                      "totalamount_6A", "totalamount_996A", "totaloutstanddebtvalue_39A",
                      "amount_1115A", "credquantity_1099L", "credquantity_984L",
                      "debtvalue_227A", "instlamount_892A", "birth_259D", "age",
                      "sex_738L", "addres_zip_823M", "education_927M", "language1_981M",
                      "maininc_215A", "lastst_736L", "twobodfilling_608L",
                      "amtinstpaidbefduel24m_4187115A", "downpmt_116A", "numactivecreds_622L",
                      "numinstls_657L", "numinstlsallpaid_934L", "annuity_780A",
                      "applicationscnt_867L", "avginstallast24m_3658937A",
                      "avglnamtstart24m_4525187A", "avgpmtlast12m_4525200A",
                      "credamount_770A", "disbursedcredamount_1113A",
                      "inittransactionamount_650A", "lastapprcredamount_781A",
                      "lastrejectcredamount_222A", "maxannuity_4075009A",
                      "maxdebt4_972A", "maxdpdinstlnum_3546846P",
                      "maxinstallast24m_3658928A", "maxlnamtstart6m_4525199A",
                      "maxpmtlast3m_4525190A", "monthsannuity_845L",
                      "numincomingpmts_3546848L", "numinstlallpaidearly3d_817L",
                      "numinstlswithoutdpd_562L", "numinstmatpaidtearly2d_4499204L",
                      "numinstpaid_4499208L", "numinstpaidearly3d_3546850L",
                      "numinstpaidearly3dest_4493216L", "price_1097A",
                      "totinstallast1m_4525188A", "maritalst_385M", "days30_165L",
                      "for3years_584L", "numberofqueries_373L")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dir_name <- "home-credit-credit-risk-model-stability/csv_files/train/"
setwd(dir_name)

#...............................................................................
# train_applprev ####
#...............................................................................

train_applprev_1_0 <- read_csv("train_applprev_1_0.csv")
train_applprev_1_1 <- read_csv("train_applprev_1_1.csv")
train_applprev_2 <- read_csv("train_applprev_2.csv")

train_applprev_1_c <- bind_rows(train_applprev_1_0, train_applprev_1_1) %>%
  filter(num_group1 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

train_applprev_2_c <- train_applprev_2 %>%
  filter(num_group1 == 0 & num_group2 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

train_applprev <- inner_join(train_applprev_1_c, train_applprev_2_c) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) # remove constant columns

rm(train_applprev_1_0, train_applprev_1_1, train_applprev_1_c,
   train_applprev_2, train_applprev_2_c)

#...............................................................................
# train_base ####
#...............................................................................
train_base <- read_csv("train_base.csv") %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

#...............................................................................
# train_credit_bureau ####
#...............................................................................

## train_credit_bureau_a ####
### train_credit_bureau_a_1 ####

file_names <- list.files(path = dir_name,
                         pattern = "train_credit_bureau_a_1_\\d*.csv",
                         full.names = TRUE)
train_credit_bureau_a_1 <- lapply(file_names, read_csv)
train_credit_bureau_a_1_c <- do.call("bind_rows", train_credit_bureau_a_1) %>%
  filter(num_group1 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

rm(file_names, train_credit_bureau_a_1)

### train_credit_bureau_a_2 ####
file_names <- list.files(path = dir_name,
                         pattern = "train_credit_bureau_a_2_\\d*.csv",
                         full.names = TRUE)
train_credit_bureau_a_2 <- lapply(file_names, read_csv)
train_credit_bureau_a_2_c <- do.call("bind_rows", train_credit_bureau_a_2) %>%
  filter(num_group1 == 0 & num_group2 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

rm(file_names, train_credit_bureau_a_2)


## train_credit_bureau_b ####
train_credit_bureau_b_1 <- read_csv("train_credit_bureau_b_1.csv") %>%
  filter(num_group1 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

train_credit_bureau_b_2 <- read_csv("train_credit_bureau_b_2.csv") %>%
  filter(num_group1 == 0 & num_group2 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

train_credit_bureau_b_c <- inner_join(train_credit_bureau_b_1,
                                      train_credit_bureau_b_2)

rm(train_credit_bureau_b_1, train_credit_bureau_b_2)


train_credit_bureau <- train_credit_bureau_a_1_c %>%
  inner_join(train_credit_bureau_a_2_c) %>%
  inner_join(train_credit_bureau_b_c) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) # remove constant columns

rm(train_credit_bureau_a_1_c, train_credit_bureau_a_2_c, train_credit_bureau_b_c)

#...............................................................................
# train_debitcard ####
#...............................................................................

train_debitcard <- read_csv("train_debitcard_1.csv") %>%
  filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

#...............................................................................
# train_deposit ####
#...............................................................................

train_deposit <- read_csv("train_deposit_1.csv") %>%
  filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

#...............................................................................
# train_other ####
#...............................................................................

train_other <- read_csv("train_other_1.csv") %>%
  filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

#...............................................................................
# train_person ####
#...............................................................................

file_names <- list.files(path = dir_name, pattern = "train_person_\\d*.csv", full.names = TRUE)
train_person_list <- lapply(file_names, read_csv)
train_person <- do.call("inner_join", train_person_list) %>%
  filter(num_group1 == 0 & num_group2 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

rm(file_names, train_person_list)

#...............................................................................
# train_static ####
#...............................................................................

## train_static_0 ####
file_names <- list.files(path = dir_name, pattern = "train_static_0_\\d*.csv", full.names = TRUE)
train_static_0_list <- lapply(file_names, read_csv)
train_static_0 <- do.call("bind_rows", train_static_0_list) %>%
  # filter(num_group1 == 0) %>%
  dplyr::select(case_id, any_of(selected_columns))

rm(file_names, train_static_0_list)

## train_static_cb ####
train_static_cb <- read_csv("train_static_cb_0.csv") %>%
  # filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))


train_static <- train_static_0 %>%
  inner_join(train_static_cb) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE)

rm(train_static_0, train_static_cb)


#...............................................................................
# train_tax_registry ####
#...............................................................................
train_tax_registry_a_1 <- read_csv("train_tax_registry_a_1.csv") %>%
  filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

train_tax_registry_b_1 <- read_csv("train_tax_registry_b_1.csv") %>%
  filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

train_tax_registry_c_1 <- read_csv("train_tax_registry_c_1.csv") %>%
  filter(num_group1 == 0) %>%
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE) %>% # remove constant columns
  dplyr::select(case_id, any_of(selected_columns))

train_tax_registry <- train_tax_registry_c_1

rm(train_tax_registry_a_1, train_tax_registry_b_1, train_tax_registry_c_1)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Merge All Data ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

all_data_combined <- train_applprev %>%
  # inner_join(train_base) %>% # has only one column
  inner_join(train_credit_bureau) %>%
  # inner_join(train_debitcard) %>% # has only one column
  # inner_join(train_deposit) %>% # has only one column
  # inner_join(train_other) %>% # has only one column
  inner_join(train_person) %>%
  inner_join(train_static) %>%
  # inner_join(train_tax_registry) %>% # has only one column
  remove_empty() %>% # Remove empty columns and rows
  remove_constant(na.rm = TRUE)


all_data_clean <- all_data_combined %>%
  rename(birth_date = birth_259D,
         sex = sex_738L,
         zip_code = addres_zip_823M,
         marital_status = maritalst_385M,
         credit_amt = amount_1115A,
         income = maininc_215A,
         install_amt = instlamount_768A,
         num_credit = credquantity_1099L) %>%
  mutate(install_amt = ifelse(is.na(install_amt), instlamount_852A, install_amt),
         birth_date = lubridate::ymd(birth_date),
         age = floor(lubridate::time_length(difftime("2024-05-31", birth_date), "years"))) %>%
  relocate(age, .after = birth_date) %>%
  dplyr::select(-instlamount_852A)

all_data_na_omit <- all_data_clean %>%
  na.omit()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Save Data ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

write_csv(all_data_clean, "home_credit_data_clean.csv")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::