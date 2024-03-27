library(DBI)
library(dplyr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(tools)
library(readr)
library(readxl)

# Connect to db
con <- dbConnect(RPostgres::Postgres(),dbname = 'WGS_DB', 
                 host = '10.10.25.163', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'secret123')

# list of query
setwd("E:/Google Drive/referred/R")

# Specify the path to your text file
file_path <- "kpn_2022_colistin.txt"

# Read the IDs from the text file
id_list <- scan(file_path, what = "")

#list of query for txt file
#id_list <- scan("to_query.txt", character(), quote = "")

# Escape single quotes in the strings
id_list <- gsub("'", "''", id_list)

# Convert the list of IDs to a comma-separated string enclosed in single quotes
id_string <- paste0("'", paste(id_list, collapse = "','"), "'")


query <- paste("SELECT * from wgs_app_referreddb 
               WHERE wgs_app_referreddb.sample_name IN (", id_string, ")", sep="")

df <- dbSendQuery(con, query)

setwd("E:/Google Drive/referred/R")

result <- dbFetch(df)
result <- result %>% mutate_all(as.character)

result <- result %>% mutate_all(~as.character(ifelse(. == "nan", "", .)))

#result <- result %>% mutate_all(~ifelse(is.na(.) | . == "nan", "", .))

#result <- data.frame(lapply(result, function(x) ifelse(is.na(x) | is.nan(x), "", x)))

# Define the columns to check
carba_to_check <- c("ipm_nd10_ris", "ipm_nm_ris", "mem_nd10_ris", "mem_nm_ris", 
                      "etp_nd10_ris", "etp_nm_ris", "dor_nd10_ris", "dor_nm_ris")

# Loop through the columns and set values in df$carba based on conditions
for (col in carba_to_check) {
  result$carba[result[[col]] %in% c("R", "I")] <- 1
  result$carba[result[[col]] == "S"] <- 0
}

# If df$carba is still NA, set it to 0
result$carba[is.na(result$carba)] <- 0

#get laboratory
result$laboratory <- substr(result$sample_name, 7, 9)
result$laboratory  <- toupper(result$laboratory)


#change date
result$spec_date <- as.Date(result$spec_date, format = "%Y-%m-%d")
result$date_admis <- as.Date(result$date_admis, format = "%Y-%m-%d")

result$date_admis[result$date_admis %in% as.Date(c("1977-07-07", "1999-09-09"))] <- ""


# Calculate the difference in days between spec_date and date_admis
result$noso <- as.numeric(difftime(result$spec_date, result$date_admis, units = "days"))

result$infection <- ifelse(result$noso > 2 | result$noso == "", "HAI", "CAI")
result$infection[is.na(result$date_admis)] <- "CAI"



# Extract year, month, and day as before
result$year <- year(result$spec_date)
result$month <- month(result$spec_date)
result$day <- day(result$spec_date)


df <- result


#specimen type define
sp_type <- read.xlsx("E:/Google Drive/whonet_codes/whonet_codes.xlsx", sheet = "SPECIMEN")

#get first 2 chars of sp type
df$spec_type <- substr(df$spec_type, 1, 2)

df <- df %>%
  left_join(sp_type, by = c("spec_type" = "SPEC_TYPE")) %>%
  mutate(specimen_type = coalesce(ENGLISH, spec_type))

# location define
lab_info <- read.xlsx("E:/Google Drive/whonet_codes/whonet_codes.xlsx", sheet = "LABORATORY")
lab_info$LATITUDE <- as.character(lab_info$LATITUDE)
lab_info$LONGITUDE <- as.character(lab_info$LONGITUDE)

df <- df %>%
  left_join(lab_info, by = c("laboratory" = "LABORATORY")) %>%
  mutate(
    latitude = coalesce(LATITUDE, laboratory),
    longitude = coalesce(LONGITUDE, laboratory),
    island = coalesce(ISLAND, laboratory),
    region = coalesce(REGION, laboratory)
  )

#ward define
ward_info <- read.csv("E:/Google Drive/whonet_codes/whonet_location_combined.csv")

ward_info <- select(ward_info, WARD,DEPARTMENT,WARD_TYPE)

ward_info <- distinct(ward_info, WARD, .keep_all = TRUE)

df <- merge(df, ward_info, by.x = "ward", by.y = "WARD", all.x = TRUE)

# Use coalesce to fill missing values
df$department <- coalesce(df$DEPARTMENT, df$department)
df$ward_type <- coalesce(df$WARD_TYPE, df$ward_type)

# Function to process antibiotics
process_antibiotic <- function(df, main_col, nm_ris_col, nd_col) {
  df <- df %>%
    mutate({{main_col}} := case_when(
      get({{nm_ris_col}}) %in% c("i", "r","ns","R","I","NS") ~ "R",
      get({{nm_ris_col}}) %in% c("s","S") ~ "S",
      is.na(get({{nm_ris_col}})) | get({{nm_ris_col}}) == "" ~
        case_when(
          get({{nd_col}}) %in% c("i", "r","ns","R","I","NS") ~ "R",
          get({{nd_col}}) %in% c("s","S") ~ "S",
          TRUE ~ ""
        ),
      TRUE ~ ""
    ))
  
  return(df)
}

# Process antibiotics
df <- process_antibiotic(df, "amp", "amp_nm_ris", "amp_nd10_ris")
df <- process_antibiotic(df, "fox", "fox_nm_ris", "fox_nd30_ris")
df <- process_antibiotic(df, "cxa", "cxa_nm_ris", "cxa_nd30_ris")
df <- process_antibiotic(df, "caz", "caz_nm_ris", "caz_nd30_ris")
df <- process_antibiotic(df, "cro", "cro_nm_ris", "cro_nd30_ris")
df <- process_antibiotic(df, "fep", "fep_nm_ris", "fep_nd30_ris")
df <- process_antibiotic(df, "ipm", "ipm_nm_ris", "ipm_nd10_ris")
df <- process_antibiotic(df, "mem", "mem_nm_ris", "mem_nd10_ris")
df <- process_antibiotic(df, "etp", "etp_nm_ris", "etp_nd10_ris")
df <- process_antibiotic(df, "amc", "amc_nm_ris", "amc_nd20_ris")
df <- process_antibiotic(df, "tzp", "tzp_nm_ris", "tzp_nd100_ris")
df <- process_antibiotic(df, "gen", "gen_nm_ris", "gen_nd10_ris")
df <- process_antibiotic(df, "amk", "amk_nm_ris", "amk_nd30_ris")
df <- process_antibiotic(df, "tob", "tob_nm_ris", "tob_nd10_ris")
df <- process_antibiotic(df, "cip", "cip_nm_ris", "cip_nd5_ris")
df <- process_antibiotic(df, "sxt", "sxt_nm_ris", "sxt_nd1_2_ris")

columns <- c("amp", "fox","cxa","caz","cro","fep","ipm","mem","etp", "amc","tzp","gen","amk","tob","cip","sxt")

#Resistance profile
for (col in columns) {
  df[[paste0("RP_", col)]] <- ifelse(df[[col]] %in% c("R", "NS"), col,
                                     ifelse(df[[col]] == "S", "   ", "---"))
}

df$RP <- do.call(paste, c(df[grep("^RP_", names(df))], sep = " "))
df$RP <- toupper(df$RP)

# Make sample_name values uppercase
df$sample_name <- toupper(df$sample_name)

# Replace underscores with hyphens
df$sample_name <- gsub("_", "-", df$sample_name)


df_meta <- df[, c("sample_name", "arsrl_org","laboratory",
              "island","region","latitude","longitude",
              "patient_id",
              "age", "sex",
              "spec_num","specimen_type",
              "spec_date", "year","month","day",
              "date_admis","noso", "infection",
              "nosocomial",
              "ward","department","ward_type",
              "amp_nd10","amp_nd10_ris","amp_nm","amp_nm_ris",
              "fox_nd30","fox_nd30_ris","fox_nm","fox_nm_ris",
              "cxa_nd30","cxa_nd30_ris","cxa_nm","cxa_nm_ris",
              "caz_nd30","caz_nd30_ris","caz_nm","caz_nm_ris",
              "cro_nd30","cro_nd30_ris","cro_nm","cro_nm_ris",
              "fep_nd30","fep_nd30_ris","fep_nm","fep_nm_ris",
              "ipm_nd10","ipm_nd10_ris","ipm_nm","ipm_nm_ris",
              "mem_nd10","mem_nd10_ris","mem_nm","mem_nm_ris",
              "etp_nd10","etp_nd10_ris", "etp_nm","etp_nm_ris",
              "dor_nd10","dor_nd10_ris","dor_nm","dor_nm_ris",
              "amc_nd20","amc_nd20_ris","amc_nm","amc_nm_ris",
              "tzp_nd100","tzp_nd100_ris","tzp_nm","tzp_nm_ris",
              "gen_nd10","gen_nd10_ris","gen_nm","gen_nm_ris",
              "amk_nd30","amk_nd30_ris","amk_nm","amk_nm_ris",
              "tob_nd10","tob_nd10_ris","tob_nm","tob_nm_ris",
              "cip_nd5","cip_nd5_ris","cip_nm","cip_nm_ris",
              "sxt_nd1_2","sxt_nd1_2_ris","sxt_nm","sxt_nm_ris",
              "carba",
              "col_nd10","col_nd10_ris","col_nm","col_nm_ris",
              "RP"
              )]



file_path_without_ext <- file_path_sans_ext(file_path)
timestamp <- format(Sys.time(), "%Y%m%d%H%M")
write.xlsx(df_meta, paste0(timestamp, "_",file_path_without_ext,"_listing.xlsx"), rowNames = TRUE)

#add WGS data
file_path <- "E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/terra_col_resistant_kpn_37.tsv"
df_terra <- read_tsv(file_path, col_types = cols(.default = "c"))

df_terra <- df_terra %>%
  # Remove special characters and spaces, convert to lowercase
  setNames(gsub("[^[:alnum:]]", "_", tolower(names(.))))

df_terra <- select(df_terra,entity_col_resistant_kpn_37_id,
                   ts_mlst_predicted_st,kleborate_mlst_sequence_type)

colnames(df_terra) <- paste0(colnames(df_terra), "_terra")


file_path <- "E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/kleborate_all .csv"
df_pw_kleborate <- read.csv(file_path)
df_pw_kleborate <- select(df_pw_kleborate,
                          Genome.Name,species,ST)

colnames(df_pw_kleborate) <- paste0(colnames(df_pw_kleborate), "_pwkl")

#pathogenwatch data
pw_stats_kpn <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-klepn-p3pinbzvzccw-k-pneumoniae-col-resistant-stats.csv")
pw_stats_kpq <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-kleqp-8sqgpfognyuk-k-quasipneumoniae-col-resistant-stats.csv")
pw_stats <- bind_rows(pw_stats_kpn,pw_stats_kpq)
pw_stats <- pw_stats %>% select(-CORE.MATCHES:-X..GC.CONTENT)

pw_typing_kpn <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-klepn-p3pinbzvzccw-k-pneumoniae-col-resistant-typing.csv")
pw_typing_kpq <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-kleqp-8sqgpfognyuk-k-quasipneumoniae-col-resistant-typing.csv")
pw_typing <- bind_rows(pw_typing_kpn,pw_typing_kpq)
pw_typing <- pw_typing %>% select(-MLST.ST..Pasteur.:-LINCODE)

pw_amr_profile_kpn <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-klepn-p3pinbzvzccw-k-pneumoniae-col-resistant-amr-profile.csv")
pw_amr_profile_kpq <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-kleqp-8sqgpfognyuk-k-quasipneumoniae-col-resistant-amr-profile.csv")
pw_amr_profile <- bind_rows(pw_amr_profile_kpn,pw_amr_profile_kpq)

pw_amr_genotypes_kpn <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-klepn-p3pinbzvzccw-k-pneumoniae-col-resistant-amr-genotypes.csv")
pw_amr_genotypes_kpq <- read.csv("E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/pw/pathogenwatch-kleqp-8sqgpfognyuk-k-quasipneumoniae-col-resistant-amr-genotypes.csv")
pw_amr_genotypes <- bind_rows(pw_amr_genotypes_kpn,pw_amr_genotypes_kpq)
pw_amr_genotypes[is.na(pw_amr_genotypes) | pw_amr_genotypes == ""] <- 0

merged_data <- merge(pw_stats, pw_typing, by = "NAME")
merged_data <- merge(merged_data, pw_amr_profile, by = "NAME")
merged_data <- merge(merged_data, pw_amr_genotypes, by = "NAME")
pw_results <- merged_data %>%
  # Remove special characters and spaces, convert to lowercase
  setNames(gsub("[^[:alnum:]]", "_", tolower(names(.)))) %>%
  # Remove extra underscores
  select(setNames(names(.), gsub("_+", "_", names(.))))



# write pw results
output_folder <- "E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/R_output"
output_file_path <- file.path(output_folder, paste0("pw_results","_", timestamp,".xlsx"))
write.xlsx(pw_results, output_file_path, rowNames = TRUE)

#merge
df <- merge(df_meta, df_terra, by.x = "sample_name", by.y = "entity_col_resistant_kpn_37_id_terra", all.x = TRUE)
df <- merge(df, df_pw_kleborate, by.x = "sample_name", by.y = "Genome.Name_pwkl", all.x = TRUE)


#select df meta
df <- df[, c("sample_name", "arsrl_org","laboratory",
                  "island","region","latitude","longitude",
                  "patient_id",
                  "age", "sex",
                  "spec_num","specimen_type",
                  "spec_date", "year","month","day",
                  "date_admis", "infection",
                  "ward","department","ward_type",
                  "amp_nd10","amp_nd10_ris","amp_nm","amp_nm_ris",
                  "fox_nd30","fox_nd30_ris","fox_nm","fox_nm_ris",
                  "cxa_nd30","cxa_nd30_ris","cxa_nm","cxa_nm_ris",
                  "caz_nd30","caz_nd30_ris","caz_nm","caz_nm_ris",
                  "cro_nd30","cro_nd30_ris","cro_nm","cro_nm_ris",
                  "fep_nd30","fep_nd30_ris","fep_nm","fep_nm_ris",
                  "ipm_nd10","ipm_nd10_ris","ipm_nm","ipm_nm_ris",
                  "mem_nd10","mem_nd10_ris","mem_nm","mem_nm_ris",
                  "etp_nd10","etp_nd10_ris", "etp_nm","etp_nm_ris",
                  "dor_nd10","dor_nd10_ris","dor_nm","dor_nm_ris",
                  "amc_nd20","amc_nd20_ris","amc_nm","amc_nm_ris",
                  "tzp_nd100","tzp_nd100_ris","tzp_nm","tzp_nm_ris",
                  "gen_nd10","gen_nd10_ris","gen_nm","gen_nm_ris",
                  "amk_nd30","amk_nd30_ris","amk_nm","amk_nm_ris",
                  "tob_nd10","tob_nd10_ris","tob_nm","tob_nm_ris",
                  "cip_nd5","cip_nd5_ris","cip_nm","cip_nm_ris",
                  "sxt_nd1_2","sxt_nd1_2_ris","sxt_nm","sxt_nm_ris",
                  "col_nm","col_nm_ris",
                  "RP","ts_mlst_predicted_st_terra",
             "species_pwkl"
)]

df <- merge(df, pw_results, by.x = "sample_name", by.y = "name", all.x = TRUE)

# Define the output folder path
output_folder <- "E:/Google Drive/ARSRL_WGS/colistin_kpn/colistin_kpn_37/R_output"

# Construct the full output file path
output_file_path <- file.path(output_folder, paste0(timestamp, "_", file_path_without_ext, "_metadata.xlsx"))

# Write the dataframe to Excel in the specified output folder
write.xlsx(df, output_file_path, rowNames = TRUE)

