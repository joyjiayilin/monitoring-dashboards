

library(dplyr)

box::use(
  magrittr[`%>%`, `%<>%`]
)




# MMP

# Exec summary datasets ----
#total gaps
sql_query <- 'SELECT * FROM "nrdp_mmp"."mmp_all_exec_total_gaps"'

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  host = "csc2cwn00020453.cloud.kp.org",
  dbname = "npdm",
  user = Sys.getenv("POSTUSER"),
  password = Sys.getenv("POSTGRES"),
  port = 5432
)

res <- DBI::dbSendQuery(con, statement = sql_query)

mmp_exec_gaps <-  DBI::dbFetch(res) 

mmp_exec_gaps$lob <- 'Medicare'

#regional summary gaps
sql_query <- glue::glue('SELECT * FROM "nrdp_mmp"."mmp_all_exec_regional_gaps"')

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  host = "csc2cwn00020453.cloud.kp.org",
  dbname = "npdm",
  user = Sys.getenv("POSTUSER"),
  password = Sys.getenv("POSTGRES"),
  port = 5432
)

res <- DBI::dbSendQuery(con, statement = sql_query)

mmp_reg_gaps <- DBI::dbFetch(res) %>% 
  rename(Specialties = 'specialties',
         Counties = 'counties',
         Type = 'type',
         Region = 'region',
         Total = 'total') %>% 
  dplyr::mutate(kp_region = dplyr::case_when(Region == 'CO' ~  'Colorado',
                                             Region == 'GA' ~  'Georgia',
                                             Region == 'HI' ~  'Hawaii',
                                             Region == 'MAS' ~  'Mid Atlantic States',
                                             Region == 'NCAL' ~  'Northern California',
                                             Region == 'NW' ~  'Northwest',
                                             Region == 'SCAL' ~  'Southern California',
                                             Region == 'WA' ~  'Washington'
  ))

mmp_reg_gaps$lob <- 'Medicare'

sql_query <- 'SELECT * FROM "nrdp_mmp"."mmp_all_regions_full"'

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  host = "csc2cwn00020453.cloud.kp.org",
  dbname = "npdm",
  user = Sys.getenv("POSTUSER"),
  password = Sys.getenv("POSTGRES"),
  port = 5432
)

res <- DBI::dbSendQuery(con, statement = sql_query)
#pulling data and minor cleanup
mmp_regional_dat <- DBI::dbFetch(res) 
mmp_regional_dat <-  mmp_regional_dat %>% 
  rename(Specialty = 'specialty',
         State = 'state',
         County = 'county',
         Type = 'type') %>% 
  mutate(kp_region = case_when(kp_region == 'CO' ~  'Colorado',
                               kp_region == 'GA' ~  'Georgia',
                               kp_region == 'HI' ~  'Hawaii',
                               kp_region == 'MAS' ~  'Mid Atlantic States',
                               kp_region == 'NCAL' ~  'Northern California',
                               kp_region == 'NW' ~  'Northwest',
                               kp_region == 'SCAL' ~  'Southern California',
                               kp_region == 'WA' ~  'Washington'
  ),
  Specialty = case_when(Specialty == "Oncology - Radiation/Radiation Oncology" ~ "Oncology Radiation",
                        Specialty == "Oncology - Medical, Surgical" ~ "Oncology Med/Surg",
                        Specialty == "Physiatry, Rehabilitative Medicine" ~ "Physiatry/Rehab Med",
                        Specialty == "Gynecology, OB/GYN" ~ "Gynecology/OBGYN",
                        Specialty == "Critical Care Services – Intensive Care Units (ICU)" ~ "Critical Care Services",
                        Specialty == 'Surgical Services (Outpatient or ASC)' ~ "Surgical Services",
                        Specialty == 'Inpatient Psychiatric Facility Services' ~ "Inpatient Psych Fac",
                        Specialty == 'Outpatient Infusion/Chemotherapy' ~ 'Outpatient Inf/Chemo',
                        Specialty == 'Cardiac Catheterization Services' ~ 'Cardiac Catheterization',
                        Specialty == 'Allergy and Immunology' ~ 'Allergy/Immunology',
                        Specialty == 'Acute Inpatient Hospitals' ~'Acute Inpatient Hosp',
                        Specialty == 'Skilled Nursing Facilities' ~ 'Skilled Nursing Fac',
                        Specialty == 'Cardiac Surgery Program' ~ 'Cardiac Surgery',
                        Specialty == 'Cardiology' ~ 'Cardiology', #TODO tester for table build
                        TRUE ~ Specialty),
  County = trimws(County, whitespace = ".* - "),
  County = gsub(" (Medicare Partial*)","", County, fixed=T)
  ) %>%
  arrange(quarter)

mmp_regional_dat$lob <- 'Medicare'




# QHP

sql_query <- 'SELECT * FROM "nrdp_qhp"."qhp_all_exec_total_gaps"'

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  host = "csc2cwn00020453.cloud.kp.org",
  dbname = "npdm",
  user = Sys.getenv("POSTUSER"),
  password = Sys.getenv("POSTGRES"),
  port = 5432
)

res <- DBI::dbSendQuery(con, statement = sql_query)

qhp_exec_gaps <-  DBI::dbFetch(res) 

qhp_exec_gaps$lob <- 'QHP'

#regional summary gaps
sql_query <- glue::glue('SELECT * FROM "nrdp_qhp"."qhp_all_exec_regional_gaps"')

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  host = "csc2cwn00020453.cloud.kp.org",
  dbname = "npdm",
  user = Sys.getenv("POSTUSER"),
  password = Sys.getenv("POSTGRES"),
  port = 5432
)

res <- DBI::dbSendQuery(con, statement = sql_query)

qhp_reg_gaps <-  DBI::dbFetch(res)

qhp_reg_gaps$lob <- 'QHP'

#pulling full dataset to get all gap types
sql_query <- 'SELECT * FROM "nrdp_qhp"."qhp_all_regions_full"'

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  host = "csc2cwn00020453.cloud.kp.org",
  dbname = "npdm",
  user = Sys.getenv("POSTUSER"),
  password = Sys.getenv("POSTGRES"),
  port = 5432
)

res <- DBI::dbSendQuery(con, statement = sql_query)
#pulling data and minor cleanup
qhp_regional_dat <- DBI::dbFetch(res) %>%
  rename(State = 'state',
         County = 'county',
         Specialty = 'specialty',
         Type = 'type')

qhp_regional_dat$lob <- 'QHP'
