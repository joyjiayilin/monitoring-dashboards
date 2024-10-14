
library(dplyr)

# pull in release notes
notes <- read.csv(paste0(getwd(),"/release_notes.csv"))
colnames(notes) <- c('region', 'notes')


# QHP

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


# MMP

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

mmp_regional_dat$lob <- 'MMP'



## address summary
sql_query <- 'SELECT * FROM "nrdp_mmp"."mmp_address_summary"'

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
mmp_address_summary <- DBI::dbFetch(res)

mmp_address_summary$lob <- 'MMP'


# MMP + QHP

combined_dat <- bind_rows(qhp_regional_dat, mmp_regional_dat)









