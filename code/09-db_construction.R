# R-script db_construction.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf

# Method of Reflections is the one used by Hidalgo and Hausmann

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)
mongo_credentials <- config::get(file = "conf/globalresources.yml")

# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(reticulate)){install.packages("reticulate")}
if(!require(vroom)){install.packages("vroom")}
if(!require(economiccomplexity)){install.packages("economiccomplexity")}


# Functions ---------------------------------------------------------------

source(file = "code/functions/data_loc.R")
source(file = "code/functions/fct_insertmongodb.R")
source(file = "code/functions/fct_add_eci.R")

# Code --------------------------------------------------------------------

# Getting BR location info
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  dplyr::distinct()

# Loading exp data
exp <- vroom::vroom(file = "data/EXP_COMPLETA_MUN2/EXP_COMPLETA_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(exp_fob=dplyr::if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  dplyr::mutate("cd_sh2" = substr(sh4, 1, 2)) %>%
  dplyr::rename(
    "sg_uf"="sg_uf_mun",
    "cd_mun"="co_mun"
  ) %>% 
  dplyr::group_by(cd_mun, sg_uf, cd_sh2) %>%
  dplyr::summarise(exp = sum(exp_fob)) %>% 
  dplyr::ungroup()

# Fixing those dumb ass mistakes
exp[which(exp$sg_uf=="SP"), "cd_mun"] = exp[which(exp$sg_uf=="SP"), "cd_mun"]+100000 # SP
exp[which(exp$sg_uf=="GO"), "cd_mun"] = exp[which(exp$sg_uf=="GO"), "cd_mun"]-100000 # GO
exp[which(exp$sg_uf=="MS"), "cd_mun"] = exp[which(exp$sg_uf=="MS"), "cd_mun"]-200000 # MS
exp[which(exp$sg_uf=="DF"), "cd_mun"] = exp[which(exp$sg_uf=="DF"), "cd_mun"]-100000 # DF

exp2 <- exp %>% 
  dplyr::mutate(cd_mun=as.character(cd_mun)) %>% 
  dplyr::left_join(., br_loc, by = c("cd_mun", "sg_uf")) %>% 
  stats::na.omit() %>% 
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_sh2, exp)

exp_t <- exp2 %>% 
  dplyr::group_by(cd_mun, sg_uf) %>% 
  dplyr::summarise(exp=sum(exp)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cd_sh2="00") %>%
  dplyr::left_join(., br_loc, by = c("cd_mun", "sg_uf")) %>% 
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_sh2, exp)

colec_mun <- dplyr::bind_rows(exp2, exp_t) %>% 
  dplyr::mutate(cd_sh2=paste0("sh", cd_sh2)) %>% 
  dplyr::mutate(
    cd_mun=as.character(cd_mun),
    cd_meso=as.character(cd_meso),
    cd_rgint=as.character(cd_rgint),
    cd_micro=as.character(cd_micro),
    cd_rgime=as.character(cd_rgime),
    cd_uf=as.character(cd_uf),
    cd_rg=as.character(cd_rg),
    product=cd_sh2,
    value=exp
  ) %>%
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product, value)

# Insert mun info into MongoDB
fmongo_insert(df = colec_mun, nm_db = "db1", nm_collec = "colec_mun")

# Get mun info from MongoDB
mongo_set <- mongolite::mongo(db = "db1", collection = "colec_mun", url = mongo_credentials$mongoURL, verbose = TRUE)
colec_mun <- mongo_set$find()


# db_uf -------------------------------------------------------------------

# Grouping by uf
colec_uf <- colec_mun %>% 
  dplyr::group_by(cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
df_uf <- fct_add_eci(df = colec_uf, cd_org = "cd_uf")

# Insert uf info into MongoDB
fmongo_insert(df = df_uf, nm_db = "db1", nm_collec = "colec_uf")

# db_meso -----------------------------------------------------------------

# Grouping by meso
colec_meso <- colec_mun %>% 
  dplyr::group_by(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
df_meso <- fct_add_eci(df = colec_meso, cd_org = "cd_meso")

# Insert meso info into MongoDB
fmongo_insert(df = df_meso, nm_db = "db1", nm_collec = "colec_meso")

# db_micro ----------------------------------------------------------------

# Grouping by micro
colec_micro <- colec_mun %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
df_micro <- fct_add_eci(df = colec_micro, cd_org = "cd_micro")

# Insert micro info into MongoDB
fmongo_insert(df = df_micro, nm_db = "db1", nm_collec = "colec_micro")


# db_rgint ----------------------------------------------------------------

# Grouping by rgint
colec_rgint <- colec_mun %>% 
  dplyr::group_by(cd_rgint, nm_rgint, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
df_rgint <- fct_add_eci(df = colec_rgint, cd_org = "cd_rgint")

# Insert rgint to MongoDB
fmongo_insert(df = df_rgint, nm_db = "db1", nm_collec = "colec_rgint")


# db_rgime ----------------------------------------------------------------

# Grouping by rgime
colec_rgime <- colec_mun %>% 
  dplyr::group_by(cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
df_rgime <- fct_add_eci(df = colec_rgime, cd_org = "cd_rgime")

# Insert rgime to df
fmongo_insert(df = df_rgime, nm_db = "db1", nm_collec = "colec_rgime")


# More --------------------------------------------------------------------


