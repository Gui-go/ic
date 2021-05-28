# R-script db_uf.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)


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


# Code --------------------------------------------------------------------

sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

source(file = "code/functions/data_loc.R")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_uf, nm_uf, sg_uf, cd_rg, sg_rg, nm_rg) %>% 
  distinct()
  
exp <- vroom::vroom(file = "data/EXP_COMPLETA_MUN/EXP_COMPLETA_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  dplyr::mutate("cd_sh2" = substr(sh4, 1, 2)) %>%
  dplyr::rename("sg_uf"="sg_uf_mun") %>% 
  dplyr::group_by(sg_uf, cd_sh2) %>%
  dplyr::summarise(exp = sum(exp_fob)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(sg_uf, cd_sh2, exp)

df <- exp %>%
  dplyr::left_join(., br_loc, by="sg_uf") %>%
  dplyr::rename("country"="cd_uf", "product"="cd_sh2", "value"="exp") %>% 
  na.omit()

eci_est <- complexity_measures(balassa_index = economiccomplexity::balassa_index(data = df2))$complexity_index_country

dff <- df %>%
  dplyr::rename(
    "cd_uf"="country", 
    "cd_sh2"="product", 
    "exp"="value"
  ) %>% 
  dplyr::mutate(cd_uf=as.character(cd_uf))

eci_estdf <- data.frame(
  cd_uf=names(eci_est),
  eci=eci_est
)

df_eci <- left_join(dff, eci_estdf, by="cd_uf") %>%
  dplyr::arrange(dplyr::desc(eci)) %>% 
  dplyr::rename("value"="exp", "product"="cd_sh2") %>% 
  dplyr::select(cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product, value, eci)

df_eci_m <- df_eci %>% 
  select(-value, -product) %>% 
  mutate(value=eci, product="eci") %>% 
  select(-eci) %>% distinct() %>% arrange(desc(value)) %>% 
  dplyr::select(cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product, value)

df_eci_exp <- bind_rows(df_eci_m, df_eci %>% select(-eci))

# Name
colec_uf_exp_eci <- df_eci_exp

# Insert ------------------------------------------------------------------

source(file = "code/functions/fct_insertmongodb.R")
fmongo_insert(df = colec_uf_exp_eci, nm_db = "db1", nm_collec = "colec_uf_exp_eci")


# try ---------------------------------------------------------------------

shp_uf <- sf::st_read("data/shp/BR_UF_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_uf = as.character(cd_uf))


dff_shp <- df_eci_exp %>% 
  dplyr::filter(product=="eci") %>% 
  dplyr::left_join(., shp_uf, by = c("cd_uf", "nm_uf")) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["value"])

