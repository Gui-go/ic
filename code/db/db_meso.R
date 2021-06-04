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
# sg_uf_br <- c("RS", "SC", "PR")

source(file = "code/functions/data_loc.R")
br_loc <- data_loc(sg_uf_br) %>% 
  # select(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg) %>% 
  distinct()

exp <- vroom::vroom(file = "data/EXP_COMPLETA_MUN2/EXP_COMPLETA_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
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
  na.omit() %>% 
  dplyr::group_by(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_sh2) %>% 
  dplyr::summarise(exp = sum(exp)) %>% 
  dplyr::ungroup()

exp_to_eci <- exp2 %>%
  dplyr::rename("country"="cd_meso", "product"="cd_sh2", "value"="exp") %>%
  na.omit()

eci_est <- complexity_measures(balassa_index = economiccomplexity::balassa_index(data = exp_to_eci))$complexity_index_country

dff <- exp_to_eci %>%
  dplyr::rename(
    "cd_meso"="country", 
    "cd_sh2"="product", 
    "exp"="value"
  ) %>% 
  dplyr::mutate(cd_meso=as.character(cd_meso))

eci_estdf <- data.frame(
  cd_meso=names(eci_est),
  eci=eci_est
)

df_eci <- dff %>% 
  dplyr::mutate(cd_sh2=paste0("sh", cd_sh2)) %>% 
  left_join(., eci_estdf) %>%
  dplyr::arrange(dplyr::desc(eci)) %>% 
  dplyr::rename("value"="exp", "product"="cd_sh2") %>%
  dplyr::select(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product, value, eci)

df_eci_m <- df_eci %>% 
  select(-value, -product) %>% 
  mutate(value=eci, product="eci") %>% 
  select(-eci) %>% distinct() %>% arrange(desc(value)) %>% 
  dplyr::select(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product, value)

df_eci_total <- df_eci %>% 
  dplyr::group_by(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(product="sh00") %>% 
  dplyr::select(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product, value)

df_eci_exp <- bind_rows(df_eci_m, df_eci %>% select(-eci)) %>% 
  dplyr::bind_rows(., df_eci_total)
paste0("df_eci_exp tem ", length(unique(df_eci_exp$product)), " produtos distintos")
unique(df_eci_exp$product)

# Name
colec_meso_exp_eci <- df_eci_exp

# Check
length(unique(colec_meso_exp_eci$cd_uf))*length(unique(colec_meso_exp_eci$product))

# Adicionar 0 nos estados que nao exportaram nada desse produto

# Insert ------------------------------------------------------------------

source(file = "code/functions/fct_insertmongodb.R")
fmongo_insert(df = colec_meso_exp_eci, nm_db = "db1", nm_collec = "colec_meso_exp_eci")


# try ---------------------------------------------------------------------

shp_meso <- sf::st_read("data/shp/shp_meso/")

ggplot(shp_meso)+
  geom_sf(aes(0))

dff_shp <- colec_meso_exp_eci %>% 
  dplyr::filter(product=="eci") %>% 
  dplyr::left_join(., shp_meso) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["value"])



