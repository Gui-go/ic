# R-script 06-shp_handling.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf


# Observations ------------------------------------------------------------

# SHP must have ONLY cd. Do not insert nm so the joins wont work properly

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
if(!require(economiccomplexity)){install.packages("economiccomplexity")}


# Code --------------------------------------------------------------------

# ufs ---------------------------------------------------------------------
shp_ufs <- sf::st_read("data/shp/BR_UF_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_uf = as.character(cd_uf),
    nm_rg = as.character(nm_regiao),
    sg_uf = as.character(sigla_uf)
  ) %>% 
  dplyr::mutate(
    sg_rg=if_else(
      nm_rg=="Norte", "N", 
      if_else(
        nm_rg=="Nordeste", "NE",
        if_else(
          nm_rg=="Sudeste", "SE", 
          if_else(
            nm_rg=="Sul", "S",
            if_else(
              nm_rg=="Centro-oeste", "CO", "Outro"
            )
          )
        )
      )
    )
  ) %>% 
  mutate(nm_rg=if_else(nm_rg=="Centro-oeste", "Centro-Oeste", nm_rg)) %>% 
  mutate(cd_uf=as.character(cd_uf), nm_uf=as.character(nm_uf), sg_uf=as.character(sg_uf), nm_rg=as.character(nm_rg), sg_rg=as.character(sg_rg))
shp_ufs_final <- shp_ufs[, c("cd_uf", "nm_uf", "sg_uf", "nm_rg", "sg_rg")]
# shp_ufs_final[which(shp_ufs_final$sg_rg=="CO"), ]
# shp_ufs_final
# dir.create("data/shp/shp_ufs")
# sf::st_write(shp_ufs_final, "data/shp/shp_ufs/shp_ufs.shp")


# meso --------------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_meso, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_meso=as.character(cd_meso)) %>% 
  distinct()

shp_meso <- sf::st_read("data/shp/BR_Mesorregioes_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_meso = as.character(cd_meso),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_meso <- shp_meso[, c("cd_meso", "nm_meso", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% sf::st_sf()

unlink("data/shp/shp_meso/", recursive = T)
dir.create("data/shp/shp_meso")
sf::st_write(shp_meso, "data/shp/shp_meso/shp_meso.shp")



# shp_micro ---------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_micro, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_micro=as.character(cd_micro)) %>% 
  distinct()

shp_micro <- sf::st_read("data/shp/BR_Microrregioes_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_micro = as.character(cd_micro),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_micro <- shp_micro[, c("cd_micro", "nm_micro", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% sf::st_sf()

unlink("data/shp/shp_micro/", recursive = T)
dir.create("data/shp/shp_micro")
sf::st_write(shp_micro, "data/shp/shp_micro/shp_micro.shp")


# shp_int -----------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_int, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_int=as.character(cd_int)) %>% 
  distinct()

shp_int <- sf::st_read("data/shp/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_micro = as.character(cd_micro),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_micro <- shp_micro[, c("cd_micro", "nm_micro", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% sf::st_sf()

unlink("data/shp/shp_micro/", recursive = T)
dir.create("data/shp/shp_micro")
sf::st_write(shp_micro, "data/shp/shp_micro/shp_micro.shp")

