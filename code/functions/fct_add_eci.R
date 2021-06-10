# R-script fct_add_eci.R

# Setup -------------------------------------------------------------------

gc()
options(stringsAsFactors = F)

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

source(file = "code/functions/fct_normalize.R")

# Code --------------------------------------------------------------------


fct_add_eci <- function(df, cd_org){

  df2 <- df %>%
    dplyr::rename("country"=cd_org) %>%
    stats::na.omit()
  
  eci_est_fit <- economiccomplexity::complexity_measures(
    balassa_index = economiccomplexity::balassa_index(data = df2), 
    method = "fitness"
  )$complexity_index_country
  
  eci_est_ref <- economiccomplexity::complexity_measures(
    balassa_index = economiccomplexity::balassa_index(data = df2), 
    method = "reflections"
  )$complexity_index_country
  
  eci_estdf_fit <- 
    data.frame(
      cd_org=names(eci_est_fit),
      product="eci_fit",
      value=as.numeric(eci_est_fit)
    ) %>% 
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_org):="cd_org",
      dplyr::everything()
    ) %>% 
    dplyr::left_join(
      ., 
      df %>% dplyr::select(-product, -value) %>% dplyr::distinct(), 
      by=cd_org
    ) %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::select(names(df))
  
  eci_estdf_fit_norm <- 
    data.frame(
      cd_org=names(eci_est_fit),
      product="eci_fit_norm",
      value=normalize(as.numeric(eci_est_fit))
    ) %>%
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_org):="cd_org",
      dplyr::everything()
    ) %>%
    dplyr::left_join(
      ., 
      df %>% dplyr::select(-product, -value) %>% dplyr::distinct(), 
      by=cd_org
    ) %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::select(names(df))

  eci_estdf_ref <- 
    data.frame(
      cd_org=names(eci_est_ref),
      product="eci_ref",
      value=as.numeric(eci_est_ref)
    ) %>% 
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_org):="cd_org",
      dplyr::everything()
    ) %>% 
    dplyr::left_join(
      ., 
      df %>% dplyr::select(-product, -value) %>% dplyr::distinct(), 
      by=cd_org
    ) %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::select(names(df))
  
  eci_estdf_ref_norm <- 
    data.frame(
      cd_org=names(eci_est_ref),
      product="eci_ref_norm",
      value=normalize(as.numeric(eci_est_ref))
    ) %>%
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_org):="cd_org",
      dplyr::everything()
    ) %>%
    dplyr::left_join(
      ., 
      df %>% dplyr::select(-product, -value) %>% dplyr::distinct(), 
      by=cd_org
    ) %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::select(names(df))
  
    
  df3 <- df2 %>%
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_org):="country",
      dplyr::everything()
    ) %>% 
    dplyr::select(names(df)) %>% 
    dplyr::bind_rows(., eci_estdf_fit) %>% 
    dplyr::bind_rows(., eci_estdf_fit_norm) %>% 
    dplyr::bind_rows(., eci_estdf_ref) %>% 
    dplyr::bind_rows(., eci_estdf_ref_norm)
  
  return(df3)
  
}
