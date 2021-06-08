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


fct_add_eci <- function(df, cd_ref){

  df2 <- df %>%
    dplyr::rename("country"=cd_ref) %>%
    stats::na.omit()
  
  eci_est <- economiccomplexity::complexity_measures(
    balassa_index = economiccomplexity::balassa_index(data = df2)
  )$complexity_index_country
  
  eci_estdf <- 
    data.frame(
      cd_ref=names(eci_est),
      product="eci",
      value=as.numeric(eci_est)
    ) %>% 
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_ref):="cd_ref",
      dplyr::everything()
    ) %>% 
    dplyr::left_join(
      ., 
      df %>% dplyr::select(-product, -value) %>% dplyr::distinct(), 
      by=cd_ref
    ) %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::select(names(df))
  
  eci_estdf_norm <- 
    data.frame(
      cd_ref=names(eci_est),
      product="eci_norm",
      value=normalize(as.numeric(eci_est))
    ) %>%
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_ref):="cd_ref",
      dplyr::everything()
    ) %>%
    dplyr::left_join(
      ., 
      df %>% dplyr::select(-product, -value) %>% dplyr::distinct(), 
      by=cd_ref
    ) %>% 
    dplyr::arrange(desc(value)) %>% 
    dplyr::select(names(df))
  
  df3 <- df2 %>%
    dplyr::select( # for renaming dynamically
      !!dplyr::quo_name(cd_ref):="country",
      dplyr::everything()
    ) %>% 
    dplyr::select(names(df)) %>% 
    dplyr::bind_rows(., eci_estdf) %>% 
    dplyr::bind_rows(., eci_estdf_norm)
  
  return(df3)
  
}
