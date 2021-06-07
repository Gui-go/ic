df <- data.frame("a"=c("gui", "ori"), "b"=c("viegas", "ortiz"))
tt <- "asasasasasasasas"

f1 <- function(df){
  component <- data.frame("name"=c("aa"), "surname"=c("bb"))
  for (i in 1:nrow(df)) {
    # h3(df$a[i])
    # h4(df$b[i])
    component <- component %>% add_row("name"="cvcvcvcv", "surname"="mnmnmnmnmn")
    # r <- htmltools::HTML("<h2>giberish2222222222222</h2>")
    # htmltools::HTML(paste0("<h1>aa", df$a[i], "& ", df$b[i], "</h1>")) %>% print()
    # paste0("aa", df$a[i], "& ", df$b[i])
  }
  return(component)
}

# f1r <- lapply(df, function(x){paste0("<h1>", x, "</h1>")})

f1r <- f1(df)
# f1r

df <- data.frame("a"=c("gui", "ori"), "b"=c("viegas", "ortiz"))
num=2
lapply(1:num, function(i) {
  div(h4(df[i, 1]), h5(df[i, 2]))
})


membros <- tabPanel(
  title = "Membros", 
  value = "membros",
  br(),
  
  
  f1r,
  
  h1(tt),
  
  h3("Este aplicativo foi desenvolvido pelo grupo de pesquisa..."),
  br(),
  h3("Desenvolvido por Guilherme Viegas... email, linkedIn, ..."),
  br(),
  h4("Mais...")
)