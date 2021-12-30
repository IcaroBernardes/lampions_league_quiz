# Gestão dos pacotes
library(tidyverse)
library(stringi)
library(png)
library(ggimage)

# Define o nome dos arquivos com escudos dos clubes e suas dimensões
times <- tibble(
  nome = c("Atlético-BA","Bahia","Bahia de Feira",
           "Barcelona-BA","Doce Mel","Jacuipense",
           "Juazeirense","UNIRB","Vitória",
           "Vitória da Conquista",
           "Afogados","Caruaru City","Íbis",
           "Náutico","Retrô","Salgueiro",
           "Santa Cruz","Sete de Setembro","Sport",
           "Vera Cruz",
           "Atlético-CE","Caucaia","Ceará",
           "Crato","Ferroviário","Fortaleza",
           "Icasa","Iguatu","Maracanã",
           "Pacajus",
           "ASA","CRB","Cruzeiro de Arapiraca",
           "CSA","CSE","Desportivo Aliança",
           "Jaciobá","Murici",
           "América-SE","Atlético Gloriense","Boca Júnior",
           "Confiança","Falcon","Freipaulistano",
           "Itabaiana","Lagarto","Maruinense",
           "Sergipe",
           "Atlético-PB","Auto Esporte","Botafogo-PB",
           "Campinense","CSP","Nacional de Patos",
           "São Paulo Crystal","Sport-PB","Sousa",
           "Treze",
           "ABC","América-RN","Assu",
           "Força e Luz","Globo FC","Potiguar de Mossoró",
           "Potyguar","Santa Cruz de Natal",
           "Cordino","IAPE","Juventude",
           "Moto Club","Pinheiro","Sampaio Correia",
           "São José","Tuntum",
           "4 de Julho","Altos","Corisabbá",
           "Flamengo-PI","Fluminense-PI","Oeirense",
           "Parnahyba","River-PI")
) %>% 
  dplyr::mutate(nome = nome %>% 
                  tolower() %>% 
                  stringi::stri_trans_general(id = "Latin-ASCII") %>% 
                  stringr::str_remove_all("[[:space:]]")) %>% 
  dplyr::mutate(image = paste0("www/teams/",nome,".png")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(width = dim(png::readPNG(image))[2],
                height = dim(png::readPNG(image))[1],
                asp_ratio = width/height) %>% 
  dplyr::ungroup()

# Transforma os escudos em contornos cinzas e os salva em dimensões adequadas
for (i in 1:dim(times)[1]) {
  df <- times %>% 
    dplyr::slice(i)
  p <- df %>% 
    ggplot() +
    geom_image(aes(x = width/2, y = height/2, image = image), size = 1, color = "gray",
               by = "width", asp = df$asp_ratio)+
    theme_void() +
    theme(aspect.ratio = 1/df$asp_ratio) +
    coord_cartesian(xlim = c(0,df$width), ylim = c(0,df$height), expand = FALSE)
  ggsave(paste0("www/teams/",df$nome,"_outline.png"), p, units = "px",
         dpi = "retina", width = df$width, height = df$height, bg = NA)
}