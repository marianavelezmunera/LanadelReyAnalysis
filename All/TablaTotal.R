# Tabla 

medias<-aggregate(lanadata$Puntaje,list(lanadata$Album),FUN=mean)
colnames(medias)<-c("Album","Puntaje promedio")

mediana<-aggregate(lanadata$Puntaje,list(lanadata$Album),FUN=median)

info_completa<-cbind(medias,mediana$x)
colnames(info_completa)<-c("Album","Media","Mediana")

info_completa<-info_completa[order(info_completa$Media,info_completa$Mediana,decreasing = TRUE),]


url_albums<-c(
"https://upload.wikimedia.org/wikipedia/commons/f/ff/NFR_Lana_de_rey.png",
"https://upload.wikimedia.org/wikipedia/commons/e/e2/Born_to_Die_-_The_Paradise_Edition.png","https://upload.wikimedia.org/wikipedia/commons/d/d5/Lana_Del_Rey_-_Lust_for_Life_logo.png","https://upload.wikimedia.org/wikipedia/commons/9/9b/Honeymoon_Logo.png","https://upload.wikimedia.org/wikipedia/commons/1/1e/Chemtrails_over_the_Country_Club_Logo.png","https://upload.wikimedia.org/wikipedia/commons/b/bf/Blue_banisters.png","https://upload.wikimedia.org/wikipedia/commons/4/4f/Ultraviolence_Logo.png","https://upload.wikimedia.org/wikipedia/commons/6/6d/Borntodietitle.png","https://upload.wikimedia.org/wikipedia/commons/d/d0/Did_You_Know_That_There%27s_a_Tunnel_Under_Ocean_Blvd_logo.svg")

info_completa$url<-url_albums
info_completa<-info_completa %>% #reordering columns
  relocate(url,.before = Album)

info_completa$url[8]

tabla<-info_completa[,c(1,3,4)] %>%
  gt() %>% #table
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(url)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = c(100,50,50,20,50,15,30,30,50)
      )
    }) %>%
  cols_label(url="Álbum",Media="Puntaje promedio",Mediana="Puntaje medio") 
rm(tabla,tabla2)

tabla<-tabla %>%
  opt_all_caps()%>%
  opt_table_font(font = list(google_font("Bebas Neue"),default_fonts()))

tabla<-tabla %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "all", weight = px(2)),
      #Make text bold
      cell_text(weight = "bold")
    ))
tabla<-tabla %>%
  tab_style(
    style = cell_borders(
      sides = c("left","right"),
      weight = px(2),color="black"
    ),
    locations = cells_body()
  )
tabla <-tabla%>%
  tab_style(
    style = cell_borders(
      sides=c("bottom","top"),color="transparent"),
    locations = cells_body()
  )
tabla <- tabla %>%
  tab_options(table_body.border.bottom.color = "black") %>%
  tab_options(column_labels.font.size = px(15)) %>%
  cols_align("center")
tabla

gtsave(tabla,"tabla_total.png")

