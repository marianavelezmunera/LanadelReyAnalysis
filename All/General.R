options(encoding = "UTF-8") #For the tildes (I don't know why ggplot wasn't having it with the tildes)
lanadata$album <- factor(lanadata$Álbum , levels=c("Born to Die", "Paradise", "Ultraviolence", "Honeymoon","Lust for Life","Norman Fucking Rockwell!","Chemtrails over the Country Club","Blue Banisters","DYKTTTUOB")) #Reordering so it is in launch order


font_add_google("Bebas Neue","Bebas Neue") #Font
font.families()
showtext_auto()

gp<-ggplot(data=lanadata,aes(x=album,y=Puntaje,fill=album))+
  geom_boxplot(color="black")+
  ylab("Puntajes")+xlab("Álbum")+
  theme_pubclean()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=c("#6B90BF","#B3984B","#89888C","#021C35","#8C0712","#A3D930","#BFBFBF","#8C5C4A","#F2DB94"))+
  theme(axis.text.y = element_text(family = "Bebas Neue",size = 20))+
  theme(axis.title.y = element_text(family = "Bebas Neue",size = 28))
gp


pimage <- axis_canvas(gp, axis = 'x')+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/6/6d/Borntodietitle.png", x = 0.5 , scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/e/e2/Born_to_Die_-_The_Paradise_Edition.png", x = 1.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/4/4f/Ultraviolence_Logo.png", x = 2.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/9/9b/Honeymoon_Logo.png", x = 3.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/d/d5/Lana_Del_Rey_-_Lust_for_Life_logo.png", x =4.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/f/ff/NFR_Lana_de_rey.png", x = 5.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/1/1e/Chemtrails_over_the_Country_Club_Logo.png", x = 6.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/b/bf/Blue_banisters.png", x = 7.5, scale = 1)+
  cowplot::draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Did_You_Know_That_There%27s_a_Tunnel_Under_Ocean_Blvd_logo.svg/2560px-Did_You_Know_That_There%27s_a_Tunnel_Under_Ocean_Blvd_logo.svg.png", x = 8.5, scale = 1)

plot_listo<-ggdraw(insert_xaxis_grob(gp, pimage, position = "bottom"))
plot_listo

ggsave("plot_total2.png",last_plot())

# Letras

bars<-ggplot(data=na.omit(lanadata),aes(x=Letra,fill=Letra))+
  geom_bar(width = 0.25,color="black")+
  theme_pubclean()+
  theme(aspect.ratio = 1)+
  ylab("NÚMERO DE CANCIONES")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(family="Bebas Neue",size = 20))+
  theme(axis.text.x = element_text(family = "Bebas Neue",size=18))+
  theme(axis.text.y = element_text(family = "Bebas Neue",size=18))+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Cringe","Masterpiece","Normal"))+
  scale_fill_manual(values =c("#8C0712","#A3D930","#021C35"))+
  ggtitle("¿CÓMO ME PARECE LA LETRA DE LAS CANCIONES?")+
  theme(plot.title = element_text(family = "Bebas Neue",size = 20,hjust = 0.5))
bars

skips<-as.data.frame(table(lanadata$Album,lanadata$Skip))
colnames(skips)<-c("album","skips","frecuencia")

skips
skips_graph<-ggplot(data = skips,aes(x=album,y=frecuencia,fill=skips))+
  geom_bar(position="dodge", stat="identity",color="black")+
  theme_pubclean()+
  ylab("NÚMERO DE CANCIONES")+
  scale_fill_manual(name = "SKIPS",values=c("#A3D930","#021C35"))+
  theme(axis.title.y = element_text(family="Bebas Neue",face="bold",size=28))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_text(family = "Bebas Neue",size = 20))+
  theme(legend.text = element_text(family = "Bebas Neue",size=20))+
  theme(axis.text = element_text(family = "Bebas Neue",size = 20))+
  theme(legend.position = "right")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12))
skips_graph

skips_listo<-ggdraw(insert_xaxis_grob(skips_graph, pimage, position = "bottom"))
skips_listo

ggsave("skips_total.png",last_plot())


# Identificación
colnames(lanadata)[6]<-"Meidentifico"
conteo_iden<-count(lanadata,album,Meidentifico) #Freq table

no<-subset(conteo_iden,Meidentifico=="NO")
si<-subset(conteo_iden,Meidentifico=="SÍ")

identi<-no[,-2]
colnames(identi)[2]<-"NO"
identi$SI<-si$n

identi$album<-factor(identi$album , levels=c("Born to Die", "Paradise", "Ultraviolence", "Honeymoon","Lust for Life","Norman Fucking Rockwell!","Chemtrails over the Country Club","Blue Banisters","DYKTTTUOB"))
loli<-ggplot(identi) +
  geom_segment( aes(x=album, xend=album, y=SI, yend=NO), color="black") +
  geom_point( aes(x=album, y=SI,color = "SI"), size=2 ) +
  geom_point( aes(x=album, y=NO,color="NO"), size=2 ) +
  scale_color_manual(values = c("#BF5690", "#162759"), 
                     name   = "¿ME IDENTIFICO CON LA CANCIÓN?")+
  theme_pubclean()+
  xlab("") +
  ylab("NÚMERO DE CANCIONES")+
  theme(legend.position = "top",
        panel.border    = element_blank())+
  theme(legend.text = element_text(family = "Bebas Neue",size = 18))+
  theme(legend.title = element_text(family = "Bebas Neue",size=18))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y  = element_text(family = "Bebas Neue",size = 24) )+
  theme(axis.text.y = element_text(family = "Bebas Neue",size=20))
loli

loli_listo<-ggdraw(insert_xaxis_grob(loli, pimage, position = "bottom"))
loli_listo
ggsave("identificada_total.png",plot=loli_listo)
