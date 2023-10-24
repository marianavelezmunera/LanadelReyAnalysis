# Data
lanadata <- read_excel("C:/Users/Maria/Downloads/Lana.xlsx",col_types = c("text", "text", "numeric","text", "text", "text", "numeric","text", "text", "text"))

lanadata<-lanadata[,1:6]
unique(lanadata$Álbum)
unique(lanadata$Skip)
unique(lanadata$`Me Identifico`)
lanadata[lanadata=="OJALÁ"]<-"NO"
lanadata[lanadata=="NO VOLUNTARIAMENTE"]<-"NO"
unique(lanadata$Letra)
colnames(lanadata)[2]<-"Album"
colnames(lanadata)[6]<-"Meidentifico"
lanadata[50,1]
