#####################################################################
###################### PASANTIA DE INVESTIGACION: ################################# Clave interactiva para géneros en anuros de ############################## la colección herpetológica UIS ##################################################################################################### ANALISIS DE AGRUPAMIENTO EXPLORATORIO ###############
#####################################################################
####### Realizado por: Silvia Nùñez, silfer1993@hotmail.com #########

library(foreign)
library(cluster)

data<-read.delim2("C:/Users/Silvia Núñez Flórez/OneDrive/Documentos/Pasantia/Analisis/Codificacion_Revision_IX.txt") #llamar matriz

Carc<-data[, c('Especies', 'C_1',	'C_2',	'C_3', 'C_4',	'C_5',	'C_6',	'C_7',	'C_8',	'C_9', 'C_10',	'C_11',	'C_12',	'C_13',	'C_14',	'C_15',	'C_16',	'C_17',	'C_18',	'C_19',	'C_20',	'C_21',	'C_22',	'C_23',	'C_24',	'C_25',	'C_26',	'C_27',	'C_28',	'C_29',	'C_30',	'C_31',	'C_32',	'C_33',	'C_34',	'C_35',	'C_36',	'C_37',	'C_38',	'C_39', 'C_40',	'C_41',	'C_42',	'C_43',	'C_44',	'C_45',	'C_46',	'C_47',	'C_48',	'C_49',	'C_50',	'C_51',	'C_52',	'C_53',	'C_54',	'C_55',	'C_56',	'C_57',	'C_58',	'C_59',	'C_60',	'C_61',	'C_62',	'C_63',	'C_64',	'C_65',	'C_66',	'C_67',	'C_68',	'C_69',	'C_70') ] #reconocer datos

is.data.frame(Carc)

CarcD<-dist(Carc[,-1], method = "manhattan")

CarcD.h <- hclust(CarcD, method = "average") #Utilizar metodo de distancia para realizar cluster

CarcD.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,0.5,0.5,1))
plot(CarcD.h, font=3, cex=0.6, main='Analisis exploratorio general')

#####################################################################

C_1<-data.frame(Carc[,1:2])

is.data.frame(C_1)

C_1D<-dist(C_1[,-1], method = "manhattan")

C_1D.h <- hclust(C_1D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_1D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_1D.h, font=3, cex=0.6, main='Tamaño maximo Longitud rostro-cloaca') #realizar cluster

#####################################################################

C_2<-data.frame(Carc[,c(1,3)])

C_2D<-dist(C_2[,-1], method = "manhattan")

C_2D.h <- hclust(C_2D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_2D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_2D.h, font=3, cex=0.6, main='Relacion entre el ancho y largo de la cabeza') #realizar cluster

#####################################################################

C_3<-data.frame(Carc[,c(1,4)])

C_3D<-dist(C_3[,-1], method = "manhattan")

C_3D.h <- hclust(C_3D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_3D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_3D.h, font=3, cex=0.6, main='Elemento intercalar entre la distal y penúltima falange ') #realizar cluster

#####################################################################

C_4<-data.frame(Carc[,c(1,5)])

C_4D<-dist(C_4[,-1], method = "manhattan")

C_4D.h <- hclust(C_4D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_4D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_4D.h, font=3, cex=0.6, main='Modificacion craneal') #realizar cluster

#####################################################################

C_5<-data.frame(Carc[,c(1,6)])

C_5D<-dist(C_5[,-1], method = "manhattan")

C_5D.h <- hclust(C_5D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_5D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_5D.h, font=3, cex=0.6, main='Forma del hocico en vista dorsal') #realizar cluster

#####################################################################

C_6<-data.frame(Carc[,c(1,7)])

C_6D<-dist(C_6[,-1], method = "manhattan")

C_6D.h <- hclust(C_6D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_6D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_6D.h, font=3, cex=0.6, main='Forma del hocico en vista lateral') #realizar cluster

#####################################################################

C_7<-data.frame(Carc[,c(1,8)])

C_7D<-dist(C_7[,-1], method = "manhattan")

C_7D.h <- hclust(C_7D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_7D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_7D.h, font=3, cex=0.6, main='Narinas') #realizar cluster

#####################################################################

C_8<-data.frame(Carc[,c(1,9)])

C_8D<-dist(C_8[,-1], method = "manhattan")

C_8D.h <- hclust(C_8D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_8D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_8D.h, font=3, cex=0.6, main='Region internarinal') #realizar cluster

#####################################################################

C_9<-data.frame(Carc[,c(1,10)])

C_9D<-dist(C_9[,-1], method = "manhattan")

C_9D.h <- hclust(C_9D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_9D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_9D.h, font=3, cex=0.6, main='Canthus rostralis') #realizar cluster

#####################################################################

C_10<-data.frame(Carc[,c(1,11)])

C_10D<-dist(C_10[,-1], method = "manhattan")

C_10D.h <- hclust(C_10D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_10D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_10D.h, font=3, cex=0.6, main='Región loreal') #realizar cluster

#####################################################################

C_11<-data.frame(Carc[,c(1,12)])

C_11D<-dist(C_11[,-1], method = "manhattan")

C_11D.h <- hclust(C_11D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_11D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_11D.h, font=3, cex=0.6, main='Relacion entre la distancia interorbital y el ancho del parpado superior del ojo') #realizar cluster

#####################################################################

C_12<-data.frame(Carc[,c(1,13)])

C_12D<-dist(C_12[,-1], method = "manhattan")

C_12D.h <- hclust(C_12D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_12D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_12D.h, font=3, cex=0.6, main='Ojos') #realizar cluster

#####################################################################

C_13<-data.frame(Carc[,c(1,14)])

C_13D<-dist(C_13[,-1], method = "manhattan")

C_13D.h <- hclust(C_13D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_13D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_13D.h, font=3, cex=0.6, main='Relacion entre el diametro del ojo y la distancia ojo-narina') #realizar cluster

#####################################################################

C_14<-data.frame(Carc[,c(1,15)])

C_14D<-dist(C_14[,-1], method = "manhattan")

C_14D.h <- hclust(C_14D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_14D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_14D.h, font=3, cex=0.6, main='Parpado superior del ojo') #realizar cluster

#####################################################################

C_15<-data.frame(Carc[,c(1,16)])

C_15D<-dist(C_15[,-1], method = "manhattan")

C_15D.h <- hclust(C_15D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_15D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_15D.h, font=3, cex=0.6, main='Relacion entre el ancho del parpado superior del ojo y la distancia internarinal') #realizar cluster

#####################################################################

C_16<-data.frame(Carc[,c(1,17)])

C_16D<-dist(C_16[,-1], method = "manhattan")

C_16D.h <- hclust(C_16D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_16D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_16D.h, font=3, cex=0.6, main='Pupila') #realizar cluster

#####################################################################

C_17<-data.frame(Carc[,c(1,18)])

C_17D<-dist(C_17[,-1], method = "manhattan")

C_17D.h <- hclust(C_17D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_17D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_17D.h, font=3, cex=0.6, main='Membrana palpebral') #realizar cluster

#####################################################################

C_18<-data.frame(Carc[,c(1,19)])

C_18D<-dist(C_18[,-1], method = "manhattan")

C_18D.h <- hclust(C_18D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_18D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_18D.h, font=3, cex=0.6, main='Timpano') #realizar cluster

#####################################################################

C_19<-data.frame(Carc[,c(1,20)])

C_19D<-dist(C_19[,-1], method = "manhattan")

C_19D.h <- hclust(C_19D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_19D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_19D.h, font=3, cex=0.6, main='Relacion entre el diametro del timpano y el diametro del ojo') #realizar cluster

#####################################################################

C_20<-data.frame(Carc[,c(1,21)])

C_20D<-dist(C_20[,-1], method = "manhattan")

C_20D.h <- hclust(C_20D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_20D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_20D.h, font=3, cex=0.6, main='Relacion entre la distancia del timpano al ojo y el diametro del timpano') #realizar cluster

#####################################################################

C_21<-data.frame(Carc[,c(1,22)])

C_21D<-dist(C_21[,-1], method = "manhattan")

C_21D.h <- hclust(C_21D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_21D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_21D.h, font=3, cex=0.6, main='Anillo timpanico') #realizar cluster

#####################################################################

C_22<-data.frame(Carc[,c(1,23)])

C_22D<-dist(C_22[,-1], method = "manhattan")

C_22D.h <- hclust(C_22D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_22D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_22D.h, font=3, cex=0.6, main='Pliegue supra timpanico') #realizar cluster

#####################################################################

C_23<-data.frame(Carc[,c(1,24)])

C_23D<-dist(C_23[,-1], method = "manhattan")

C_23D.h <- hclust(C_23D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_23D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_23D.h, font=3, cex=0.6, main='Forma de la lengua') #realizar cluster

#####################################################################

C_24<-data.frame(Carc[,c(1,25)])

C_24D<-dist(C_24[,-1], method = "manhattan")

C_24D.h <- hclust(C_24D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_24D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_24D.h, font=3, cex=0.6, main='Contorno de la lengua') #realizar cluster

#####################################################################

C_25<-data.frame(Carc[,c(1,26)])

C_25D<-dist(C_25[,-1], method = "manhattan")

C_25D.h <- hclust(C_25D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_25D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_25D.h, font=3, cex=0.6, main='Adherencia de la lengua') #realizar cluster

#####################################################################

C_26<-data.frame(Carc[,c(1,27)])

C_26D<-dist(C_26[,-1], method = "manhattan")

C_26D.h <- hclust(C_26D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_26D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_26D.h, font=3, cex=0.6, main='Procesos dentigeros vomerinos') #realizar cluster

#####################################################################

C_27<-data.frame(Carc[,c(1,28)])

C_27D<-dist(C_27[,-1], method = "manhattan")

C_27D.h <- hclust(C_27D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_27D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_27D.h, font=3, cex=0.6, main='Forma de los procesos dentigeros vomerinos') #realizar cluster

#####################################################################

C_28<-data.frame(Carc[,c(1,29)])

C_28D<-dist(C_28[,-1], method = "manhattan")

C_28D.h <- hclust(C_28D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_28D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_28D.h, font=3, cex=0.6, main='Posición de los procesos dentígeros vomerinos') #realizar cluster

#####################################################################

C_29<-data.frame(Carc[,c(1,30)])

C_29D<-dist(C_29[,-1], method = "manhattan")

C_29D.h <- hclust(C_29D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_29D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_29D.h, font=3, cex=0.6, main='Coanas') #realizar cluster

#####################################################################

C_30<-data.frame(Carc[,c(1,31)])

C_30D<-dist(C_30[,-1], method = "manhattan")

C_30D.h <- hclust(C_30D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_30D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_30D.h, font=3, cex=0.6, main='Forma del saco vocal') #realizar cluster

#####################################################################

C_31<-data.frame(Carc[,c(1,32)])

C_31D<-dist(C_31[,-1], method = "manhattan")

C_31D.h <- hclust(C_31D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_31D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_31D.h, font=3, cex=0.6, main='Glandula mental') #realizar cluster

#####################################################################

C_32<-data.frame(Carc[,c(1,33)])

C_32D<-dist(C_32[,-1], method = "manhattan")

C_32D.h <- hclust(C_32D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_32D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_32D.h, font=3, cex=0.6, main='Membrana axial') #realizar cluster

#####################################################################

C_33<-data.frame(Carc[,c(1,34)])

C_33D<-dist(C_33[,-1], method = "manhattan")

C_33D.h <- hclust(C_33D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_33D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_33D.h, font=3, cex=0.6, main='Espina humeral') #realizar cluster

#####################################################################

C_34<-data.frame(Carc[,c(1,35)])

C_34D<-dist(C_34[,-1], method = "manhattan")

C_34D.h <- hclust(C_34D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_34D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_34D.h, font=3, cex=0.6, main='Relacion entre la contextura de la muñeca y el antebrazo') #realizar cluster

#####################################################################

C_35<-data.frame(Carc[,c(1,36)])

C_35D<-dist(C_35[,-1], method = "manhattan")

C_35D.h <- hclust(C_35D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_35D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_35D.h, font=3, cex=0.6, main='Ornamentos externos en el antebrazo') #realizar cluster

#####################################################################

C_36<-data.frame(Carc[,c(1,37)])

C_36D<-dist(C_36[,-1], method = "manhattan")

C_36D.h <- hclust(C_36D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_36D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_36D.h, font=3, cex=0.6, main='Terminacion distal de los dedos') #realizar cluster

#####################################################################

C_37<-data.frame(Carc[,c(1,38)])

C_37D<-dist(C_37[,-1], method = "manhattan")

C_37D.h <- hclust(C_37D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_37D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_37D.h, font=3, cex=0.6, main='Relacion entre el diametro del disco y el diametro del timpano') #realizar cluster

#####################################################################

C_38<-data.frame(Carc[,c(1,39)])

C_38D<-dist(C_38[,-1], method = "manhattan")

C_38D.h <- hclust(C_38D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_38D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_38D.h, font=3, cex=0.6, main='Longitud relativa de los dedos manuales') #realizar cluster

#####################################################################

C_39<-data.frame(Carc[,c(1,40)])

C_39D<-dist(C_39[,-1], method = "manhattan")

C_39D.h <- hclust(C_39D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_39D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_39D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos manuales I y II ') #realizar cluster

#####################################################################

C_40<-data.frame(Carc[,c(1,41)])

C_40D<-dist(C_40[,-1], method = "manhattan")

C_40D.h <- hclust(C_40D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_40D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_40D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos manuales II y III') #realizar cluster

#####################################################################

C_41<-data.frame(Carc[,c(1,42)])

C_41D<-dist(C_41[,-1], method = "manhattan")

C_41D.h <- hclust(C_41D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_41D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_41D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos manuales III y IV') #realizar cluster

#####################################################################

C_42<-data.frame(Carc[,c(1,43)])

C_42D<-dist(C_42[,-1], method = "manhattan")

C_42D.h <- hclust(C_42D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_42D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_42D.h, font=3, cex=0.6, main='Tuberculos subarticulares manuales') #realizar cluster

#####################################################################

C_43<-data.frame(Carc[,c(1,44)])

C_43D<-dist(C_43[,-1], method = "manhattan")

C_43D.h <- hclust(C_43D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_43D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_43D.h, font=3, cex=0.6, main='Tuberculo subarticular distal en el IV dedo manual') #realizar cluster

#####################################################################

C_44<-data.frame(Carc[,c(1,45)])

C_44D<-dist(C_44[,-1], method = "manhattan")

C_44D.h <- hclust(C_44D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_44D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_44D.h, font=3, cex=0.6, main='Tuberculos supernumerarios manuales') #realizar cluster

#####################################################################

C_45<-data.frame(Carc[,c(1,46)])

C_45D<-dist(C_45[,-1], method = "manhattan")

C_45D.h <- hclust(C_45D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_45D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_45D.h, font=3, cex=0.6, main='Relacion entre el tamaño de los tuberculos supernumerarios y los tuberculos subarticulares manuales') #realizar cluster

#####################################################################

C_46<-data.frame(Carc[,c(1,47)])

C_46D<-dist(C_46[,-1], method = "manhattan")

C_46D.h <- hclust(C_46D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_46D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_46D.h, font=3, cex=0.6, main='Tuberculo palmar') #realizar cluster

#####################################################################

C_47<-data.frame(Carc[,c(1,48)])

C_47D<-dist(C_47[,-1], method = "manhattan")

C_47D.h <- hclust(C_47D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_47D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_47D.h, font=3, cex=0.6, main='Tuberculo thenar o prepollical') #realizar cluster

#####################################################################

C_48<-data.frame(Carc[,c(1,49)])

C_48D<-dist(C_48[,-1], method = "manhattan")

C_48D.h <- hclust(C_48D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_48D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_48D.h, font=3, cex=0.6, main='Excrecencias nupciales ') #realizar cluster

#####################################################################

C_49<-data.frame(Carc[,c(1,50)])

C_49D<-dist(C_49[,-1], method = "manhattan")

C_49D.h <- hclust(C_49D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_49D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_49D.h, font=3, cex=0.6, main='Relacion entre la contextura del talon y la tibia') #realizar cluster

#####################################################################

C_50<-data.frame(Carc[,c(1,51)])

C_50D<-dist(C_50[,-1], method = "manhattan")

C_50D.h <- hclust(C_50D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_50D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_50D.h, font=3, cex=0.6, main='Ornamentos internos en el tarso') #realizar cluster

#####################################################################

C_51<-data.frame(Carc[,c(1,52)])

C_51D<-dist(C_51[,-1], method = "manhattan")

C_51D.h <- hclust(C_51D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_51D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_51D.h, font=3, cex=0.6, main='Ornamentos externos en el tarso') #realizar cluster

#####################################################################

C_52<-data.frame(Carc[,c(1,53)])

C_52D<-dist(C_52[,-1], method = "manhattan")

C_52D.h <- hclust(C_52D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_52D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_52D.h, font=3, cex=0.6, main='Ornamentos en el talon') #realizar cluster

#####################################################################

C_53<-data.frame(Carc[,c(1,54)])

C_53D<-dist(C_53[,-1], method = "manhattan")

C_53D.h <- hclust(C_53D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_53D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_53D.h, font=3, cex=0.6, main='Tuberculos subarticulares pediales') #realizar cluster

#####################################################################

C_54<-data.frame(Carc[,c(1,55)])

C_54D<-dist(C_54[,-1], method = "manhattan")

C_54D.h <- hclust(C_54D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_54D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_54D.h, font=3, cex=0.6, main='Tuberculo subarticular distal en el V dedo pedial') #realizar cluster

#####################################################################

C_55<-data.frame(Carc[,c(1,56)])

C_55D<-dist(C_55[,-1], method = "manhattan")

C_55D.h <- hclust(C_55D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_55D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_55D.h, font=3, cex=0.6, main='Tuberculos supernumerarios pediales') #realizar cluster

#####################################################################

C_56<-data.frame(Carc[,c(1,57)])

C_56D<-dist(C_56[,-1], method = "manhattan")

C_56D.h <- hclust(C_56D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_56D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_56D.h, font=3, cex=0.6, main='Relacion entre el tamaño de los tuberculos supernumerarios y los tuberculos subarticulares pediales') #realizar cluster

#####################################################################

C_57<-data.frame(Carc[,c(1,58)])

C_57D<-dist(C_57[,-1], method = "manhattan")

C_57D.h <- hclust(C_57D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_57D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_57D.h, font=3, cex=0.6, main='Tuberculo metatarsal interno') #realizar cluster

#####################################################################

C_58<-data.frame(Carc[,c(1,59)])

C_58D<-dist(C_58[,-1], method = "manhattan")

C_58D.h <- hclust(C_58D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_58D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_58D.h, font=3, cex=0.6, main='Tuberculo metatarsal externo') #realizar cluster

#####################################################################

C_59<-data.frame(Carc[,c(1,60)])

C_59D<-dist(C_59[,-1], method = "manhattan")

C_59D.h <- hclust(C_59D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_59D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_59D.h, font=3, cex=0.6, main='Relacion entre el tamaño de los discos pediales y los discos manuales') #realizar cluster

#####################################################################

C_60<-data.frame(Carc[,c(1,61)])

C_60D<-dist(C_60[,-1], method = "manhattan")

C_60D.h <- hclust(C_60D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_60D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_60D.h, font=3, cex=0.6, main='Longitud de los dedos pediales') #realizar cluster

#####################################################################

C_61<-data.frame(Carc[,c(1,62)])

C_61D<-dist(C_61[,-1], method = "manhattan")

C_61D.h <- hclust(C_61D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_61D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_61D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos pedial I y II ') #realizar cluster

#####################################################################

C_62<-data.frame(Carc[,c(1,63)])

C_62D<-dist(C_62[,-1], method = "manhattan")

C_62D.h <- hclust(C_62D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_62D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_62D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos pediales II y III') #realizar cluster

#####################################################################

C_63<-data.frame(Carc[,c(1,64)])

C_63D<-dist(C_63[,-1], method = "manhattan")

C_63D.h <- hclust(C_63D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_63D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_63D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos pediales III y IV') #realizar cluster

#####################################################################

C_64<-data.frame(Carc[,c(1,65)])

C_64D<-dist(C_64[,-1], method = "manhattan")

C_64D.h <- hclust(C_64D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_64D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_64D.h, font=3, cex=0.6, main='Membrana interdigital entre los dedos pediales IV y V') #realizar cluster

#####################################################################

C_65<-data.frame(Carc[,c(1,66)])

C_65D<-dist(C_65[,-1], method = "manhattan")

C_65D.h <- hclust(C_65D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_65D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_65D.h, font=3, cex=0.6, main='Abertura anal') #realizar cluster

#####################################################################

C_66<-data.frame(Carc[,c(1,67)])

C_66D<-dist(C_66[,-1], method = "manhattan")

C_66D.h <- hclust(C_66D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_66D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_66D.h, font=3, cex=0.6, main='Envoltura anal') #realizar cluster

#####################################################################

C_67<-data.frame(Carc[,c(1,68)])

C_67D<-dist(C_67[,-1], method = "manhattan")

C_67D.h <- hclust(C_67D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_67D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_67D.h, font=3, cex=0.6, main='Glandulas') #realizar cluster

#####################################################################

C_68<-data.frame(Carc[,c(1,69)])

C_68D<-dist(C_68[,-1], method = "manhattan")

C_68D.h <- hclust(C_68D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_68D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_68D.h, font=3, cex=0.6, main='Pliegues dorsales longitudinales') #realizar cluster

#####################################################################

C_69<-data.frame(Carc[,c(1,70)])

C_69D<-dist(C_69[,-1], method = "manhattan")

C_69D.h <- hclust(C_69D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_69D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_69D.h, font=3, cex=0.6, main='Piel dorsal') #realizar cluster

#####################################################################

C_70<-data.frame(Carc[,c(1,71)])

C_70D<-dist(C_70[,-1], method = "manhattan")

C_70D.h <- hclust(C_70D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_70D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_70D.h, font=3, cex=0.6, main='Piel ventral') #realizar cluster

#####################################################################

C_71<-data.frame(Carc[,c(1,72)])

C_71D<-dist(C_71[,-1], method = "manhattan")

C_71D.h <- hclust(C_71D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_71D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_71D.h, font=3, cex=0.6, main='Transparentacion ventral') #realizar cluster

#####################################################################

C_72<-data.frame(Carc[,c(1,73)])

C_72D<-dist(C_72[,-1], method = "manhattan")

C_72D.h <- hclust(C_72D, method = "average") #Utilizar metodo de distancia para realizar cluster

C_72D.h$labels <- Carc$Especies #Nombrar datos

par(mar=c(1,2,2,1))
plot(C_72D.h, font=3, cex=0.6, main='Verrugas pericloacales') #realizar cluster


#####################################################################################################Final#####################################################################################################
