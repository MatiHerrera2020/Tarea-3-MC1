library(dplyr)
library(foreign)
library(haven)
###############################################################################
###################### CREANDO LAS BASES PARA ESTIMAR
###############################################################################

lista_bases<-list.files("Bases", full.names = TRUE )
lista_abierta<-vector("list",length = length(lista_bases))
for (i in seq_along(lista_bases)) {
          lista_abierta[[i]]<-read_dta(lista_bases[[i]])
}
######### creando bases para cada trimestre en ambos años
lista_filtradas<-vector("list", length = 18L)
for (i in c(1:18)) {
          lista_filtradas[[i]]<-assign(paste("filtrada_",i,sep = ""),as.data.frame(rbind(
                    select(lista_abierta[[(2*i)-1]], activ,sexo,ano_encuesta),
                    select(lista_abierta[[2*i]],activ,sexo,ano_encuesta))))
}

########### creando lista con todas las bases 2019-2020 por trimestres

for (i in c(1:9)) {
          names(lista_filtradas[[i]])<-c("actividad","sexo","año")
}


############ transformando a variables binarias todas las bases

for (i in c(1:9)) {
          
lista_filtradas[[i]]$actividad[!(lista_filtradas[[i]]$actividad==1)]<-0
lista_filtradas[[i]]$sexo[lista_filtradas[[i]]$sexo==1]<-0
lista_filtradas[[i]]$sexo[lista_filtradas[[i]]$sexo==2]<-1
lista_filtradas[[i]]$año[!(lista_filtradas[[i]]$año==2020)]<-0
lista_filtradas[[i]]$año[lista_filtradas[[i]]$año==2020]<-1
lista_filtradas[[i]]$actividad[is.na(lista_filtradas[[i]]$actividad)]<-0

}

########## creando variable de interacción entre sexo y año, es decir, tratamiento
#más efecto covid.
for (i in c(1:9)) {
          lista_filtradas[[i]]$interacción<-lista_filtradas[[i]]$sexo*lista_filtradas[[i]]$año
}

head(lista_filtradas[[5]])

################################################################################
####### modelo lineal PARA AMJ
################################################################################

lista_empleo<-vector("list",length = 9L)
lista_sexo<-vector("list",length = 9L)
lista_año<-vector("list",length = 9L)
lista_interacción<-vector("list",length = 9L)
for (i in c(1:9)) {

          lista_empleo[[i]]<-assign(paste("PBB_EMPLEO",i,sep = ""),
                    as.numeric(lista_filtradas[[i]]$actividad))
          lista_sexo[[i]]<-assign(paste("SEXO",i,sep = ""),
                    as.numeric(lista_filtradas[[i]]$sexo))
          lista_año[[i]]<-assign(paste("AÑO",i,sep = ""),
                    as.numeric(lista_filtradas[[i]]$año))
          lista_interacción[[i]]<-assign(paste("INT",i,sep = ""),
                    as.numeric(lista_filtradas[[i]]$interacción))
}

listaY<-vector("list",length = 9L)
listaX<-vector("list",length = 9L)
for (i in c(1:9)) {
          
          listaY[[i]]<-assign(paste("Y",i,sep = ""),lista_empleo[[i]])
          listaX[[i]]<-assign(paste("X",i,sep = ""),as.matrix(cbind(1,
                    lista_sexo[[i]], lista_año[[i]], lista_interacción[[i]])))
}

listaDD<-vector(mode = "list",length = 9L)
for (i in seq_along(listaX)) {
          listaDD[[i]]<-assign(paste("estimadorDD",i,sep = ""),
                    solve(t(listaX[[i]]) %*% listaX[[i]])%*%t(listaX[[i]])%*%listaY[[i]])
}

###### redondeando los valores de los estimadores
for (i in seq_along(listaDD)) {
          round(listaDD[[i]], 5)
}

######## obteniendo los resúmenes de regresiones
lista_ajuste<-vector(mode = "list", length = 9L)
for (i in seq_along(lista_empleo)) {
          lista_ajuste[[i]]<-assign(paste("ajusteDD",i,sep = ""), lm(listaY[[i]] ~ listaX[[i]][,-1]))
}
for (i in seq_along(lista_ajuste)) {
          print(summary(lista_ajuste[[i]]))
}

vector_beta0<-vector("numeric",length = 9L)
vector_beta1<-vector("numeric",length = 9L)
vector_beta2<-vector("numeric",length = 9L)
vector_beta3<-vector("numeric",length = 9L)
for (i in c(1:9)) {
          vector_beta0[[i]]<-listaDD[[i]][[1]]*100
          vector_beta1[[i]]<-listaDD[[i]][[2]]*100
          vector_beta2[[i]]<-listaDD[[i]][[3]]*100
          vector_beta3[[i]]<-listaDD[[i]][[4]]*100
}


plot(vector_beta0, xlab = "Trimestres Móviles 2019-2020",
          ylab = "Puntos porcentuales", type = "b",
          main = "Evolución del estimador de DD, Beta0 en los trimestres de 2019-2020")
plot(vector_beta1, xlab = "Trimestres Móviles 2019-2020",
          ylab = "Puntos porcentuales", type = "b",
          main = "Evolución del estimador de DD, Beta1 en los trimestres de 2019-2020")
plot(vector_beta2, xlab = "Trimestres Móviles 2019-2020",
          ylab = "Puntos porcentuales", type = "b",
          main = "Evolución del estimador de DD, Beta2 en los trimestres de 2019-2020")
plot(vector_beta3, xlab = "Trimestres Móviles 2019-2020",
          ylab = "Puntos porcentuales", type = "b",
          main = "Evolución del estimador de DD, Beta3 en los trimestres de 2019-2020")

