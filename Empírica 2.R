library(dplyr)
library(foreign)
library(haven)

base_articulo<-read_dta("CardKrueger.dta")
base_articulo$state[is.na(base_articulo$state)]<-0
base_articulo$empft[is.na(base_articulo$empft)]<-0
base_articulo$emppt[is.na(base_articulo$emppt)]<-0
fte_nj<-base_articulo[base_articulo$state==1,c("state","empft","emppt")]
fte_pa<-base_articulo[base_articulo$state==2,c("state","empft","emppt")]
fte_nj$fte<-fte_nj$empft+fte_nj$emppt*0.5
fte_pa$fte<-fte_pa$empft+fte_pa$emppt*0.5

mean(fte_nj$fte)
mean(fte_pa$fte)
sd(fte_nj$fte)


fte_nj2<-base_articulo[base_articulo$state==1,c("state","empft2","emppt2")]
fte_pa2<-base_articulo[base_articulo$state==2,c("state","empft2","emppt2")]
mean(fte_nj2$empft2+fte_nj2$emppt2*0.5, na.rm = T)
mean(fte_pa2$empft2+fte_pa2$emppt2*0.5, na.rm = T)
