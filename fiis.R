###############################################################################
###########             Analisando o melhor FII               #################
###############################################################################

#Lendo a biblioteca necess?ria
library(readxl)


#importando a planilha de dados
setwd(setwd("G:/Meu Drive/financas/investimentos/"))
fii <- read_excel(
  "G:/Meu Drive/financas/investimentos/fiis/fii.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))

#colocando cada coluna em um vetor
cod = c()
for (i in fii[,'CODIGO']) {
  cod = c(cod,i)  
}

setor = c()
for (i in fii[,'SETOR']) {
  setor = c(setor,i)
}

liq.diaria = c()
for (i in fii[,'LIQUIDEZ DIARIA']) {
  liq.diaria = c(liq.diaria,i)
}

div = c()
for (i in fii[,'DIVIDEND']) {
  div = c(div,i)
}

dy.3m = c()
for (i in fii[,'DY (3M) M']) {
  dy.3m = c(dy.3m,i)
}

dy.6m = c()
for (i in fii[,'DY (6M) M']) {
  dy.6m = c(dy.6m,i)
}

dy.12m = c()
for (i in fii[,'DY (12M) M']) {
  dy.12m = c(dy.12m,i)
}

dy.ano = c()
for (i in fii[,"DY ANO"]) {
  dy.ano = c(dy.ano,i)
}

p.vpa = c()
for (i in fii[,'P/VPA']) {
  p.vpa = c(p.vpa,i)
}

dy.p = c()
for (i in fii[,'DY P']) {
  dy.p = c(dy.p,i)
}

var.p = c()
for (i in fii[,'VARIACAO P']) {
  var.p = c(var.p,i)
}

rent.p = c()
for (i in fii[,'RENTAB. PATR. P']) {
  rent.p = c(rent.p,i)
}

rent.ac = c()
for (i in fii[,'RENTAB. PATR. AC']) {
  rent.ac = c(rent.ac,i)
}

vac.fis = c()
for (i in fii[,'VACANCIA FIS']) {
  vac.fis = c(vac.fis,i)
}

vac.fin = c()
for (i in fii[,'VACANCIA FIN']) {
  vac.fin = c(vac.fin,i)
}

quant = c()
for (i in fii[,'QUANTIDADE']) {
  quant = c(quant,i)
}

desv.p = c()
for (i in fii[,'DESV. PAD. DIV.Y']) {
  desv.p = c(desv.p,i)
}
tend = c()
for (i in fii[,"DIV.Y TENDENCIA"]) {
  tend = c(tend,i)
}

#Ranqueando os fiis e colocando as posi??es em um dataframe
rank.ac = data.frame(.5*rank(liq.diaria), 1.5*rank(div), 1.6*rank(dy.3m),
                    1.7*rank(dy.6m), 1.8*rank(dy.12m), 1.8*rank(dy.ano),
                    1*rank(-p.vpa), 0,8*rank(dy.p), 1.2*rank(var.p),
                    .7*rank(rent.p), 1.1*rank(rent.ac), .6*rank(-vac.fis),
                    .5*rank(-vac.fin), 3*rank(quant), 4*rank(-desv.p),
                    3*rank(tend) ,row.names = cod)

#colocando a som dos pontos em um vetor
res = c()
for (i in 1:500) {
  if (is.na(sum(rank.ac[i,]))){
  }
  else{
    res = c(res,sum(rank.ac[i,]))
  }
}

#Ordenando o vetor
ord = order(res,na.last = NA,decreasing = T)

#retirando as 3 melhores empresas
best3=c()
for (i in ord) {
  if(length(best3)<3)
    best3 = c(best3,cod[i])
  
}

#Colocando os valores em ordem
sor = sort(res,na.last = NA,decreasing = T)

#Retirando os 3 melhores valores
val3 = c()
for (i in sor){
  if(length(val3)<3)
    val3 = c(val3,i)
}

#Plotando o gr?fico
pct = round(val3/sum(val3)*100,2)
best3 = paste(best3,pct)
best3 = paste(best3,"%",sep = " ")
pie(val3,labels = best3,col = rainbow(length(val3)),
    main = "Porcentagem das A??es")

#retirando as 10 melhores empresas
best10=c()
for (i in ord) {
  if(length(best10)<10)
    best10 = c(best10,cod[i])
  
}

#Retirando os 3 melhores valores
val10 = c()
for (i in sor){
  if(length(val10)<10)
    val10 = c(val10,i)
}

#printando os 10 melhores
print.10 = data.frame(best10,val10)
print(print.10)