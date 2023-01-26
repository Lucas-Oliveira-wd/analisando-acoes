#Importar os dados do excel
#carregar biblioteca
library(readxl)
setwd("C:/files/projects/programacao/R/analisando-acoes")
acoes <- read_excel("G:/Meu Drive/financas/investimentos/acoes/acoes.xlsm", 
                    col_types = c("text", "text", "date", 
                                  "text", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))
#Extra?ndo os valores das vari?veis e colocamdo em vetores
p.l = c()
for (i in acoes[,"P/L"]) {
  p.l = c(p.l,i)
}
p.vpa = c()
for (i in acoes[,"P/VPA"]) {
  p.vpa = c(p.vpa,i)
}
roe = c()
for (i in acoes[,"ROE"]){
  roe = c(roe,i)
}
roic = c()
for (i in acoes["ROIC (%)"]){
  roic = c(roic,i)
}
p.cxa = c()
for (i in acoes[,"P/Cx.A"]) {
  p.cxa = c(p.cxa,i)
}
divbr.p = c()
for (i in acoes[,"Div.Br/Pat.L"]) {
  divbr.p = c(divbr.p,i)
}
divbr.cx = c()
for(i in acoes[,"Div.Br./Cx."]){
  divbr.cx = c(divbr.cx,i)
}
marg.ebit = c()
for(i in acoes[,"Marg. EBIT"]){
  marg.ebit = c(marg.ebit,i)
}
marg.liq = c()
for(i in acoes[,"Marg. Liquida"]){
  marg.liq = c(marg.liq,i)
}
cres.rec = c()
for(i in acoes[,"Cres. Rec (5a) (%)"]){
  cres.rec = c(cres.rec,i)
}
div.yeld = c()
for(i in acoes[,"DIV. YIELD (%)"]){
  div.yeld = c(div.yeld,i)
}
lynch = c()
for(i in acoes[,"Lynch"]){
  lynch = c(lynch,i)
}
per.resist = c()
for(i in acoes[,"Periodo de resistencia"]){
  per.resist = c(per.resist,i)
}
div.Lucm = c()
for(i in acoes[,"Div/Luc.M"]){
  div.Lucm = c(div.Lucm,i)
}
desv.pad = c()
for (i in acoes[,"Desv.Pad.Rel"]) {
  desv.pad = c(desv.pad,i)
}
cod = c()
for(i in acoes[,"Codigo"]){
  cod = c(cod,i)
}
#Criando ranking para atribuir pontos empresas
rank.ac = data.frame(rank(-p.l), rank(-p.vpa), rank(roe), rank(roic),
                     rank(-p.cxa), rank(-divbr.p), rank(-divbr.cx),
                     rank(marg.ebit), rank(marg.liq), rank(cres.rec),
                     rank(div.yeld), rank(lynch),
                     rank(per.resist), rank(-div.Lucm),
                     rank(-desv.pad), row.names = cod)

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

#retirando as 7 melhores empresas
best7=c()
for (i in ord) {
  if(length(best7)<7)
  best7 = c(best7,cod[i])
  
}
#Colocando os valores em ordem
sor = sort(res,na.last = NA,decreasing = T)
#Retirando os 7 melhores valores
val7 = c()
for (i in sor){
  if(length(val7)<7)
  val7 = c(val7,i)
}
#retirando as empresas em ordem
best.e=c()
for (i in ord) {
    best.e = c(best.e,cod[i])
}

#Retirando os 15 valores em ordem
val.e = c()
for (i in sor){
    val.e = c(val.e,i)
}

#preparando para printar as empresas com os valores em ordem
print.e = data.frame(best.e,val.e)

#colocando o melhor banco para compor a carteira
bancos <- read_excel("G:/Meu Drive/financas/investimentos/acoes/bancos.xlsx", 
                    col_types = c("text", "date", 
                                  "text", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric"))

#Extra?ndo os valores das vari?veis e colocamdo em vetores
p.l = c()
for (i in bancos[,"P/L"]) {
  p.l = c(p.l,i)
}
p.vpa = c()
for (i in bancos[,"P/VPA"]) {
  p.vpa = c(p.vpa,i)
}
roe = c()
for (i in bancos[,"ROE"]){
  roe = c(roe,i)
}
p.at.a = c()
for (i in bancos[,"P/At.A"]) {
  p.at.a = c(p.at.a,i)
}
cres.rec = c()
for(i in bancos[,"Cres. Rec (5a) (%)"]){
  cres.rec = c(cres.rec,i)
}
div.yeld = c()
for(i in bancos[,"DIV. YIELD (%)"]){
  div.yeld = c(div.yeld,i)
}
lynch = c()
for(i in bancos[,"Lynch"]){
  lynch = c(lynch,i)
}
cod = c()
for(i in bancos[,"Codigo"]){
  cod = c(cod,i)
}
#Criando ranking para atribuir pontos empresas
rank.ac = data.frame(rank(-p.l),rank(-p.vpa),rank(roe),rank(-p.at.a),
                     rank(cres.rec),
                     rank(div.yeld),rank(lynch),row.names = cod)

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
#retirando o melhor banco
best =c()
for (i in ord) {
  if(length(best)<1)
    best = c(best,cod[i])
}
#Colocando os valores em ordem
sor = sort(res,na.last = NA,decreasing = T)
#Retirando o melhor valor
val = 12.5

#colocando com os valores das 7 a??es
val8 = c((87.5/sum(val7))*val7,val)

#Plotando o gr?fico
pct =round(val7/sum(val7)*87.5,2)
pct = c(pct,12.5)
best8 = c(best7,best)
best8.pct = paste(best8,pct)
best8.pct.perc = paste(best8.pct,"%",sep = " ")
pie(val8,labels = best8.pct.perc,col = rainbow(length(val8)),main = "Porcentagem das Acoes")

#retirando os bancos em ordem
best.b=c()
for (i in ord) {
    best.b = c(best.b,cod[i])
}

#Retirando os valores em ordem
val.b = c()
for (i in sor){
    val.b = c(val.b,i)
}

#printando os bancos e as empresas em ordem
print.b = data.frame(best.b,val.b)

#Essas s?o as a??es
print(print.e)

#Esses s?o os bancos
print(print.b)