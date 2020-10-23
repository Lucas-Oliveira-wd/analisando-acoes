#Importar os dados do excel
#carregar biblioteca
library(readxl)
setwd("C:/LUCAS/Ações")
acoes = read_xlsx("acoes.xlsx",col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#Extraíndo os valores das variáveis e colocamdo em vetores
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
for (i in acoes[,"Dív.Br.A/P"]) {
  divbr.p = c(divbr.p,i)
}
divbr.cx = c()
for(i in acoes[,"Dív.Br./Cx."]){
  divbr.cx = c(divbr.cx,i)
}
marg.ebit = c()
for(i in acoes[,"Marg. EBIT"]){
  marg.ebit = c(marg.ebit,i)
}
marg.liq = c()
for(i in acoes[,"Marg. Líquida"]){
  marg.liq = c(marg.liq,i)
}
cres.rec = c()
for(i in acoes[,"Cres. Rec (5a) (%)"]){
  cres.rec = c(cres.rec,i)
}
div.yeld = c()
for(i in acoes[,"DIV. YELD (%)"]){
  div.yeld = c(div.yeld,i)
}
lynch = c()
for(i in acoes[,"Lynch"]){
  lynch = c(lynch,i)
}
per.resist = c()
for(i in acoes[,"Período de resistência"]){
  per.resist = c(per.resist,i)
}
div.Lucm = c()
for(i in acoes[,"Div/Luc.M"]){
  div.Lucm = c(div.Lucm,i)
}
cod = c()
for(i in acoes[,"Código"]){
  cod = c(cod,i)
}
#Criando ranking para atribuir pontos empresas
rank.ac = data.frame(rank(-p.l),rank(-p.vpa),rank(roe),rank(roic),rank(-p.cxa),
                     rank(-divbr.p),rank(-divbr.cx),rank(marg.ebit),
                     rank(marg.liq),rank(cres.rec),rank(div.yeld),rank(lynch),
                     rank(per.resist),rank(-div.Lucm),row.names = cod)
ep1 = sum(rank.ac[1,])
ep2 = sum(rank.ac[2,])
ep3 = sum(rank.ac[3,])
ep4 = sum(rank.ac[4,])
ep5 = sum(rank.ac[5,])
ep6 = sum(rank.ac[6,])
ep7 = sum(rank.ac[7,])
ep8 = sum(rank.ac[8,])
ep9 = sum(rank.ac[9,])
ep10 = sum(rank.ac[10,])
ep11 = sum(rank.ac[11,])
ep12 = sum(rank.ac[12,])
ep13 = sum(rank.ac[13,])
ep14 = sum(rank.ac[14,])
ep15 = sum(rank.ac[15,])
ep16 = sum(rank.ac[16,])
ep17 = sum(rank.ac[17,])
ep18 = sum(rank.ac[18,])
ep19 = sum(rank.ac[19,])
ep20 = sum(rank.ac[20,])
ep21 = sum(rank.ac[21,])
ep22 = sum(rank.ac[22,])
ep23 = sum(rank.ac[23,])
ep24 = sum(rank.ac[24,])
ep25 = sum(rank.ac[25,])
ep26 = sum(rank.ac[26,])
ep27 = sum(rank.ac[27,])
ep28 = sum(rank.ac[28,])
ep29 = sum(rank.ac[29,])
ep30 = sum(rank.ac[30,])
ep31 = sum(rank.ac[31,])
ep32 = sum(rank.ac[32,])
ep33 = sum(rank.ac[33,])
ep34 = sum(rank.ac[34,])
ep35 = sum(rank.ac[35,])
ep36 = sum(rank.ac[36,])
ep37 = sum(rank.ac[37,])
ep38 = sum(rank.ac[38,])
ep39 = sum(rank.ac[39,])
ep40 = sum(rank.ac[40,])
ep41 = sum(rank.ac[41,])
ep42 = sum(rank.ac[42,])
ep43 = sum(rank.ac[43,])
ep44 = sum(rank.ac[44,])
ep45 = sum(rank.ac[45,])
ep46 = sum(rank.ac[46,])
ep47 = sum(rank.ac[47,])
ep48 = sum(rank.ac[48,])
ep49 = sum(rank.ac[49,])
ep50 = sum(rank.ac[50,])

#Colocando os valores em um vetor
res = c(ep1,ep2,ep3,ep4,ep5,ep6,ep7,ep6,ep7,ep8,ep9,ep10,ep11,ep12,ep13,ep14,ep15,ep16,ep17,ep18,ep19,ep20,ep21,ep22)
#Ordenando o vetor
ord = order(res,na.last = NA,decreasing = T)
#retirando as 8 melhores empresas
best8=c()
for (i in ord) {
  if(length(best8)<8)
  best8 = c(best8,cod[i])
  
}
#Colocando os valores em ordem
sor = sort(res,na.last = NA,decreasing = T)
#Retirando os 8 melhores valores
val8 = c()
for (i in sor){
  if(length(val8)<8)
  val8 = c(val8,i)
}
#Plotando o gráfico
pct =round(val8/sum(val8)*100,2)
best8 = paste(best8,pct)
best8 = paste(best8,"%",sep = " ")
pie(val8,labels = best8,col = rainbow(length(val8)),main = "Porcentagem das Ações")