#Importar os dados do excel
#carregar biblioteca
#importando de todas as acoes
library(readxl)
setwd("G:/Meu Drive/financas/investimentos/acoes/")
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
                                  "numeric"),
                    sheet = "acoes")

#importando das acoes na carteira
## OBS: o range ? de acordo com a qtd de acoes na carteira
part.xlsx = read_excel(
  "G:/Meu Drive/financas/investimentos/carteira/participacao.xlsx", 
                  range = "a1:a8", col_types = c("text"), sheet = "acoes")

#Extra?ndo os valores das vari?veis e colocamdo em vetores
cod = c()
for(i in acoes[,"Codigo"]){
  cod = c(cod,i)
}

part = c()
for (i in part.xlsx) {
  part = c(part,i)
}

p.l = c()
for (i in acoes["P/L"]){
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

#Criando ranking para atribuir pontos as empresas
rank.ac = data.frame(1.5*rank(-p.l),1.2*rank(-p.vpa),rank(roe),rank(roic),
                     1.2*rank(-p.cxa), rank(-divbr.p),rank(-divbr.cx),
                     rank(marg.ebit),rank(marg.liq),1.3*rank(cres.rec),
                     1.2*rank(div.yeld),1.2*rank(lynch),
                     0.8*rank(per.resist),1.2*rank(-div.Lucm),
                     rank(-desv.pad),cod , row.names = cod)

#colocando os valores que pertencem a carteira em vetores
pl.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    pl.c = c(pl.c,rank.ac[i,1])
  }
}
pvpa.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    pvpa.c = c(pvpa.c,rank.ac[i,2])
  }
}
roe.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    roe.c = c(roe.c,rank.ac[i,3])
  }
}
roic.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    roic.c = c(roic.c,rank.ac[i,4])
  }
}
pxa.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    pxa.c = c(pxa.c,rank.ac[i,5])
  }
}
divbr.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    divbr.c = c(divbr.c,rank.ac[i,6])
  }
}
divbr.cx.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    divbr.cx.c = c(divbr.cx.c,rank.ac[i,7])
  }
}
marg.ebit.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    marg.ebit.c = c(marg.ebit.c,rank.ac[i,8])
  }
}
marg.liq.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    marg.liq.c = c(marg.liq.c,rank.ac[i,9])
  }
}
cres.rec.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    cres.rec.c = c(cres.rec.c,rank.ac[i,10])
  }
}
div.y.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    div.y.c = c(div.y.c,rank.ac[i,11])
  }
}
lynch.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    lynch.c = c(lynch.c,rank.ac[i,12])
  }
}
perres.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    perres.c = c(perres.c,rank.ac[i,13])
  }
}
divluc.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    divluc.c = c(divluc.c,rank.ac[i,14])
  }
}
desv.pad.c = c()
for (i in rank.ac[,"cod"]) {
  if(i%in%part){
    desv.pad.c = c(desv.pad.c,rank.ac[i,15])
  }
}

#colocando os valores em um data frame
cart.dat = data.frame(pl.c, pvpa.c, roe.c, roic.c, pxa.c, divbr.c,
                  divbr.cx.c, marg.ebit.c, marg.liq.c, cres.rec.c,
                  div.y.c, lynch.c, perres.c, divluc.c, desv.pad.c,
                  row.names = head(part,-1))

#colocando os valores em vetores
ep1 = sum(cart.dat[1,])
ep2 = sum(cart.dat[2,])
ep3 = sum(cart.dat[3,])
ep4 = sum(cart.dat[4,])
ep5 = sum(cart.dat[5,])
ep6 = sum(cart.dat[6,])
ep7 = sum(cart.dat[7,])
ep8 = sum(cart.dat[8,])
ep9 = sum(cart.dat[9,])
ep10 = sum(cart.dat[10,])

#Colocando os valores em um vetor
res = c(ep1,ep2,ep3,ep4,ep5,ep6,ep7,ep8,ep9,ep10)

#retirando os valores vazios
n.res = res[! res %in% NA]

#criando um vetor para os valores
cod.c = c()
for(i in rank.ac[,"cod"]){
  if(i%in%part){
    cod.c = c(cod.c,i)
  }
}

#colocando o banco no vetor de codigos
bancos <- read_excel("G:/Meu Drive/financas/investimentos/acoes/bancos.xlsx", 
                     col_types = c("text", "date", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric"))
cod = c()
for(i in bancos[,"Codigo"]){
  cod = c(cod,i)
}

for(i in cod){
  if(i%in%part){
    cod.c = c(cod.c,i)
  }
}

#Plotando o gr?fico 
pct =round(n.res/sum(n.res)*87.5,2)  # 100% - porcentagem do banco
pct = c(pct,12.50)        #valor do banco para completar o 100%
cod.c.pct = paste(cod.c,pct)
cod.c.pct.perc = paste(cod.c.pct,"%",sep = " ")
pie(pct,labels = cod.c.pct.perc,col = rainbow(length(pct)),
    main = "Porcentagem das Ações na Carteira")
