library(tidyverse)
library(gridExtra)


df_tri = read.csv("data/acoesb3.csv")

df_day = read.csv("data/acoesb3cot.csv")


#gridExtra::grid.table(df_day %>% slice(1:20)) # para plotar o db

ult_cot = cod = cotAtual = divY = c()

for (i in 1:length(df_day[,"ultCot"])){
  if (df_day[i,"cod"] %in% cod){
    j = match(df_day[i,"cod"], cod)
    if (df_day[i,"ultCot"]>df_day[j,"ultCot"]){
      ult_cot = ult_cot[-j]; cod = cod[-j]; cotAtual = cotAtual[-j];
      divY = divY[-j]
      
      ult_cot = c(ult_cot, df_day[i,"ultCot"])
      cod = c(cod, df_day[i,"cod"])
      cotAtual = c(cotAtual, df_day[i,"cotAtual"])
      divY = c(divY, df_day[i, "divYield"])
    }
  } else {
    ult_cot = c(ult_cot, df_day[i,"ultCot"])
    cod = c(cod, df_day[i,"cod"])
    cotAtual = c(cotAtual, df_day[i,"cotAtual"])
    divY = c(divY, df_day[i, "divYield"])
  }
  
  
}
df_day_fil = data.frame(cod, cotAtual, divY, ult_cot)


ult_bal = codigo = roic = cres_rec5 = n_ac = divb = disp = ativc = c();
ativ = patl = recl12 = ebit12 = Lucl12 = recl3 = c()

ult_cot_t = cotAtual_t = divY_t = c()
for (i in 1:length(df_tri[,"ultBal"])){
  if (df_tri[i,"ultBal"] == "2023-03-31"){
    j = match(df_tri[i,"codigo"], cod)
    ult_cot_t = c(ult_cot_t, ult_cot[j])
    cotAtu(cotAtual_t = cal_t, cotAtual[j])
    ult_bal =  c(ult_bal, df_tri[i,"ultBal"])
    codigo = c(codigo, df_tri[i,"codigo"])
    roic = c(roic, df_tri[i,"roic"])
    cres_rec5 = c(cres_rec5, df_tri[i,"cresRec5a"])
    divY_t = c(divY_t, divY[j])
    n_ac = c(n_ac, df_tri[i,"nAcoes"])
    divb = c(divb, df_tri[i,"divBruta"])
    disp = c(disp, df_tri[i,"disponib"])
    ativc = c(ativc, df_tri[i,"ativCirc"])
    ativ = c(ativ, df_tri[i,"ativos"])
    patl = c(patl, df_tri[i,"patLiq"])
    recl12 = c(recl12, df_tri[i,"recLiq12m"])
    ebit12 = c(ebit12, df_tri[i,"ebit12m"])
    Lucl12 = c(Lucl12, df_tri[i,"LucLiq12m"])
    recl3 = c(recl3, df_tri[i,"recLiq3m"])
  }
}
ult_bal_dates = c()
for (i in df_tri[,"ultBal"]){
  if (i %in% ult_bal_dates){}
  else{
    ult_bal_dates = c(ult_bal_dates, i)
  }
}
per = c()
for (i in length(df_tri[,"ultBal"])){
  j = match(df_tri[i,"ultBal"], ult_bal_dates)
  per = c(per,data.frame(ult_bal, codigo, roic, cres_rec5, n_ac, divb, disp,
                         ativc, ativ, patl, recl12, ebit12, Lucl12, recl3))
}

df_tri_3t22 = data.frame(ult_cot_t, cotAtual_t, ult_bal, codigo, roic, cres_rec5,
                         divY_t, n_ac, divb, disp,
                         ativc, ativ, patl, recl12, ebit12, Lucl12, recl3,
                         row.names = codigo)


#gridExtra::grid.table(df_tri_3t22 %>% slice(1:20))


pl = round(cotAtual_t/(Lucl12/n_ac), 2)
pv = round(cotAtual_t/(patl/n_ac), 2)
roe = round((Lucl12/patl)*100, 2)
roic = round(roic, 2)
p_cxa = round(cotAtual_t/(disp/n_ac), 2)
p_ativc = round(cotAtual_t/(ativc/n_ac), 2)
p_ativ = round(cotAtual_t/(ativ/n_ac), 2)
divbr_p = round((divb/patl), 2)
div_cxa = round(divb/disp, 2)
marg_ebit = round((ebit12/recl12)*100, 2)
marg_liq = round((Lucl12/recl12)*100, 2)
cres_rec5 = round(cres_rec5, 2)
divY_t = round(divY_t, 2)
lynch = round((divY_t+(cres_rec5/5))/pl, 2)
div_lucm = round(divb/(Lucl12/12), 2)

crit = data.frame(pl ,pv ,roe ,roic ,p_cxa ,p_ativc ,p_ativ , div_cxa,
                  marg_ebit ,marg_liq ,cres_rec5 ,divY_t ,lynch,
                  div_lucm, row.names = codigo)

colnames(crit) = c("P/L", "P/VPA",
                   "ROE",
                   "ROIC",
                   "Preço/(Caixa/Ação)", "Preço/(Ativos Circulantes/Ação)",
                   "Preço/(Ativos/Ação)", "Dív Bruta/Caixa", "Mar. EBITDA",
                   "Marg. Líquida", "Cresc. Rec. (5 Anos)",
                   "Dividendyield", "Lynch",
                   "Dív. Bruta/Lucro Mensal")


rmv_inf_values_row = function(df){
  for (r in 1:length(df[,1])){
    if (is.na(match(Inf, df[r,])) || is.na(match(Inf, df[r,]))){
      
    } else {
      df = df[-r,]
    }
    
  }
  return(df)
}

rmv_neg_pl = function(df){
  pl_neg = c()
  col_i = match("P/L",colnames(df))
  len = length(df[,col_i])
  for (r in 1:len){
    if (!is.na(df[r,col_i])){
      if (df[r,col_i] < 0){
        pl_neg = c(pl_neg,(df[r,"P/L"]))
      }
    }
  }
  
  for (i in pl_neg) {
    if (i %in% df[,col_i]){
      df = df[-match(i,df[,col_i]),]
    }
  }
  return(df)
}

rmv_neg_pvpa = function(df){
  pvpa_neg = c()
  col_i = match("P/VPA",colnames(df))
  len = length(df[,col_i])
  for (r in 1:len){
    if (!is.na(df[r,col_i])){
      if (df[r,col_i] < 0){
        pvpa_neg = c(pvpa_neg,df[r,"P/VPA"])
      }
    }
  }
  for (i in pvpa_neg) {
    if (i %in% df[,col_i]){
      df = df[-match(i,df[,col_i]),]
    }
  }
  return(df)
}

emp_lucpat_neg = c()
for(r in 1:length(df_tri_3t22[,1])){
  if(df_tri_3t22[r,'Lucl12'] < 0 && df_tri_3t22[r,'patl'] < 0){
    emp_lucpat_neg = c(emp_lucpat_neg, df_tri_3t22[r, 'codigo'])
  }
}

roe_err = c()
for (r in emp_lucpat_neg){
  roe_err = c(roe_err,(df_tri_3t22[r,"Lucl12"]/df_tri_3t22[r,'patl'])*100)
}

emp_lucpat_neg = c()
for(r in 1:length(df_tri_3t22[,1])){
  if(df_tri_3t22[r,'Lucl12'] < 0 && df_tri_3t22[r,'patl'] < 0){
    emp_lucpat_neg = c(emp_lucpat_neg, df_tri_3t22[r, 'codigo'])
  }
}


#Criando ranking para atribuir pontos empresas
rank.ac = data.frame(rank(-pl), rank(-pv), rank(roe), rank(roic),
                     rank(-p_cxa), rank(-p_ativc), rank(-p_ativ),
                     rank(-divbr_p), rank(-div_cxa),
                     rank(marg_ebit), rank(marg_liq), rank(cres_rec5),
                     rank(divY_t), rank(lynch), rank(-div_lucm),
                     row.names = codigo)

view(rank.ac)

rmv_neg_pl = function(df){
  pl_neg = c()
  col_i = match("P/L",colnames(df))
  len = length(df[,col_i])
  for (r in 1:len){
    if (!is.na(df[r,col_i])){
      if (df[r,col_i] < 0){
        pl_neg = c(pl_neg,(df[r,"P/L"]))
      }
    }
  }
  
  for (i in pl_neg) {
    if (i %in% df[,col_i]){
      df = df[-match(i,df[,col_i]),]
    }
  }
  return(df)
}

rmv_neg_pvpa = function(df){
  pvpa_neg = c()
  col_i = match("P/VPA",colnames(df))
  len = length(df[,col_i])
  for (r in 1:len){
    if (!is.na(df[r,col_i])){
      if (df[r,col_i] < 0){
        pvpa_neg = c(pvpa_neg,df[r,"P/VPA"])
      }
    }
  }
  for (i in pvpa_neg) {
    if (i %in% df[,col_i]){
      df = df[-match(i,df[,col_i]),]
    }
  }
  return(df)
}

res = rmv_neg_pl(rank.ac)
length(crit[,1])
res = rmv_neg_pvpa(res)
length(crit[,1])

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