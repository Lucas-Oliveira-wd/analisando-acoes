###############################################################################
###########             Analisando o melhor FII               #################
###############################################################################

#Lendo a biblioteca necessária
library(readxl)

#importando a planilha de dados
setwd(setwd("C:/lucas-sam/Ações"))
fii <- read_excel("C:/lucas-sam/Ações/fii.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric"))

#colocando cada coluna em um vetor
cod = c()
for (i in fii[,'CÓDIGO']) {
  cod = c(cod,i)  
}

setor = c()
for (i in fii[,'SETOR']) {
  setor = c(setor,i)
}

liq.diaria = c()
for (i in fii[,'LIQUIDEZ DIÁRIA']) {
  liq.diaria = c(liq.diaria,i)
}

div = c()
for (i in fii[,'DIVIDEND']) {
  div = c(div,i)
}

dy.3m = c()
for (i in fii[,'DY (3M) AC']) {
  dy.3m = c(dy.3m,i)
}

dy.6m = c()
for (i in fii[,'DY (6M) AC']) {
  dy.6m = c(dy.6m,i)
}

dy.12m = c()
for (i in fii[,'DY (12M) AC']) {
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
for (i in fii[,'VARIAÇÃO P']) {
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
for (i in fii[,'VACÂNCIA FIS']) {
  vac.fis = c(vac.fis,i)
}

vac.fin = c()
for (i in fii[,'VACÂNCIA FIN']) {
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

#Ranqueando os fiis e colocando as posições em um dataframe
rank.ac = data.frame(.8*rank(liq.diaria), 1.2*rank(div), 1.3*rank(dy.3m),
                    1.4*rank(dy.6m), 1.5*rank(dy.12m), 1.5*rank(dy.ano),
                    .8*rank(-p.vpa), .7*rank(dy.p), .6*rank(var.p),
                    .9*rank(rent.p), 1.2*rank(rent.ac), .8*rank(-vac.fis),
                    .6*rank(-vac.fin), 4*rank(quant), 5*rank(-desv.p),
                    row.names = cod)

#colocando os pontos em vetores
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
ep51 = sum(rank.ac[51,])
ep52 = sum(rank.ac[52,])
ep53 = sum(rank.ac[53,])
ep54 = sum(rank.ac[54,])
ep55 = sum(rank.ac[55,])
ep56 = sum(rank.ac[56,])
ep57 = sum(rank.ac[57,])
ep58 = sum(rank.ac[58,])
ep59 = sum(rank.ac[59,])
ep60 = sum(rank.ac[60,])
ep61 = sum(rank.ac[61,])
ep62 = sum(rank.ac[62,])
ep63 = sum(rank.ac[63,])
ep64 = sum(rank.ac[64,])
ep65 = sum(rank.ac[65,])
ep66 = sum(rank.ac[66,])
ep67 = sum(rank.ac[67,])
ep68 = sum(rank.ac[68,])
ep69 = sum(rank.ac[69,])
ep70 = sum(rank.ac[70,])
ep71 = sum(rank.ac[71,])
ep72 = sum(rank.ac[72,])
ep73 = sum(rank.ac[73,])
ep74 = sum(rank.ac[74,])
ep75 = sum(rank.ac[75,])
ep76 = sum(rank.ac[76,])
ep77 = sum(rank.ac[77,])
ep78 = sum(rank.ac[78,])
ep79 = sum(rank.ac[79,])
ep80 = sum(rank.ac[80,])
ep81 = sum(rank.ac[81,])
ep82 = sum(rank.ac[82,])
ep83 = sum(rank.ac[83,])
ep84 = sum(rank.ac[84,])
ep85 = sum(rank.ac[85,])
ep86 = sum(rank.ac[86,])
ep87 = sum(rank.ac[87,])
ep88 = sum(rank.ac[88,])
ep89 = sum(rank.ac[89,])
ep90 = sum(rank.ac[90,])
ep91 = sum(rank.ac[91,])
ep92 = sum(rank.ac[92,])
ep93 = sum(rank.ac[93,])
ep94 = sum(rank.ac[94,])
ep95 = sum(rank.ac[95,])
ep96 = sum(rank.ac[96,])
ep97 = sum(rank.ac[97,])
ep98 = sum(rank.ac[98,])
ep99 = sum(rank.ac[99,])
ep100 = sum(rank.ac[100,])
ep101 = sum(rank.ac[101,])
ep102 = sum(rank.ac[102,])
ep103 = sum(rank.ac[103,])
ep104 = sum(rank.ac[104,])
ep105 = sum(rank.ac[105,])
ep106 = sum(rank.ac[106,])
ep107 = sum(rank.ac[107,])
ep108 = sum(rank.ac[108,])
ep109 = sum(rank.ac[109,])
ep110 = sum(rank.ac[110,])
ep111 = sum(rank.ac[111,])
ep112 = sum(rank.ac[112,])
ep113 = sum(rank.ac[113,])
ep114 = sum(rank.ac[114,])
ep115 = sum(rank.ac[115,])
ep116 = sum(rank.ac[116,])
ep117 = sum(rank.ac[117,])
ep118 = sum(rank.ac[118,])
ep119 = sum(rank.ac[119,])
ep120 = sum(rank.ac[120,])
ep121 = sum(rank.ac[121,])
ep122 = sum(rank.ac[122,])
ep123 = sum(rank.ac[123,])
ep124 = sum(rank.ac[124,])
ep125 = sum(rank.ac[125,])
ep126 = sum(rank.ac[126,])
ep127 = sum(rank.ac[127,])
ep128 = sum(rank.ac[128,])
ep129 = sum(rank.ac[129,])
ep130 = sum(rank.ac[130,])
ep131 = sum(rank.ac[131,])
ep132 = sum(rank.ac[132,])
ep133 = sum(rank.ac[133,])
ep134 = sum(rank.ac[134,])
ep135 = sum(rank.ac[135,])
ep136 = sum(rank.ac[136,])
ep137 = sum(rank.ac[137,])
ep138 = sum(rank.ac[138,])
ep139 = sum(rank.ac[139,])
ep140 = sum(rank.ac[140,])
ep141 = sum(rank.ac[141,])
ep142 = sum(rank.ac[142,])
ep143 = sum(rank.ac[143,])
ep144 = sum(rank.ac[144,])
ep145 = sum(rank.ac[145,])
ep146 = sum(rank.ac[146,])
ep147 = sum(rank.ac[147,])
ep148 = sum(rank.ac[148,])
ep149 = sum(rank.ac[149,])
ep150 = sum(rank.ac[150,])
ep151 = sum(rank.ac[151,])
ep152 = sum(rank.ac[152,])
ep153 = sum(rank.ac[153,])
ep154 = sum(rank.ac[154,])
ep155 = sum(rank.ac[155,])
ep156 = sum(rank.ac[156,])
ep157 = sum(rank.ac[157,])
ep158 = sum(rank.ac[158,])
ep159 = sum(rank.ac[159,])
ep160 = sum(rank.ac[160,])
ep161 = sum(rank.ac[161,])
ep162 = sum(rank.ac[162,])
ep163 = sum(rank.ac[163,])
ep164 = sum(rank.ac[164,])
ep165 = sum(rank.ac[165,])
ep166 = sum(rank.ac[166,])
ep167 = sum(rank.ac[167,])
ep168 = sum(rank.ac[168,])
ep169 = sum(rank.ac[169,])
ep170 = sum(rank.ac[170,])
ep171 = sum(rank.ac[171,])
ep172 = sum(rank.ac[172,])
ep173 = sum(rank.ac[173,])
ep174 = sum(rank.ac[174,])
ep175 = sum(rank.ac[175,])
ep176 = sum(rank.ac[176,])
ep177 = sum(rank.ac[177,])
ep178 = sum(rank.ac[178,])
ep179 = sum(rank.ac[179,])
ep180 = sum(rank.ac[180,])
ep181 = sum(rank.ac[181,])
ep182 = sum(rank.ac[182,])
ep183 = sum(rank.ac[183,])
ep184 = sum(rank.ac[184,])
ep185 = sum(rank.ac[185,])
ep186 = sum(rank.ac[186,])
ep187 = sum(rank.ac[187,])
ep188 = sum(rank.ac[188,])
ep189 = sum(rank.ac[189,])
ep190 = sum(rank.ac[190,])
ep191 = sum(rank.ac[191,])

#Colocando os valores do pontos em um vetor
res = c(ep1,ep2,ep3,ep4,ep5,ep6,ep7,ep8,ep9,ep10,
        ep11,ep12,ep13,ep14,ep15,ep16,ep17,ep18,ep19,ep20,
        ep21,ep22,ep23,ep24,ep25,ep26,ep27,ep28,ep29,ep30,
        ep31,ep32,ep33,ep34,ep35,ep36,ep37,ep38,ep39,ep40,
        ep41,ep42,ep43,ep44,ep45,ep46,ep47,ep48,ep49,ep50,
        ep51,ep52,ep53,ep54,ep55,ep56,ep57,ep58,ep59,ep60,
        ep61,ep62,ep63,ep64,ep65,ep66,ep67,ep68,ep69,ep70,
        ep71,ep72,ep73,ep74,ep75,ep76,ep77,ep78,ep79,ep80,
        ep81,ep82,ep83,ep84,ep85,ep86,ep87,ep88,ep89,ep90,
        ep91,ep92,ep93,ep94,ep95,ep96,ep97,ep98,ep99,ep100,
        ep101,ep102,ep103,ep104,ep105,ep106,ep107,ep108,ep109,ep110,
        ep111,ep112,ep113,ep114,ep115,ep116,ep117,ep118,ep119,ep120,
        ep121,ep122,ep123,ep124,ep125,ep126,ep127,ep128,ep129,ep130,
        ep131,ep132,ep133,ep134,ep135,ep136,ep137,ep138,ep139,ep140,
        ep141,ep142,ep143,ep144,ep145,ep146,ep147,ep148,ep149,ep150,
        ep151,ep152,ep153,ep154,ep155,ep156,ep157,ep158,ep159,ep160,
        ep161,ep162,ep163,ep164,ep165,ep166,ep167,ep168,ep169,ep170,
        ep171,ep172,ep173,ep174,ep175,ep176,ep177,ep178,ep179,ep180,
        ep181,ep182,ep183,ep184,ep185,ep186,ep187,ep188,ep189,ep190,
        ep191)

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

#Plotando o gráfico
pct = round(val3/sum(val3)*100,2)
best3 = paste(best3,pct)
best3 = paste(best3,"%",sep = " ")
pie(val3,labels = best3,col = rainbow(length(val3)),
    main = "Porcentagem das Ações")

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