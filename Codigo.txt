# FUNDAMENTOS DE ESTATÍSTICA APLICADA 2022
# LINGUAGEM DE PROGRAMAÇÃO APLICADA R 2022
#
# Grupo:
#   Ismael Weslley Neves de Brito	- 2018101710
#   Leonardo Monteiro Assumpção - 2018200873
#

#Importações---------------------------------------------------------------------------------------------------------#

#Importa a biblioteca necessária para ler os dados da PLanilha
library(readxl)

#Variáveis-----------------------------------------------------------------------------------------------------------#

#Define o nome dos Anos
anos <- c("1ºAno","2ºAno","3ºAno","4ºAno","5ºAno","6ºAno","7ºAno","8ºAno","9ºAno")


#Define o Endereço em que a planilha de dados se localiza
url<-c("C:\\Users\\leloe\\Desktop\\TrabalhoR\\escolas_media_alunos_turma_2010.xls")

#Define o tipo dos dados de cada coluna da planilha
colunas<-c("numeric","text","text","numeric","text","numeric","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")

#Importações----------------------------------------------------------------------------------------------------------#

#Obtem os dados de cada planilha separados por região

#NORTE
N <- read_excel(path=url,sheet = 1,skip=8,na ="--",col_types=colunas)   

#NORDESTE A  Exceto Maranhão e Bahia
NDa <- read_excel(path=url,sheet = 2,skip=8,na ="--",col_types=colunas) 

#NORDESTE B Somente Maranhão e Bahia
NDb <- read_excel(path=url,sheet = 3,skip=8,na ="--",col_types=colunas) 

#SUDESTE
SD <- read_excel(path=url,sheet = 4,skip=8,na ="--",col_types=colunas)  

#SUL
S <- read_excel(path=url,sheet = 5,skip=8,na ="--",col_types=colunas)  

#CENTRO-OESTE
CO <- read_excel(path=url,sheet = 6,skip=8,na ="--",col_types=colunas) 

#UNINDO BASES
TOTAL=rbind(N,NDa,NDb,SD,S,CO)


#Funções-------------------------------------------------------------------------------------------------------------#

#Função que Calcula a média de cada dado por ano, sendo necessário carregar as regiões antes de usar
mediasAno<-function(i){
  
  #Calcula as médias removendo os valores na
  mediasPorAno<-c(round(mean(N[[i+15]], na.rm=TRUE),2),
                  round(mean(NDa[[i+15]], na.rm=TRUE),2),
                  round(mean(NDb[[i+15]], na.rm=TRUE),2),
                  round(mean(SD[[i+15]], na.rm=TRUE),2),
                  round(mean(S[[i+15]], na.rm=TRUE),2),
                  round(mean(CO[[i+15]], na.rm=TRUE),2))
  
  #Exibe o gráfico das médias
  bar<-barplot(mediasPorAno,
               names.arg=c("Norte","NordesteA","NordesteB","Sudeste","Sul","Centro-Oeste"),
               main = paste(i,"º Ano"),
               ylab = "Região",
               col = c("lightblue","green","yellow","Lavender","pink","tomato"),
               xlab = "Média",
               xlim=c(0,30),
               horiz = TRUE)
  
  #Coloca o valor das médias no gráfico
  text(x = mediasPorAno-4, y = bar, labels = mediasPorAno)
  
  #Exibe as médias por ano
  print(mediasPorAno)
  
  return (mediasPorAno)
}




#Função que exibe os dados de uma região
#Exivindo seu gráfico e seus dados em forma de uma tabela
#sendo extremamente fácil visualizar cada dado
dados<-function(tb,visual,nome){
  
  
  #Cria função para fazer gradiente nas cores
  colfunc <- colorRampPalette(c(visual, "white"))
  
  
  # a = Valor Mínimo
  # b = Valor Máximo
  # c = Desvio Padrão
  # d = Mediana
  # e = Quartis 1 2 3 4
  # medias = Médias
  # f = Outliers Identificados não mostrando outliers repetidos
  
  
  #Foi utilizado [i+15] para encontrar o indice das colunas dos anos 1 até 9
  
  
  #VALORES MÍNIMOS
  a<-c()
  for (i in 1:9)
    a<-c(a, min(tb[i+15], na.rm = TRUE) )
  
  
  #VALORES MÁXIMOS
  b<-c()
  for (i in 1:9)
    b<-c(b, max(tb[i+15], na.rm = TRUE) )
  
  #DESVIO PADRÃO
  c<-c()
  for (i in 1:9)
    c<-c(c, sd(tb[[i+15]],na.rm = TRUE))
  
  #MEDIANA
  d<-c()
  for (i in 1:9)
    d<-c(d, median(tb[[i+15]],na.rm = TRUE))
  
  
  #QUARTIS
  quar<-function(n){
    return (quantile(tb[[15+i]],na.rm = TRUE,probs = n))
  }
  e<-c()
  for (i in 1:9){
    e<-c(e,paste(quar(0.25),quar(0.50),quar(0.75),quar(1)))
  }
  
  #MÉDIAS
  medias<-c()
  for (i in 1:9)
    medias<-c(medias, round(mean(tb[[i+15]], na.rm=TRUE),2))
  
  #OUTLIERS
  f<-c()
  for (i in 1:9)
    f<-c(f,paste(sort(unique(boxplot.stats(tb[[15+i]])$out)),collapse = ' '))
  
  #GRÁFICO
  
  bar<-barplot(medias,
               names.arg=anos,
               main = c(nome,"Média de alunos"),
               xlab = "Ano",
               col = colfunc(12),
               ylab = "Média",
               ylim=c(0,30))
  
  
  
  text(x = bar, y = medias-1, labels = medias)
  
  #table
  
  cat("\n\n")
  df <- data.frame(ano=anos,
                   Medias=medias,
                   Min=a,
                   Max=b,
                   Desvio=c,
                   Mediana=d,
                   Quartis=e,
                   Outliers=f)
  
  View(df)
  return (df)
}

#Execuções----------------------------------------------------------------------------------------------------------#


N_Dados<-dados(N,"lightblue","Norte")

NDa_Dados<-dados(NDa,"green","NordesteA")

NDb_Dados<-dados(NDb,"yellow","NordesteB")

SD_Dados<-dados(SD,"Lavender","Sudeste")

S_Dados<-dados(S,"pink","Sul")

CO_Dados<-dados(CO,"tomato","Centro-Oeste")

TOTAL_Dados<-dados(TOTAL,"orange","Total")


medias_1<-mediasAno(1)

medias_2<-mediasAno(2)

medias_3<-mediasAno(3)

medias_4<-mediasAno(4)

medias_5<-mediasAno(5)

medias_6<-mediasAno(6)

medias_7<-mediasAno(7)

medias_8<-mediasAno(8)

medias_9<-mediasAno(9)

