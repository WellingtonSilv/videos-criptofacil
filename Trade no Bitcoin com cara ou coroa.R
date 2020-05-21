###########################################################
# Wellington Silva                                        #
# https://www.kaggle.com/gatunnopvp                       #
# https://github.com/WellingtonSilv                       #
# https://www.linkedin.com/in/wellington-silva-b430a1195/ #
###########################################################

# descomente e instale as bibliotecas abaixo se ja nao tiver instaladas

#install.packages('tidyverse')
#install.packages('reshape2')

# carregando bibliotecas necessarias
library(reshape2)
library(tidyverse)

# importando a base de dados do meu github
asset <- read.csv('https://raw.githubusercontent.com/WellingtonSilv/databases/master/bitcoin_finex.csv'
                  , header = TRUE, sep = ';')

# calculando os retornos do bitcoin
asset$returns <- asset$close/asset$open-1

# calculando a porcentagem de dias positivos
asset$alta_baixa <- ifelse(asset$returns>0,'alta','baixa')
pos_neg <- summary(as.factor(asset$alta_baixa))
pos_neg_per <- c(pos_neg[1]/sum(pos_neg),pos_neg[2]/sum(pos_neg))

# plotando um grafico com as porcentagens
pie(pos_neg_per
    , col = c('green','red')
    , main = 'Proporcao de dias positivos e dias negativos')
text(0, 0.4, paste(round(pos_neg_per[1]*100,2), "%", sep = ''), col = "black")
text(0.1, -0.3, paste(round(pos_neg_per[2]*100,2), "%", sep = ''), col = "black")

# calculando os stops de compra e de venda
asset$stop_buy <- asset$open/asset$low-1
asset$stop_sell <- asset$high/asset$open-1

# defina o stop aqui em porcentagem, exemplo: 1% == 0.01, 3% == 0.03
asset$stop <- 0.01

# defina as taxas e o capital
asset$taxas <- 0.000
asset$capital <- 1000

# defina o numero de simulacoes
n_simu <- 200

testing <- asset
for(i in 1:n_simu){ # loop para calcular e armazenar todos os resultados das simulacoes
  
  # gerando os sinais de trade de forma aleatoria
  testing$trade <- sample(c(1,0),dim(testing)[1],replace = T,prob = c(0.50,0.50))
  
  # calculando os resultados que deram e que nao deram stop
  testing$results <- ifelse(testing$trade==1 & testing$returns>0 & testing$stop_buy<testing$stop
                            ,(testing$returns*testing$capital)-(testing$capital*testing$taxas),NA)
  
  testing$results <- ifelse(testing$trade==1 & testing$returns<0 & testing$stop_buy<testing$stop
                            ,(testing$returns*testing$capital)-(testing$capital*testing$taxas),testing$results)
  
  testing$results <- ifelse(testing$stop_buy>=testing$stop
                            ,(-testing$stop*testing$capital)-(testing$capital*testing$taxas),testing$results)
  
  testing$results <- ifelse(testing$trade==0 & testing$returns<0 & testing$stop_sell<testing$stop
                            ,(-testing$returns*testing$capital)-(testing$capital*testing$taxas),testing$results)
  
  testing$results <- ifelse(testing$trade==0 & testing$returns>0 & testing$stop_sell<testing$stop
                            ,(-testing$returns*testing$capital)-(testing$capital*testing$taxas),testing$results)
  
  testing$results <- ifelse(testing$stop_sell>=testing$stop & is.na(testing$results)==TRUE
                            ,(-testing$stop*testing$capital)-(testing$capital*testing$taxas),testing$results)
  
  testing$results[is.na(testing$results)==TRUE] <- 0
  
  # juntando todas as curvas de resutados a planilha principal 'testing'
  testing[paste('acum_',i,sep='')] <- cumsum(testing$results)
  
  # na ultima simulacao, criar o grafico final
  if(i==n_simu){
    
    # selecionando os acumulados
    acums <- testing[,15:(n_simu+14)]
    acums <- cbind(testing$data,acums)
    names(acums)[1] <- 'data'
    
    # convertendo as datas para objetos do tipo POSIXct
    acums$data <- as.POSIXct(acums$data, tz = 'UTC', '%d/%m/%Y')
    
    # convertendo os dados pra plotar com o ggplot
    acums <- melt(acums, id="data")
    
    # criando o grafico de patrimonio acumulado com todas as curvas
    g <- ggplot(data = acums, aes(x = data, y = value, colour = variable)) +
      ggtitle(paste("Evolucao do patrimonio em",n_simu,"simulacoes"))+
      geom_line()+
      theme(legend.position = "none")
  }
}

# plotando o grafico final
g
