###########################################################
# Wellington Silva                                        #
# https://www.kaggle.com/gatunnopvp                       #
# https://github.com/WellingtonSilv                       #
# https://www.linkedin.com/in/wellington-silva-b430a1195/ #
###########################################################

# descomente e instale as bibliotecas abaixo se ja nao tiver instaladas

#install.packages(lubridate)
#install.packages(reshape2)
#install.packages(tidyverse)

# carregando bibliotecas necessarias
library(lubridate)
library(reshape2)
library(tidyverse)

# importando a base de dados do meu github
asset <- read.csv('https://raw.githubusercontent.com/WellingtonSilv/databases/master/bitcoin_finex.csv'
                  , header = TRUE, sep = ';')

# convertendo a coluna datas para o formato POSIXct
asset$data <- as.character(asset$data)
datas <- asset$data
datas <- as.POSIXct(datas,tz='GMT','%d/%m/%Y')

# pegando o dia do mes, o mes e o ano
asset$mday <- mday(datas)
asset$month <- month(datas)
asset$year <- year(datas)

# 1 = comprando apenas nos negativos
# 2 = comprando apenas nos potivos
# 3 = comprando em todos os meses

#### ---------- comprando um total de 10000 reais ao longo do tempo ------ ####

# comprando apenas nos negativos
parcelas1 <- length(asset$close[asset$mday==3
                                & (asset$month==1
                                   | asset$month==3
                                   | asset$month==9)])
asset$btc1 <- 0
asset$btc1[asset$mday==3
           & (asset$month==1
              | asset$month==3
              | asset$month==9)] <- (10000/parcelas1)/asset$close[asset$mday==3
                                                                  & (asset$month==1
                                                                     | asset$month==3
                                                                     | asset$month==9)]

# comprando apenas nos positivos
parcelas2 <- length(asset$close[asset$mday==3
                                & asset$month!=1
                                & asset$month!=3
                                & asset$month!=9])
asset$btc2 <- 0
asset$btc2[asset$mday==3
           & asset$month!=1
           & asset$month!=3
           & asset$month!=9] <- (10000/parcelas2)/asset$close[asset$mday==3
                                                              & asset$month!=1
                                                              & asset$month!=3
                                                                & asset$month!=9]

# comprando totos os meses
parcelas3 <- length(asset$close[asset$mday==3])
asset$btc3 <- 0
asset$btc3[asset$mday==3] <- (10000/parcelas3)/asset$close[asset$mday==3]

# calculando o resultado acumulado
asset$acum_btc1 <- cumsum(asset$btc1)
asset$acum_btc1 <- asset$acum_btc1*asset$close
asset$acum_btc2 <- cumsum(asset$btc2)
asset$acum_btc2 <- asset$acum_btc2*asset$close
asset$acum_btc3 <- cumsum(asset$btc3)
asset$acum_btc3 <- asset$acum_btc3*asset$close

# calculando os retornos do Bitcoin
asset$returns <- asset$close/asset$open-1

# calculado os retornos acumulados em cada mes de cada ano
acumulated <- aggregate(asset$returns,list(asset$year,asset$month),sum)
acumulated$data <- paste('28','/',acumulated$Group.2,'/',acumulated$Group.1,sep = '')
acumulated$data <- as.POSIXct(acumulated$data,tz='GMT','%d/%m/%Y')

acumulated <-acumulated[order(acumulated$data),]
acumulated$color <- ifelse(acumulated$x>0,'positivos','negativos')
names(acumulated)[3] <- 'acumulado'

# plotando grafico do acumulado por mes em cada ano
ggplot(acumulated, aes(x = data, y = acumulado, fill = color))+
  ggtitle(paste('Acumulado do mês a cada ano com média de:'
                , round(mean(acumulated$acumulado), 2)*100, '%'))+
  geom_col()+
  geom_hline(yintercept = mean(acumulated$acumulado))

# calculando a porcentagem de meses positivos e negativos
pos_neg <- summary(as.factor(acumulated$color))
pos_neg_per <- c(pos_neg[1]/sum(pos_neg),pos_neg[2]/sum(pos_neg))

# plotando um grafico com as porcentagens
pie(pos_neg_per
    , col = c('red','green')
    , main = 'Proporcao de meses positivos e dias negativos')
text(0.1, -0.3, paste(round(pos_neg_per[2]*100,2), "%", sep = ''), col = "black")
text(0, 0.4, paste(round(pos_neg_per[1]*100,2), "%", sep = ''), col = "black")

# calculando o acumulado por mes
by_month <- aggregate(asset$returns,list(asset$month),sum)
names(by_month) <- c('meses','acumulado')
by_month$meses <- month(by_month$meses,label = T)
by_month$color <- ifelse(by_month$acumulado>0,'positivos','negativos')

# plotando o acumulado por mes
ggplot(by_month, aes(x = meses, y = acumulado, fill = color))+
  ggtitle('Acumulado por mês')+
  geom_col()

# pegando os resultados acumulados das compras
dados <- asset[,12:14]
dados$data <- datas

# convertendo para plotar com ggplot
dados <- melt(dados, id="data")

# plotando os resultados acumulados
ggplot(dados, aes(x = data, y = log(value), colour = variable))+
  ggtitle('Acumulado comprado apenas na baixa, apenas na alta e em ambos')+
  geom_line()