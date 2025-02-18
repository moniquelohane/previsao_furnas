# install.packages("pacman")
require(pacman)

p_load(forecast, tidyverse, readxl, gridExtra, knitr, forecTheta, Mcomp, timeSeries, tseries)


# (0) - Leitura dos dados ----
if(length(args)>1){
  pasta_raiz <- args[1]
}else{
  pasta_raiz <- dirname(rstudioapi::getSourceEditorContext()$path)
}

setwd(paste0(pasta_raiz, "./../data/processed"))

dados <- read_excel("dados_hidrologicos.xlsx", skip = 1, col_names = F)

# Pontos a serem analisados:
# 1. decomposição ok
# 2. seleção de modelos ok 
# 3. análise de resíduos ok
# 4. estudo da capacidade preditiva pontual e intervalar ok
# 5. previsão pontual e previsão intervalar. ok


# (1) - Transformações nos dados ----
dados <- t(dados)
dados <- as.data.frame(dados)
dados <- dados[5:nrow(dados),]
row.names(dados) <- NULL
names(dados) <- c("data",'valor')
dados$valor <- as.numeric(dados$valor)


dados$data <- paste(rep(2010:2021, each=12),format(ISOdate(2000, 1:12, 1), "%b"),rep(11,144), sep="")
dados$data <- as.Date(as.character(dados$data), "%Y%b%d")
data <- dados$data
valor <- dados$valor

hid.ts <- timeSeries(valor,data)
hid.ts2 <- ts(valor, frequency = 12, start = c(2010, 1))

# (2) - Visualização dos dados ----
setwd("./../../reports/figures")

fig1 <- autoplot(hid.ts2) + labs(x = "Ano", y = 'Nível de montante (m)') +
  labs(title = "Níveis hidrológicos no Brasil, 2010-2021") +
  theme_bw()
# ggsave(filename = "fig1.png",plot = fig1, device = 'png', width = 7, height = 5,units="in", dpi = 300)


# (3) - Decomposição da serie ----
decomp.mstl <- mstl(hid.ts2)
fig2 <- autoplot(decomp.mstl) +
  labs(x = "Ano") +
  labs(title = "Decomposição MSTL da série de níveis\nhidrologicos, 2010-2021") +
  theme_bw()
# ggsave(filename = "fig2.png",plot = fig2, device = 'png', width = 5, height = 7, units="in", dpi = 300)

# (4) - Seleção de modelos ----

# (4.1) - Estacionariedade da serie ----
kpss.test(hid.ts)

# p-value = 0.01
# a serie rejeitou estacionariedade a um nivel de significancia alpha de 0.05
ndiffs(hid.ts2) # 1

# w: serie com uma diferença simples
w <- diff(hid.ts2)

fig3 <- autoplot(w) +
  labs(x = "Ano", y = 'Diferença Nível de montante') +
  labs(title = 'Série de níveis hidrológicos com uma diferença, 2010-2022') +
  theme_bw()
# ggsave(filename = "fig3.png",plot = fig3, device = 'png', width = 7, height = 5, units="in", dpi = 300)

kpss.test(w)
# p-value = 0.1
# nao rejeitou a estacionariedade, porem ela ainda apresenta a componente sazonal e 
# portanto ainda nao é estacionaria

# (4.2) - Analise da FAC e FACP ----

p0 <- autoplot(w) +
  labs(title = "Série do nível de montante (m) com 1 diferença simples") +
  labs( x = 'Diferença Volume')+
  theme_bw()

# serie com d=1
p1 <- ggAcf(w, lag.max = 12*7) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "Gráfico ACF (1 diferença simples)") +
  theme_bw()
# componente de sazonalidade expressiva
p2 <- ggPacf(w, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "Gráfico PACF (1 diferença simples)") +
  theme_bw()
fig4 <- grid.arrange(p0,p1,p2)
# ggsave(filename = "fig4.png",plot = fig4, device = 'png', width = 5, height = 7, units="in", dpi = 300)

# sendo necessario assim tomar diferenças sazonais
nsdiffs(hid.ts2) # 1
# ws: serie com uma diferença simples e uma sazonal
ws <- diff(w, lag = 12) # lag 12 pois essa é uma serie mensal e seu ciclo sazonal é de 12 obs

p0 <- autoplot(ws) +
  labs(title = "Série do nível de montante (m) com\n1 diferença simples e 1 sazonal") +
  theme_bw()
# serie com d=1 e D=1
p1 <- ggAcf(ws, lag.max = 12*7) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "Gráfico ACF (1 diferença simples e 1 sazonal)") +
  theme_bw()
p2 <- ggPacf(ws, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "Gráfico PACF (1 diferença simples e 1 sazonal)") +
  theme_bw()
fig5 <- grid.arrange(p0,p1,p2)
## ggsave(filename = "fig5.png",plot = fig5, device = 'png', width = 5, height = 7, units="in", dpi = 300)

kpss.test(ws)

# p-value = 0.1 : nao rejeita hipotese de estacionariedade

# ambos decaem para 0, exceto por autocorrelação significativa no lag 12

# (4.3) - Sugestão de modelos ----

# SARIMA(0,1,0)x(1,1,1)12
# SARIMA(1,1,1)x(1,1,1)12

# SARIMA(1,1,0)x(1,1,1)
mod1 <- arima(hid.ts2, order = c(1,1,0), seasonal = c(1,1,1), include.mean = F)
# SARIMA(0,1,1)x(1,1,1)
mod2 <- arima(hid.ts2, order = c(0,1,1), seasonal = c(1,1,1), include.mean = F)
# SARIMA(1,1,1)x(1,1,1)
mod3 <- arima(hid.ts2, order = c(1,1,1), seasonal = c(1,1,1), include.mean = F)
# SARIMA(1,1,1)x(0,1,1)
mod4 <- arima(hid.ts2, order = c(1,1,1), seasonal = c(0,1,1), include.mean = F)
# SARIMA(1,1,1)x(1,1,0)
mod5 <- arima(hid.ts2, order = c(1,1,1), seasonal = c(1,1,0), include.mean = F)
# SARIMA(0,1,1)x(0,1,1)
mod6 <- arima(hid.ts2, order = c(0,1,1), seasonal = c(0,1,1), include.mean = F)


Modelo <- c("SARIMA(1,1,0)x(1,1,1)",
            "SARIMA(0,1,1)x(1,1,1)",
            'SARIMA(1,1,1)x(1,1,1)',
            "SARIMA(1,1,1)x(0,1,1)",
            "SARIMA(1,1,1)x(1,1,0)",
            'SARIMA(0,1,1)x(0,1,1)')
AIC <- c(AIC(mod1),AIC(mod2),AIC(mod3),AIC(mod4),AIC(mod5),AIC(mod6))
BIC <- c(BIC(mod1),BIC(mod2),BIC(mod2),BIC(mod2),BIC(mod2),BIC(mod2))
tabela <- data.frame(Modelo, AIC, BIC) %>% arrange(AIC,BIC)
tabela
#                Modelo      AIC      BIC
# 1 SARIMA(1,1,1)x(0,1,1) 388.7205 401.7920
# 2 SARIMA(0,1,1)x(0,1,1) 389.5408 401.7920
# 3 SARIMA(1,1,1)x(1,1,1) 389.7183 401.7920
# 4 SARIMA(1,1,0)x(1,1,1) 390.0589 401.5597
# 5 SARIMA(0,1,1)x(1,1,1) 390.2912 401.7920
# 6 SARIMA(1,1,1)x(1,1,0) 406.6561 401.7920

# SARIMA(0,1,1)x(0,1,1), modelo 6, é o modelo com menor AIC e BIC

mod_arima <- mod6
mod_arima
# Coefficients:
#          ma1     sma1
#       0.4826  -0.8120
# s.e.  0.0704   0.0947

# (5) - Analise de residuos ----
  
res1 <- residuals(mod_arima)  

# Estacionaridade
p1 <- autoplot(res1) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()
# Independência
p2 <- ggAcf(res1, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "") +
  theme_bw()
p3 <- ggPacf(res1, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "") +
  theme_bw()
# Normalidade
p4 <- data.frame(res1) %>%
  ggplot(aes(sample = res1)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_bw()
fig6 <- grid.arrange(p1,p4,p2,p3)
## ggsave(filename = "fig6.png",plot = fig6, device = 'png', width = 7, height = 5, units="in", dpi = 300)

# testes
kpss.test(res1)
Box.test(res1, lag = 12, type = "Ljung-Box", fitdf = 2) # fitdf = p+q
shapiro.test(res1)

# selecionando janela de dados
res2 <- window(res1, start=time(ws)[84]) 
# 144 obs/12 anos = 12 meses * 5 anos = 60, 144-60 = 84, isto é, peguei os ultimos 5 anos
  
  
# Estacionaridade
p1 <- autoplot(res2) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()
# Independência
p2 <- ggAcf(res2, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "") +
  theme_bw()
p3 <- ggPacf(res2, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "") +
  
  theme_bw()
# Normalidade
p4 <- data.frame(res2) %>%
  ggplot(aes(sample = res2)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_bw()
fig7 <- grid.arrange(p1,p4,p2,p3)
# ggsave(filename = "fig7.png",plot = fig7, device = 'png', width = 7, height = 5, units="in", dpi = 300)


# testes
kpss.test(res2)
Box.test(res2, lag = 12, type = "Ljung-Box", fitdf = 2)
shapiro.test(res2)

# (6) - Capacidade preditiva do modelo pontual e intervalar ----


f_arima <- function(y,h){
  fit = mod_arima
  forecast(fit,h)
}


# o resultado é quanto o modelo errou pra essas previsões
CV_arima = tsCV(hid.ts2, forecastfunction = f_arima, h=5)
cv_final <- CV_arima %>% round(2) %>% tail(15) 


EAM_arima = CV_arima %>% abs() %>% colMeans(na.rm = T) %>% round(2) %>% tibble()

fig8 <- plot.ts(EAM_arima, plot.type = 's', col=1, lwd=c(2,2), xlab = 'h passos', ylab='MEA')
# salvei manualmente

# # retorna a soma dos erros absolutos
# mea0 <- groe(hid.ts2, mod_arima, g='AE', n1= length(hid.ts2)-14, m=2, H=5)
# mea1 <- groe(hid.ts2, ses, g='AE', n1= length(hid.ts2)-14, m=2, H=5)
# mea2 <- groe(hid.ts2, holt, g='AE', n1= length(hid.ts2)-14, m=2, H=5)
# mea3 <- groe(hid.ts2, function(y,h) forecast(ets(y),h), g='AE', n1= length(hid.ts2)-14, m=2, H=5)
# mea4 <- groe(hid.ts2, function(y,h) forecast(auto.arima(y),h), g='AE', n1= length(hid.ts2)-14, m=2, H=5)
# mea <- data.frame(mea1,mea2,mea3,mea4)

# (7) - Previsao pontual e intervalar ----

## modelo aditivo
fita1 <- HoltWinters(hid.ts2, seasonal = 'additive')
fita1
plot(fita1, xlab='Ano', ylab='Nível de montante (m)')


resHW <- fita1$fitted[,1] - fita1$x
# Estacionaridade
p1 <- autoplot(resHW) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()
# Independência
p2 <- ggAcf(resHW, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "") +
  theme_bw()
p3 <- ggPacf(resHW, lag.max = 12*5) +
  scale_y_continuous(limits = c(-.6, .6)) +
  labs(title = "") +
  theme_bw()
# Normalidade
p4 <- data.frame(resHW) %>%
  ggplot(aes(sample = resHW)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_bw()
fig10 <- grid.arrange(p1,p4,p2,p3)
# ggsave(filename = "fig10.png",plot = fig10, device = 'png', width = 7, height = 5, units="in", dpi = 300)


# testes
kpss.test(resHW)
Box.test(resHW, lag = 12, type = "Ljung-Box", fitdf = 2) # fitdf = p+q
shapiro.test(resHW)

# ## modelo multiplicativo
# fita2 <- HoltWinters(hid.ts2, seasonal = 'multiplicative', beta = F)
# fita2
# plot(fita2)
# 
# resHW2 <- fita2$fitted[,1] - fita2$x
# # Estacionaridade
# p1 <- autoplot(resHW2) +
#   labs(x = "Ano", y = "Resíduos") +
#   theme_bw()
# # Independência
# p2 <- ggAcf(resHW2, lag.max = 12*5) +
#   scale_y_continuous(limits = c(-.6, .6)) +
#   labs(title = "") +
#   theme_bw()
# p3 <- ggPacf(resHW2, lag.max = 12*5) +
#   scale_y_continuous(limits = c(-.6, .6)) +
#   labs(title = "") +
#   theme_bw()
# # Normalidade
# p4 <- data.frame(resHW2) %>%
#   ggplot(aes(sample = resHW2)) +
#   stat_qq() +
#   stat_qq_line() +
#   labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
#   theme_bw()
# grid.arrange(p1,p4,p2,p3)
# # testes
# kpss.test(resHW)
# Box.test(resHW, lag = 12, type = "Ljung-Box", fitdf = 2) # fitdf = p+q
# shapiro.test(resHW)


#  Previsao HoltWinters
x <- cbind(hid.ts2, predict(fita1, n.ahead = 10), forecast(mod_arima, bootstrap = T, h = 10)[['mean']])
plot(x, xlab = 'Ano', ylab = 'Nível de montante (m)',
     plot.type = 'single', col = c(1,2,3), lwd =2 , lty=c(1,2,3))
legend( x = "bottomleft",
        legend = c("Holt-Winters", "SARIMA"),
        col = c("red","green"), lwd = 1,
        #lty = c(0,0),
        pch = c(17,19), 
        #bty= 'n'
        )
# salvei manualmente

# Previsao SARIMA
arima_preds <- forecast(mod_arima, h = 10, level = c(95))
figprev <- autoplot(hid.ts2) + 
  xlab("Ano") + 
  ylab("Nível de montante (m)") +
  theme_bw() +
  autolayer(arima_preds, series="SARIMA") +
  autolayer(hid.ts2, series="Série observada") +
  scale_colour_manual(values = c("SARIMA" = "#0000AA", "Série observada" = "black"),
                      breaks = c("SARIMA", "Série observada"), name = "")
# ggsave(filename = 'figprev.png',plot = figprev, device = 'png', width = 7, height =5)





