library(readxl)
library(tseries)
library(vars)
library(lmtest)
library(ggplot2)

dane_dzienne <- read_xlsx("dane_dzienne.xlsx")


# Analiza danych

## Statystyki opisowe
summary(dane_dzienne["dji"])
summary(dane_dzienne["usdpln"])
summary(dane_dzienne["wig20"])

## Wykresy
ggplot(dane_dzienne, aes(x = data, y = dji)) +
  geom_line() +
  labs(x = "Data", y = "Wartosc") +
  ggtitle("Wykres liniowy na szeregu czasowym")

ggplot(dane_dzienne, aes(x = data, y = usdpln)) +
  geom_line() +
  labs(x = "Data", y = "Wartosc") +
  ggtitle("Wykres liniowy na szeregu czasowym")

ggplot(dane_dzienne, aes(x = data, y = wig20)) +
  geom_line() +
  labs(x = "Data", y = "Wartosc") +
  ggtitle("Wykres liniowy na szeregu czasowym")




dji <- ts(dane_dzienne["dji"])
usdpln <- ts(dane_dzienne["usdpln"])
wig20 <- ts(dane_dzienne["wig20"])

#test adf ()
adf.test(dji)
adf.test(usdpln)
adf.test(wig20)

ln_dane_dzienne <- read_xlsx("ln_dane_dzienne.xlsx")

ln_dji <- ts(ln_dane_dzienne["ln_dji"])
ln_usdpln <- ts(ln_dane_dzienne["ln_usdpln"])
ln_wig20 <- ts(ln_dane_dzienne["ln_wig20"])

adf.test(ln_dji)
adf.test(ln_usdpln)
adf.test(ln_wig20)

VARselect(ln_dane_dzienne, lag.max = 10, type="both")

var_model <- VAR(ln_dane_dzienne, p=1)
summary(var_model)

#normalnosc
normality.test(var_model)

#autokorelacja
serial.test(var_model)

#heteroscedasticity
arch.test(var_model)

predict(var_model, n.ahead=4, ci=0.95)
var_model.pred <- predict(var_model, n.ahead=4, ci=0.95)
#fanchart(var_model.pred)

irf(var_model, impulse="ln_dji", n.ahead=4, response=c("ln_usdpln", "ln_wig20"), boot=FALSE)
irf1<-irf(var_model, impulse="ln_dji", n.ahead=4, response=c("ln_usdpln", "ln_wig20"), boot=FALSE)
plot(irf1)

# irf(var_model, impulse="ln_usdpln", n.ahead=4, response=c("ln_dji", "ln_wig20"), boot=FALSE)
# irf2<-irf(var_model, impulse="ln_usdpln", n.ahead=4, response=c("ln_dji", "ln_wig20"), boot=FALSE)
# plot(irf2)
# irf(var_model, impulse="ln_usdpln", n.ahead=4, response=c("ln_dji", "ln_wig20"), boot=FALSE)
# irf3<-irf(var_model, impulse="ln_wig20", n.ahead=4, response=c("ln_dji", "ln_usdpln"), boot=FALSE)
# plot(irf3)

#H1
causality(var_model,cause=c("ln_wig20","ln_usdpln"))

#H2
causality(var_model,cause=c("ln_dji","ln_usdpln"))

#H3
causality(var_model,cause=c("ln_dji","ln_wig20"))


