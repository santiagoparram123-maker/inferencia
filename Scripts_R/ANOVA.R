
#ANOVA

library(readxl)
Metodos <- read_excel("C:/Users/Diana/Downloads/Metodos.xlsx")

modelo=aov(Metodos$Produccion~Metodos$Metodo)
modelo

ANOVA=anova(modelo)
ANOVA


#Comparaciones múltiples

library(agricolae)
library(car)

#prueba Tukey
IntervaloTuk=TukeyHSD(modelo,
                      conf.level = 0.95)
plot(IntervaloTuk)


boxplot(Metodos$Produccion~Metodos$Metodo,
        col=rainbow(3))



#LSD

interLSD=LSD.test(Metodos$Produccion,#variab cuanti
                  Metodos$Metodo,#variable cuali
                  DFerror = ANOVA$Df[2],#gl error
                  MSerror = ANOVA$`Mean Sq`[2],#cuadrados medios
                  console = TRUE)
plot(interLSD)


#Prueba de los supuestos

#normalidad

#prueba de shapiro

error=modelo$residuals

shapiro.test(error)


#supuesto de varianza constante
library(stats)
library(lmtest)
bptest(modelo)


#supuesto de independencia

library(lmtest)
dwtest(modelo)
