###############################################################################
#1.Yol Analizi                                                                #
###############################################################################

#veri setini okutmak icin gerekli olan paketi cagiralim. Yuklu degil ise 
#install.packages("readxl")
#kodu ile yukleme yapabilirsiniz.
library(readxl)
#veri setini okuttuk
araci_veri_seti <- read_xlsx("D:/G_Drive/Akademik/Kitap_Yazimi/sakine_kitap/aracilik_kitap_bolumu_veri_setleri/mediation.xlsx")
#yol analizi icin toplam puanlari tanimladik
araci_veri_seti$pio <- rowSums(veri_seti[,1:15])
araci_veri_seti$sd <- rowSums(veri_seti[,16:23])
araci_veri_seti$yd <- rowSums(veri_seti[,24:40])

#yol analizi yapabilmek icin lavaan paketini cagirdik. Yuklu degil ise 
#install.packages("lavaan")
#kodu ile yukleme yapabilirsiniz.
library(lavaan)

#inceleyecegimiz modeli tanimladik
model <- ' # direkt etki
             yd ~ c_ussu*pio
           # araci degiskeni modelleyelim
             sd ~ a*pio
             yd ~ b*sd
           # dolaylı etki (a*b)
             ab := a*b
           # toplam etki
             c := c_ussu + (a*b)
         '

#modelden kestirim yapabilmek icin sem fonksiyonunu kullaniyoruz
fit <- sem(model, data = araci_veri_seti, se = "bootstrap", bootstrap = 5000)

#uyum istatistikleri icin fitMeasures fonksiyonunu kullaniyoruz
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"),
            output = "matrix")

#kestirim sonuclarini incelemek icin summary fonksiyonunu kullaniyoruz
summary(fit,  standardized = TRUE, rsquare = TRUE, ci = TRUE)



###############################################################################
#2.YEM Modeli                                                                 #
###############################################################################
library(readxl)
#veri setini okuttuk
araci_veri_seti<- read_xlsx("C:/Users/Faruk/Desktop/aracilik_kitap_bolumu_veri_setleri/mediation.xlsx")

#YEM analizi yapabilmek icin lavaan paketini cagirdik. Yuklu degil ise 
#install.packages("lavaan")
#kodu ile yukleme yapabilirsiniz.
library(lavaan)

#inceleyecegimiz modeli tanimladik
model <- 'pio =~ P1 + P2 + P3 + P4 + P5 + 
                                 P6 + P7 + P8 + P9 + P10 + 
                                 P11 + P12 + P13 + P14 + P15
                   
           sd =~ S1 + S2 + S3 + S4 + S5 + 
                            S6 + S7 + S8
          
           yd =~ Y1 + Y2 + Y3 + Y5 + 
                          Y6 + Y7 + Y8 + Y9 + Y10 +
                          Y11 + Y12 + Y13 + Y14 + Y15 + 
                          Y16 + Y17

           # direkt etki
             yd ~ c_ussu*pio
           # araci degiskeni modelleyelim
             sd ~ a*pio
             yd ~ b*sd
           # dolaylı etki (a*b)
             ab := a*b
           # toplam etki
             c := c_ussu + (a*b)
         '

fit <- sem(model, data = araci_veri_seti, ordered = T, estimator = "DWLS", se = "bootstrap", bootstrap = 5)

#uyum istatistikleri icin fitMeasures fonksiyonunu kullaniyoruz
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"),
            output = "matrix")


#veri setini surekli olarak kabul edip en cok olabilirlik yontemiyle de kestirim yapilabilir.
fit <- sem(model, data = araci_veri_seti, estimator = "ML", se = "bootstrap", bootstrap = 5)

#kestirim sonuclarini incelemek icin summary fonksiyonunu kullaniyoruz
summary(fit,  standardized = TRUE, rsquare = T, ci = T)


semPlot::semPaths(fit, intercept = FALSE, whatLabel = "std",
                  residuals = FALSE, exoCov = FALSE, edge.label.cex = 1, title.cex = 2)



















