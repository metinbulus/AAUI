#veri setini okutalim
library(readxl)
#veri setini okuttuk
duzenleyici_veri_seti <- read_xlsx("D:/G_Drive/Akademik/Kitap_Yazimi/sakine_kitap/aracilik_kitap_bolumu_veri_setleri/moderation.xlsx")

olceklenmis_duzenleyici_veri_seti <- data.frame(pio = scale(veri_seti$pio, center = TRUE, scale = TRUE), 
           h_a = scale(veri_seti$h_a, center = TRUE, scale = TRUE), 
           yd = scale(veri_seti$yd, center = TRUE, scale = TRUE))

#regresyon modelini kuralim
fitMod <- lm(yd ~ pio + h_a + pio*h_a, data = olceklenmis_duzenleyici_veri_seti) 

#ozet istatistikler
summary(fitMod)

#etki buyuklugu
library(effectsize)
cohens_f_squared(fitMod)

#etkiyi gorsellestirelim
library(rockchalk)
ps  <- plotSlopes(fitMod, plotx="pio", modx="h_a", xlab = "Psikolojik Ýyi Oluþ", ylab = "Yaþam Doyumu", modxVals = "std.dev", interval = "confidence", legendPct = FALSE )

