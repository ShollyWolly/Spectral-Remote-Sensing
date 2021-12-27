#Seminar Final
#Marketing in bezug auf Wasser
#Fl??sse: Schiffsfahrten
#Meere: Badeartikel
#Seen: Badeartikel/Anglerartikel

#Einlesen der Pakete
library(raster)
library(rgdal)
library(ggplot2)
library(magick)
install.packages("magick")

#Setzen des Pfades
setwd("./Bilder/rs")


#Einlesen der Bands (Nordsee)
Blue = raster("Project/Bands/Blue/Nordsee-Blue.tiff")

Red = raster("Project/Bands/Red/Nordsee-Red.tiff")

Green = raster("Project/Bands/Green/Nordsee-Green.tiff")

NIR = raster("Project/Bands/NIR/Nordsee-NIR.tiff")

SWIR1 = raster("Project/Bands/SWIR 1375nm/Nordsee-SWIR.tiff")

SWIR2 = raster("Project/Bands/SWIR 1610nm/Nordsee-SWIR.tiff")

SWIR3 = raster("Project/Bands/SWIR 2190nm/Nordsee-SWIR.tiff")

#Einlesen der Bands (Losheim)
Blue = raster("Project/Bands/Blue/Losheim-Blue.tiff")

Red = raster("Project/Bands/Red/Losheim-Red.tiff")

Green = raster("Project/Bands/Green/Losheim-Green.tiff")

NIR = raster("Project/Bands/NIR/Losheim-NIR.tiff")

SWIR1 = raster("Project/Bands/SWIR 1375nm/Losheim-SWIR.tiff")

SWIR2 = raster("Project/Bands/SWIR 1610nm/Losheim-SWIR.tiff")

SWIR3 = raster("Project/Bands/SWIR 2190nm/Losheim-SWIR.tiff")

#Plotten von Blue, Green, Red und NIR
par(mfrow = c(2,2))

plot(Blue, main = "Blue", col = gray(0:100 / 100))
plot(Green, main = "Green", col = gray(0:100 / 100))
plot(Red, main = "Red", col = gray(0:100 / 100))
plot(NIR, main = "NIR", col = gray(0:100 / 100))  

par(mfrow=c(1,1))

#Stacking
Layerstack = stack(Blue,Red,Green,NIR,SWIR1,SWIR2,SWIR3)

names(Layerstack) = c("Blue","Red","Green","NIR","SWIR1","SWIR2","SWIR3")

nlayers(Layerstack)

landsatRGB = Layerstack[[c(2,3,1)]]
landsatFCC = Layerstack[[c(4,2,3)]]

#Plotten von True Colour und False Colour

plotRGB(landsatRGB, axes = FALSE, stretch = "lin", main = "Nordsee")

plotRGB(landsatFCC, axes=FALSE, stretch="lin", main="Landsat False Color Composite")

#NDWI (Normalized Difference Water Index)
ndwi = (Layerstack[['Green']] - Layerstack[['NIR']]) / (Layerstack[['Green']] + Layerstack[['NIR']])
plot(ndwi, col = rev(terrain.colors(10)), main = "Landsat-NDWI")

ndwi2 = (Layerstack[['NIR']] - Layerstack[['SWIR2']]) / (Layerstack[['NIR']] + Layerstack[['SWIR2']])
plot(ndwi2, col = rev(terrain.colors(6)), main = "Landsat-NDWI2")

#histogramm
hist(ndwi,
     main = "Distribution of NDWI values",
     xlab = "NDWI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-0.8, 1),
     breaks = 30,
     xaxt = 'n')

axis(side=1, at = seq(-0.8,1, 0.05), labels = seq(-0.8,1, 0.05))


#Thresholding
Water <- reclassify(ndwi, cbind(-Inf, 0, NA))
plot(Water, main='Water')

Water2 <- reclassify(ndwi2, cbind(-Inf, 0.3, NA))
plot(Water2, main='Water2')

#??ber den Colorplot Plotten
plotRGB(landsatRGB, axes=TRUE, stretch="lin", main="Landsat False Color Composite")
plot(Water, col = c("Green"), add=TRUE, legend=FALSE)

#Klassifikation (FCC)

set.seed(123)
twcss <- sapply(1:10, function(k){kmeans(landsatFCC[], k)$tot.withinss})
qplot(x = 1:10, y = twcss, geom = 'line', main = "Clusterzahlwahl",ylab= "WCSS")

set.seed(123)
kMeansResult <- kmeans(landsatFCC[], centers=5)
result <- raster(landsatRGB[[1]])
result <- setValues(result, kMeansResult$cluster)

#plot(result, col= c("orange","Blue","Darkgreen"), main = "K-Means Klassifikation (FCP)",axes=FALSE)
plot(result, col= c("Green","Darkgreen","#808000","Blue","Orange"), main = "K-Means Klassifikation (FCP)")


#get coordinates
set.seed(123)
df = data.frame(kMeansResult$cluster,coordinates(result))

df$kMeansResult.cluster[df$kMeansResult.cluster!=4]=NA

na.omit(df)

spg <- df
coordinates(spg) <- ~ x + y

gridded(spg) <- TRUE

rasterDF <- raster(spg)

plot(rasterDF, col=c("Blue"))

#Zusammef??gen, unterschiede erkennen
plotRGB(landsatRGB, axes=FALSE, stretch="lin", main="Unterschiede")
plot(rasterDF, col=c("Blue"), add=TRUE, legend=FALSE) #Kmeans
plot(Water, col = c("Green"), add=TRUE, legend=FALSE) #Bands


kMeansResult$ifault
#Fazit, bei ger

Test = image_read("./Bilder/Wasser Losheim Klassifikation.tiff")

plot(Test)
plot(Water, col = c("Green"), add=TRUE, legend=FALSE) #Bands


