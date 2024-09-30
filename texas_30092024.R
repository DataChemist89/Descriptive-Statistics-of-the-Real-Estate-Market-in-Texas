#import delle librerie e del dataset 
dir()
setwd(getwd())
data_texas_real_estate<- read.csv("realestate_texas.csv", sep=",")
library(gridExtra)
library(moments)
library(ineq)
library(dplyr)
library(ggplot2)

#indagare la struttura dati
class(data_texas_real_estate) #<- tipo di file
str(data_texas_real_estate) # <- struttura dataset
colSums(is.na(data_texas_real_estate)) <- #ricerca valori Na nel dataset
head(data_texas_real_estate,2) #<-print first 2 datasetrow
colSums(is.na(data_texas_real_estate)) #<- analisi della presenza dei valori nulli Na nel dataset, altrimenti sum(is.na(dataset))

#salviamo il primo elemento del vettore in una variabile n
n <- dim(data_texas_real_estate)[1]
attach(data_texas_real_estate)

#######################################################################################################################
#INDICI DI POSIZIONE
continue_var_summary <- summary(data_texas_real_estate[, c("sales", "volume", "median_price",
                                                           "listings", "months_inventory")])
continue_var_summar
#######################################################################################################################
#Distribuzione di Frequenza  Variabile City
city <- as.factor(city)
levels(city)
table(city)

#Costruire i livelli per la variabile Year
year <- factor(year, ordered = TRUE)
levels(year)
year_freq<-data.frame(table(year))

#Distribuzione di Frequenza Variabile month
month <- factor(month,
                levels = 1:12,
                labels = c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic"),
                ordered = TRUE)
table(month)

#Distribuzione di Frequenza Variabile sales 
Nbins<-(1+log2(n))
bins_amplitude_sales<-(((max(sales))-min(sales))/Nbins)
sales_classes=cut(sales,(seq((min(sales)-3),(max(sales)+3),bins_amplitude_sales)))
ni <- table(sales_classes)
fi <- ni/n
Ni <- cumsum(table(sales_classes))
Fi <- Ni/n
distr_sales <- as.data.frame(cbind(ni,fi,Ni,Fi))
distr_sales <- round(distr_sales,2)
distr_sales

#Distribuzione di Frequenza Variabile volume 
bins_amplitude_volume<-(((max(volume))-min(volume))/Nbins)
volume_classes=cut(volume,(seq((min(volume)-3),(max(volume)+3),bins_amplitude_volume)))
ni <- table(volume_classes)
fi <- ni/n
Ni <- cumsum(ni)
Fi <- Ni/n
distr_volume <- as.data.frame(cbind(ni,fi,Ni,Fi))
distr_volume <- round(distr_volume,2)
distr_volume

#######################################################################################################################
#visualizzazione frequenza con grafico a barre

sales_classes=cut(sales,(seq((min(sales)-3),(max(sales)+3),bins_amplitude)))
sales_classes
ggplot()+
  geom_bar(aes(x=sales_classes, fill=city),col="black")+
  labs(title="Distribuzione x classi di sales",x="Lunghezza in classi",y="frequenze assolute")+
  scale_y_continuous(breaks = seq(0,50,5))+
  theme_classic()

barplot(ni,
        main = "Distribuzione delle classi di frequenza nelle vendite",
        xlab = "Classi di vendite in numero",
        ylab = "Frequenze assolute",
        ylim = c(0,50),
        col = "blue",
        names.arg = rownames(sales_classes))

#######################################################################################################################
#Indice di variabilità e forma (codice inserito nella consegna della certificazione):
sales_range<-max(sales)-min(sales)
volume_range<-max(volume)-min(volume)
median_price_range<-max(median_price)-min(median_price)
listings_range<-max(listings)-min(listings)
months_inventory_range<-max(months_inventory)-min(months_inventory)
CV <- function(x)
{return(sd(x)/mean(x)*100)}
variability_shape_dataframe<- data.frame(
  Variabile=c("sales", "volume", "median_price", "listings", "months_inventory"),
  Range=c(sales_range, volume_range,median_price_range,listings_range,months_inventory_range),
  media=c(mean(sales), mean(volume),mean(median_price),mean(listings),mean(months_inventory)),
  SD=c(sd(sales), sd(volume),sd(median_price),sd(listings),sd(months_inventory)),
  CV=c(CV(sales), CV(volume),CV(median_price),CV(listings),CV(months_inventory)),
  IQR=c(IQR(sales), IQR(volume),IQR(median_price),IQR(listings),IQR(months_inventory)),
  skewness=c(skewness(sales), skewness(volume),skewness(median_price),skewness(listings),skewness(months_inventory)),
  kurtosis=c((kurtosis(sales)-3), (kurtosis(volume)-3),(kurtosis(median_price)-3),(kurtosis(listings)-3),(kurtosis(months_inventory)-3)))
variability_shape_dataframe<- variability_shape_dataframe %>%
  mutate_if(is.numeric, round, digits = 2)
variability_shape_dataframe
#######################################################################################################################
#Studio grafico delle forma e della variabilità
attach(variability_shape_dataframe)
#KDE sales
ggplot(data_texas_real_estate) +
  geom_density(aes(x = sales), col = "black", fill = "lightblue") +
  labs(y="FREQUENZE i-esime", x="VARIABILE SALES")+
  labs(title = "KDE - Variabile sales")
#KDE volume
ggplot(data_texas_real_estate) +
  geom_density(aes(x = volume), col = "black", fill = "lightblue") +
  labs(y="FREQUENZE i-esime", x="VARIABILE VOLUME")+
  labs(title = "KDE - Variabile volume")
#KDE median_price
ggplot(data_texas_real_estate) +
  geom_density(aes(x = median_price), col = "black", fill = "lightblue") +
  labs(y="FREQUENZE i-esime", x="VARIABILE MEDIAN PRICE")+
  labs(title = "KDE - Variabile median price")
#KDE listings
ggplot(data_texas_real_estate) +
  geom_density(aes(x = listings), col = "black", fill = "lightblue") +
  labs(y="FREQUENZE i-esime", x="VARIABILE LISTINGS")+
  labs(title = "KDE - Variabile listings")
#KDE months_invntory
ggplot(data_texas_real_estate) +
  geom_density(aes(x = months_inventory), col = "black", fill = "lightblue") +
  labs(y="FREQUENZE i-esime", x="VARIABILE MONTHS INVENTORY")+
  labs(title = "KDE - Variabile months inventory")

# Creo i boxplot separati per ogni variabile
library(ggplot2)
city_box<- ggplot(data_texas_real_estate, aes(y = city)) + geom_boxplot() + labs(title = "city")
year_box<- ggplot(data_texas_real_estate, aes(y = year)) + geom_boxplot() + labs(title = "Year")
month_box<- ggplot(data_texas_real_estate, aes(y = month)) + geom_boxplot() + labs(title = "month")
sales_box<- ggplot(data_texas_real_estate, aes(y = sales)) + geom_boxplot() + labs(title = "Sales")
volume_box<- ggplot(data_texas_real_estate, aes(y = volume)) + geom_boxplot() + labs(title = "Volume")
medianprice_box<- ggplot(data_texas_real_estate, aes(y = median_price)) + geom_boxplot() + labs(title = "median_price")
listings_box<- ggplot(data_texas_real_estate, aes(y = listings)) + geom_boxplot() + labs(title = "listings")
months_inv_box<- ggplot(data_texas_real_estate, aes(y = months_inventory)) + geom_boxplot() + labs(title = "months_invetory")
#grid.arrange per visualizzarli insieme
library(gridExtra)
grid.arrange(city_box,year_box,
             month_box,
             sales_box,
             volume_box,
             medianprice_box,
             listings_box,
             months_inv_box,
             ncol = 3)
#indentificare outliers in Volume:
Q1 <- quantile(volume, 0.25)
Q3 <- quantile(volume, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- data_texas_real_estate%>%
  filter(volume < lower_bound | volume > upper_bound)
outliers

#######################################################################################################################
#QUAL E LA VARIABILE CON LA VARIABILITA' PIU ELEVATA? E QUALE QUELLA PIU ASIMMETRICA?
ggplot(data=variability_shape_dataframe)+
  geom_col(aes(x=Variabile,y=CV),fill="blue", col="black")+
  scale_y_continuous(breaks =seq(0,60,5))+
  labs(x="", y="Coefficiente di variazione CV(%)")

ggplot(data=variability_shape_dataframe)+
  geom_col(aes(x=Variabile,y=skewness),fill="green", col="black")+
  scale_y_continuous(breaks =seq(-1,1,0.1))+
  labs(x="", y="skewness")

#######################################################################################################################
#STUDIO APPROFONDITO DELLA VARIABILE QUANTITATIVA SALES
#suddivisione in classi
Nbins<-round((1+log2(n)))
Nbins
bins_amp<-((((max(sales))-min(sales))/Nbins))
bins_amp
classes=cut(sales,(seq((min(sales)-3),(max(sales)+bins_amp),bins_amp)))
#distribuzione di frequenza
ni <- table(classes)
fi <- ni/n
Ni <- cumsum(ni)
Fi <- cumsum(ni)/n
distr_freq<- as.data.frame(cbind(ni,fi,Ni,Fi))
round(distr_freq,2)
#grafico a barre
library(ggplot2)
ggplot()+
  geom_bar(aes(x=classes),col="black", fill="blue")+
  labs(title="Distribuzione di frequenza delle classi di sales",
       x="Lunghezza in classi",
       y="frequenze assolute")+
  scale_y_continuous(breaks = seq(0,50,5))+
  theme_classic()

#calcolo indice di eterogeneità di Gini sulla variabile sales:
attach(distr_freq)
gini.index <- function(x){
  ni=table(x)
  fi=ni/length(x) 
  fi2=fi^2 
  J=length(table(x))
  gini= 1-sum(fi2)
  gini.normalizzato=gini/((J-1)/J)
  return(gini.normalizzato)
}
cat("L'indice di gini per la distribuzione di freqenza in classi di sales è pari a: ", round(gini.index(classes),2))

#######################################################################################################################
#PROBABILITA city BEAUMONT
p_beaumont <- sum(city=="Beaumont")/length(city)
cat("La probabilità che la riga del dataset riporti Beaumont è pari al:", p_beaumont*100, "%")
#PROBABILITA mese di Luglio
month <- factor(month,
                levels = 1:12,
                labels = c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic"),
                ordered = TRUE)

p_july <- sum(month=="Lug")/length(month)
cat("La probabilità che la riga del dataset riporti un mese di Luglio è pari al:", round(p_july*100,2), "%")


p_year_2012<- sum(year==2012)/length(year)
cat("La probabilità che la riga del dataset riporti l'anno 2012:", round(p_year_2012*100,2), "%")

p_December = p_july
p_december_2012 <- p_December * p_year_2012
cat("La probabilità che la riga del dataset riporti il mese di Dicembre 2012 è del:", round(p_december_2012*100,2), "%")
#######################################################################################################################
#8.	Esiste una colonna col prezzo mediano, creane una che indica invece il prezzo medio, 
#utilizzando le altre variabili che hai a disposizione
mean_price_USdollars<-(volume/sales)*1000000
prezzo_medio_kdollari <- data.frame(mean_price_USdollars)
data_texas_real_estate<-cbind(data_texas_real_estate,mean_price_USdollars) 
data_texas_real_estate
#Creare colonna efficacia annunci di vendita
listings_efficiency <- round((sales/listings),2)
data_texas_real_estate <-cbind(data_texas_real_estate, listings_efficiency)
df_efficiency <- data.frame(cbind(city,year,sales,listings,listings_efficiency))
df_efficiency
attach(prova)
range(listings_efficiency)
boxplot(listings_efficiency, xlab="listings_efficiency")
boxplot(listings_efficiency~year)
boxplot(listings_efficiency~city)

#######################################################################################################################
#ESERCIZIO NUMERO 9
library(dplyr)
by_Year_City<-data_texas_real_estate %>%
  group_by(city,year)%>%
  reframe(median_price=mean(median_price),
          media=mean(sales),
            dev.st=sd(sales),
            listings_efficiency=mean(listings_efficiency),
            months_inventory=mean(months_inventory))
by_Year_City

library(dplyr)
by_month<-data_texas_real_estate %>%
  group_by(month)%>%
  reframe(median_price=mean(median_price),
          vendite=mean(sales),
          listings_efficiency=mean(listings_efficiency),
          months_inventory=mean(months_inventory))

by_month

ggplot(data_texas_real_estate, aes(x = city, y = median_price, fill = as.factor(year))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Prezzi mediani delle case in $ ripartiti per città in funzione degli anni", x = "CITTA' DEL TEXAS", y = "PREZZI MEDIANI IN $")


ggplot(data_texas_real_estate, aes(x = city, y = sales, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "n. DI VENDITE RIPARTITE PER CITTA E PER ANNO", x = "CITTA' DEL TEXAS", y = "N. DI VENDITE")+
  theme(axis.text.x = element_text(size=14, angle = 45, hjust = 1), title = element_text(hjust =0.5))+
  theme(plot.title= element_text(hjust =0.5))


ggplot(data_texas_real_estate, aes(x = city, y = 1/listings_efficiency, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "EFFICIENZA DELLA CAMPAGNE PUBBLICITARIA NEGLI ANNI NELLE CITTA DEL TEXAS", x = "CITTA' DEL TEXAS", y = "N. ANNUNCI PER SINGOLA VENDITA")+
  theme(axis.text.x = element_text(size=14, angle = 45, hjust = 1), title = element_text(hjust =0.5))+
  theme(plot.title= element_text(hjust =0.5))

#boxplot ESERCIZIO n9
by_month
#boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città.
ggplot(data_texas_real_estate, aes(x = city, y = median_price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Distribuzione del Prezzo Mediano tra le Città", x = "Città", y = "Prezzo Mediano")
# Boxplot per il volume delle vendite per città e anno
ggplot(data_texas_real_estate, aes(x = city, y = volume, fill = as.factor(year))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribuzione del Volume delle Vendite tra Città e Anni", x = "Città", y = "Volume delle Vendite")
# Grafico a barre sovrapposte per le vendite nei mesi
ggplot(data_texas_real_estate, aes(x = month, y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Totale delle Vendite nei Vari Mesi per Città", x = "Mese", y = "Vendite Totali")
# Grafico a barre sovrapposte per le vendite nei mesi normalizzato 
ggplot(data_texas_real_estate, aes(x = month, y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Totale delle Vendite nei Vari Mesi per Città normalizzato", x = "Mese", y = "Vendite Totali")

#######################################################################################################################
#SERIE STORICHE
# Caricamento del dataset
# Unione di anno e mese per creare una colonna "date"
data_texas_real_estate$date <- as.Date(with(data_texas_real_estate, paste(year, month, "01", sep="-")), "%Y-%m-%d")
# Filtrare per le città di interesse
city_filtered_data <- data %>%
  filter(city %in% c("Beaumont", "Bryan-College Station", "Tyler", "Wichita Falls"))
# Raggruppamento per data e città, e somma delle vendite
library(dplyr)
time_series_cities <- city_filtered_data %>%
  group_by(date, city) %>%
  summarise(total_sales = sum(sales))
time_series_cities
# Creazione del grafico a linee per il confronto tra le città
library(ggplot2)
ggplot(time_series_cities, aes(x = date, y = total_sales, color = city)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Confronto del numero di vendite nel fra le città nel corso del tempo",
       x = "Data",
       y = "Numero di Vendite") +
  theme_minimal() +
  theme(legend.position = "top")  # Sposta la legenda in alto

#FINE 































































##############################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################
#CODICE NON INCLUSO  NELLA RICONSEGNA DEL PROGETTO 26/9/2024
boxplot(sales~month)

sales_grouped_by<-data_texas_real_estate%>%
  group_by(year)%>%
  summarise(vendite=table(sales_classes))

distr_sales
data_texas_real_estate$sales %>%
  group_by(city)

#INDICI DI VARIABILITA'
install.packages("moments")
library(moments)
lista <- list(sales = sales, 
              volume = volume, 
              median_price = median_price, 
              listings = listings, 
              months_inventory = months_inventory)

varianza <- sapply(lista, var)
list_var_df <- as.data.frame((varianza)) 
list_var_df
dev.standard <- sapply(lista, sd)
list_sd <- as.data.frame((dev.standard)) 
list_sd
skewness <- sapply(lista, skewness)
list_var_skew <- as.data.frame(skewness)
list_var_skew
kurtosis <- sapply(lista, (kurtosis))
list_var_kurtosis <- as.data.frame(kurtosis) 
list_var_kurtosis <-list_var_kurtosis-3
list_var_kurtosis
t1<-t(list_var_df)
t2<-t(list_sd)
t3<-t(list_var_skew)
t4<-t(list_var_kurtosis)
var_skw_kur_df <- as.data.frame(rbind(varianza=t1,Dev.STD=t2,Skewness=t3,Kurtosis=t4))
var_skw_kur_df 
#arrotondo alla 2°cifra decimale
var_skw_kur_df  <- var_skw_kur_df  %>%
  mutate_if(is.numeric, round, digits = 2)
var_skw_kur_df 


#####################################################################################################################################################################################################################################################################################################################################################################
#ESEMPIO DI curtosi negativa(listings) 
ggplot(data_texas_real_estate) +
  geom_density(aes(x = listings), col = "black", fill = "lightblue") +
  labs(y="frequenze i-esime")

variability_shape_dataframe 
boxplot(sales~month)

#####################################################################################################################################################################################################################################################################################################################################################################
# Creazione del boxplot con le linee per gli outlier
p1 <- ggplot(data_texas_real_estate, aes(y = volume)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 18) +  # Boxplot con outlier in rosso
  geom_hline(yintercept = lower_bound, linetype = "dashed", color = "blue") +  # Linea per outlier inferiore
  geom_hline(yintercept = upper_bound, linetype = "dashed", color = "blue") +  # Linea per outlier superiore
  labs(title = "Boxplot di Sales con linee per gli Outlier")
#####################################################################################################################################################################################################################################################################################################################################################################
#asimmetria delle variabili
#INDICI DI FORMA(calcolo del momento 3° e 4° della distribuzione)

#esempio di skweness positiva (sale dove media>mediana>moda)
library(ggplot2)
media <- mean(sales)
mediana <- median(sales)
dens <- density(sales)
moda <- dens$x[which.max(dens$y)]

ggplot(data_texas_real_estate) +
  geom_density(aes(x = sales), col = "black", fill = "lightblue") +
  
  geom_vline(aes(xintercept = media), col = "blue", linetype = "dashed") +
  geom_text(aes(x = media, y = 0.0020, label = paste("Media =", round(media, 2))), color = "blue", vjust = -1) +
  
  geom_vline(aes(xintercept = mediana), col = "black", linetype = "dashed") +
  geom_text(aes(x = mediana, y = 0.0025, label = paste("Mediana =", round(mediana, 2))), color = "black", vjust = -1) +
  
  geom_vline(aes(xintercept = moda ), col = "red", linetype = "dashed") +
  geom_text(aes(x = moda, y = 0.003, label = paste("Moda =", round(moda, 2))), color = "red", vjust = -1)+
  labs(y="frequenze i-esime")

#######################################################################################################################
#esempio di skweness negativa (median_price dove media<mediana<moda)
media <- mean(median_price)
mediana <- median(median_price)
dens <- density(median_price)
moda <- dens$x[which.max(dens$y)]

ggplot(data_texas_real_estate) +
  geom_density(aes(x = median_price), col = "black", fill = "lightblue") +
  
  geom_vline(aes(xintercept = media), col = "blue", linetype = "dashed") +
  geom_text(aes(x = media, y = 0.0000020, label = paste("Media =", round(media, 2))), color = "blue", vjust = -1) +
  
  geom_vline(aes(xintercept = mediana), col = "black", linetype = "dashed") +
  geom_text(aes(x = mediana, y = 0.0000025, label = paste("Mediana =", round(mediana, 2))), color = "black", vjust = -1) +
  
  geom_vline(aes(xintercept = moda ), col = "red", linetype = "dashed") +
  geom_text(aes(x = moda, y = 0.000003, label = paste("Moda =", round(moda, 2))), color = "red", vjust = -1)+
  labs(y="frequenze i-esime")
#######################################################################################################################
#ESEMPIO DI curtosi negativa(listings) 
ggplot(data_texas_real_estate) +
  geom_density(aes(x = listings), col = "black", fill = "lightblue") +
  labs(y="frequenze i-esime")
#######################################################################################################
prob.city <-sample(city,10000,replace=T)
table(prob.city)
#probabilità assoluta su BEAUMONT
ggplot()+
  geom_histogram(aes(x=prob.city),
                 stat="count",
                 col="black",
                 fill="lightblue")+
  scale_y_continuous(breaks = seq(0,10000,500))+
  labs(x="city", y= "frequenze assolute", title="Probabilita' assoluta per città")
#probabilità relativa su BEAUMONT
ggplot()+
  geom_histogram(aes(x=prob.city, y=stat((count/sum(count)))*100),
                 stat="count",
                 col="black",
                 fill="lightblue")+
  scale_y_continuous(breaks = seq(0,100,5))+
  labs(x="city", y= "frequenze relative %", title="Probabilita' relativa per città")

#probabilità relativa su LUGLIO (mese n.7)
prob.july <-sample(month,50000,replace=T)
table(month)
ggplot()+
  geom_histogram(aes(x=prob.july, y=stat((count/sum(count)))*100),
                 stat="count",
                 col="black",
                 fill="lightblue")+
  scale_y_continuous(breaks = seq(0,10,0.5))+
  scale_x_continuous(breaks = seq(0,12,1))+
  labs(x="mese", y= "frequenze relative %", title="Probabilita' relativa per mese del dataset")
probabilità_mese=0.083
#probabilità relativa su DICEMBRE 2012
prob.year <-sample(year,500000,replace=T) 
ggplot()+
  geom_histogram(aes(x=prob.year, y=stat((count/sum(count)))*100),
                 stat="count",
                 col="black",
                 fill="lightblue")+
  scale_y_continuous(breaks = seq(0,100,5))+
  labs(x="anno", y= "frequenze relative %", title="Probabilita' relativa per anno")
probabilità_anno=0.2

#Probabilità di trovare il mese DICEMBRE 2012 fra le righe del dataset
probabilità_dic_2012=probabilità_anno*probabilità_mese
probabilità_dic_2012 #p12_2012=0.166



