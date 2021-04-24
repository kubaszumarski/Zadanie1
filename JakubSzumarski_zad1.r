## Rozwiązania Zadania 1 - JAKUB SZUMARSKI pd1904 gr 13C BIGDATA
#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

sprawdz_podzielnossc<- function(l1,l2){
  w<-l1%%l2
  if (w==0) paste('liczba',l1,'jest podzielna przez',l2,sep=" ")
  else
    paste('liczba',l1,'nie jest podzielna przez',l2,sep=" ")
}
#------------------------------------------------------------------
#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.
srednia_v_pociagu<-function(v1,v2){
  #s-polowa drogi
  # v1=s/t1 -> t1=s/v1
  # v2=s/t2 -> t2=s/v2
  # v=2s/((s/v1)+(s/v2))=2s/((V2s/V1V2)+(V1s/V1V2))=2s/(s(v1+V2)/v1v2)=2s * (v1v2/s(v1+v2))=2*v1v2/(v1+v2)
  v=2*v1*v2/(v1+v2)
  paste('Średnia predkosc pociagu wynosi',v,sep=" ")
}
#srednia_v_pociagu(120,90)
#[1] "Średnia predkosc pociagu wynosi 102.857142857143"
 


#------------------------------------------------------------------
#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.
#https://www.naukowiec.org/wzory/statystyka/wspolczynnik-korelacji-r-pearsona_22.html

korelacja<- function(plik, kolumna1, kolumna2){
  dane<-read.csv2(plik)
  dane['iloczyn']<-dane[[kolumna1]]*dane[[kolumna2]]
  srednia_kol1<-mean(dane[[kolumna1]])
  srednia_kol2<-mean(dane[[kolumna2]])
  
  srednia_iloczyn<-mean(dane[['iloczyn']])
  iloczyn_sr_kol<-srednia_kol1*srednia_kol2
  kowariancja<-srednia_iloczyn-iloczyn_sr_kol
  odchylenie_std_kol1<-sd(dane[[kolumna1]])
  odchylenie_std_kol2<-sd(dane[[kolumna2]])
  iloczyn_odchylen=odchylenie_std_kol1*odchylenie_std_kol2
  wsp_korelacji=kowariancja/iloczyn_odchylen
  print(paste('Średnia wartosc w kolumnie_1:',srednia_kol1, sep=" "))
  print(paste('Średnia wartosc w kolumnie_2:',srednia_kol2, sep=" "))
  print(paste('Srednia wartość iloczynów:', srednia_iloczyn, sep=" "))
  print(paste('Iloczyn średnich kolumn:',iloczyn_sr_kol, sep=" "))
  print(paste('Kowariancja:',kowariancja, sep=" "))
  print(paste('Odchylenie standardowe kolumna_1:',odchylenie_std_kol1,sep=" "))
  print(paste('Odchylenie standardowe kolumna_2:',odchylenie_std_kol2,sep=" "))
  print(paste('Iloczyn odchyleń std obu kolumn:',iloczyn_odchylen, sep=" "))
  print(paste('Współczynnik korelacji:', wsp_korelacji, sep=" "))
}
# korelacja('dane.csv','waga','wzrost')
#[1] "Średnia wartosc w kolumnie_1: 74.669"
#[1] "Średnia wartosc w kolumnie_2: 169.586"
#[1] "Srednia wartość iloczynów: 12894.395"
#[1] "Iloczyn średnich kolumn: 12662.817034"
#[1] "Kowariancja: 231.577966000001"
#[1] "Odchylenie standardowe kolumna_1: 15.5928373163955"
#[1] "Odchylenie standardowe kolumna_2: 15.1799550406131"
#[1] "Iloczyn odchyleń std obu kolumn: 236.698569418478"
#[1] "Współczynnik korelacji: 0.978366563722555"
# Współczynnik korelacji bliski 1 lub -1 oznacza silną korelacje pomiędzy badanymi wartościami

####to samo za pomocą wbudowanej funkcji cor.test
####_____________________________________________

dane<-read.csv2('dane.csv')
result<-cor.test(dane[,'waga'],dane[,'wzrost'])
result
#Pearson's product-moment correlation

#data:  dane[, "waga"] and dane[, "wzrost"]
#t = 153.02, df = 998, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9766477 0.9817352
#sample estimates:
#      cor 
#0.9793459 

#Oznaczenia:
# t - wynik testu t_studenta
# df stopnie swobody
# p-value poziom istotności i jest on poniżej 0,001 czyli wynik jest istotny statystycznie
# conf.int wspoczynnik korelacji przy 95%
# sample estimates jest wspóczynnikiem korelacji  - w naszym przypadku wynosi 0,98 czyli jest to bardzo silna korelacja, tzn
# możemy stwierdzić że waga jest powiązana ze wzrostem czlowieka, korelacja jest dodatnia czyli że wysokim wartościom
# wagi odpowiadają wysokie wartości wzrostu


#------------------------------------------------------------------
#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość 
#wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, 
#że gdy użytkownik nie poda żadnej #wartości jako parametr, domyślna wartością będzie 1)

stworzDataFrame <- function(ile=1){
  kolumny_input<-readline(prompt='Podaj nazwy kolumn rodzielajac przecinkiem: ')
  kolumny<-unlist(strsplit(kolumny_input,","))
  kolumny
  df<-data.frame(matrix(ncol = length(kolumny),nrow = 0))
  i<-1
  while (i<=ile){
    text=paste('Wprowadz',length(kolumny),'warosci dla wiersza nr',i,'rodzielane przecinkiem:',sep=" ")
    wiersz_input<-readline(prompt=text)
    wiersz<-unlist(strsplit(wiersz_input,","))
    df<-rbind(df,wiersz)
    i<-i+1
  }
colnames(df)<-kolumny
df
}
#------------------------------------------------------------------
#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy:
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katalogu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny.

liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){
  nazwaKolumny<-paste("X",nazwaKolumny,sep="")
  pliki<-list.files(sciezka)
  df<-data.frame()
  for(i in 1:DlaIluPlikow){
    f_path<-file.path(sciezka,pliki[i])
    dane<-read.csv(f_path,sep=",",na.strings =c("","NA") )
    kolumna<-as.data.frame(dane[[nazwaKolumny]], drop=false)
    df<-rbind(df,kolumna)
  }
  nazwaKolumny<-substring(nazwaKolumny,2)
  colnames(df)<-c(nazwaKolumny)
  df<-na.omit(df)
  
  wynik<-switch (jakaFunkcja,
        'mean' = paste('Srednia arytmetyczna kolumny',nazwaKolumny,'wynosi:',mean(df[[nazwaKolumny]]),sep=" "),
        'median' = paste('Mediana kolumny',nazwaKolumny,'wynosi:',median(df[[nazwaKolumny]]),sep=" "),
        'max' = paste('Maksymalna wartość kolumny',nazwaKolumny,'wynosi:',max(df[[nazwaKolumny]]),sep=" "),
        'min' = paste('Minimalna wartość kolumny',nazwaKolumny,'wynosi:',min(df[[nazwaKolumny]]),sep=" ")
  )
  print(wynik)
 }
#folder<-'smogKrakow'
# > liczZplikow(folder,'3_humidity',,6)
#[1] "Srednia arytmetyczna kolumny 3_humidity wynosi: 74.7184466019417"
#> liczZplikow(folder,'3_humidity','median',6)
#[1] "Mediana kolumny 3_humidity wynosi: 77"
#> 
