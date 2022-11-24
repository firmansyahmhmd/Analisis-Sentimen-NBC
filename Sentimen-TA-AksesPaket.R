#1. Listing Program Pemasangan dan Akses Paket Modul RStudio

#UNDUH ATAU PASANG PAKET MODUL ==== 
install.packages("tm")  #Dasar menjalankan text mining
install.packages("textclean") #Melakukan normalisasi
install.packages("stringr") #Melakukan pelabelan kata
install.packages("plyr")  #Melakukan pelabelan kata
install.packages("tokenizers")  #Melakukan tokenisasi
install.packages("e1071") #Menghitung model probabilitas naive bayes data latih
install.packages("gmodels") #Menampilkan jumlah label yang diolah
install.packages("caret") #Menjalankan confusion matriks atau akurasi data
install.packages("wordscloud")  #Visualisasi data kumpulan kata acak
install.packages("devtools")  #Mengambil kamus data dari situs github
install_github("nurandi/katadasaR") 
#Mengambil kamus stemming katadasaR pada akun github nurandi 


#AKSES PAKET MODUL ====
library(tm) 
library(textclean) 
library(stringr) 
library(plyr) 
library(tokenizers) 
library(e1071) 
library(gmodels) 
library(caret) 
library(wordcloud) 
library(devtools) 
library(katadasaR)













