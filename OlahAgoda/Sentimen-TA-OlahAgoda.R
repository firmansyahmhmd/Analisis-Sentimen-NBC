#2. Listing Program Menggunakan Data Tanggapan Aplikasi Agoda

#Atur penyimpanan lokal selama program berlangsung
setwd("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahAgoda") 
#Import data tanggapan aplikasi agoda 
data.agoda <- read.csv("agoda_rev.csv", stringsAsFactors = FALSE, 
                       encoding = 'UTF-16')
#Ubah nama kolom 
names(data.agoda) <- c("nama", "peringkat", "tanggapan", "tanggal")
#Hapus kolom peringkat 
data1.agoda <- data.agoda[, -c(2)]
#Eksport data csv kolom tanggapan 
write.csv(data1.agoda$tanggapan, "data1agoda_tanggapan.csv")
#Import data tanggapan menggunakan readLines 
agoda.rL <- readLines("data1agoda_tanggapan.csv")
#Ubah data tanggapan menjadi koleksi dokumen 
agoda.corpus <- Corpus(VectorSource(agoda.rL))
# 
# 
# 

#DATA TANGGAPAN APLIKASI AGODA
#1. STRUKTURISASI DATA ====

#A. CLEANING TEXT 
#Membuat fungsi untuk menghilangan tanda /,@,\\| 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) 
clean1.agoda <- tm_map(agoda.corpus, toSpace, "/") 
clean2.agoda <- tm_map(clean1.agoda, toSpace, "@") 
clean3.agoda <- tm_map(clean2.agoda, toSpace, "\\|")
#Mengubah karakter huruf kecil 
clean11.agoda <- tm_map(clean3.agoda, content_transformer(tolower))
#Menghilangkan punctuation atau tanda hubung 
clean12.agoda <- tm_map(clean11.agoda, toSpace, "[[:punct:]]")
#Menghilangkan digit atau penomoran angka 
clean13.agoda <- tm_map(clean12.agoda, toSpace, "[[:digit:]]")
# 

#Membuat fungsi untuk menghilangkan karakter emoji 
remove.emoticon <- function (x) gsub("[^\x01-\x7F]", "", x)
clean21.agoda <- tm_map(clean13.agoda, remove.emoticon)
#Membuat fungsi untuk menghilangkan karakter kode 
remove.code <- function (x) gsub("<\\S+","",x) 
clean22.agoda <- tm_map(clean21.agoda, remove.code)
#

#Membuat fungsi untuk menghilangkan karakter char atau penulisan yang berulang 
remove.char <- function (x) gsub("([[:alpha:]])\\1{2,}", "\\1",x) 
clean31.agoda<- tm_map(clean22.agoda, remove.char)
#Menghilangkan jarak antar kata 
clean32.agoda <- tm_map(clean31.agoda, stripWhitespace)
#Membuat fungsi untuk menghilangkan karakter alamat situs 
removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x) 
clean33.agoda <- tm_map(clean32.agoda, removeURL) 
# 

#ubah data menjadi dataframe 
df.clean.agoda <- data.frame(text=unlist(sapply(clean33.agoda, `[`)), 
                             stringsAsFactors=F)

#B. NORMALISASI DATA 
#Import kamus kata normalisasi 
lexiconindo <- read.csv("slangwords.csv")
#proses normalisasi 
slang.agoda <- replace_internet_slang(df.clean.agoda$text,  
                                      slang = paste0("\\b",
                                                     lexiconindo$slang, "\\b"),
                                      replacement = lexiconindo$formal,
                                      ignore.case = TRUE)

#Ubah data menjadi koleksi dokumen 
corslang.agoda <- Corpus(VectorSource(slang.agoda))

#C. STOPWORDS REMOVAL 
#Import kamus stopwords 
myStopwords = readLines("stopword.txt")
#Proses stopwords 
stopw.coragoda <- tm_map(corslang.agoda, removeWords, myStopwords) 
stopw_1.coragoda <- tm_map (stopw.coragoda,  
                            removeWords, c("agoda","airy rooms","airyrooms","sa", 
                                           "airy room","airyroom","airirooms", 
                                           "airiroom","airi rooms","airi room", 
                                           "oyo","reddoorz","reddoors","redoors", 
                                           "boss","lehuga","lehugha","anjir", 
                                           "skuy","yuks","kuy","yuk","uhuy", 
                                           "asap","rcp","imho","cmiiw","leh uga", 
                                           "leh ugha","deh","mantul","jempol", 
                                           "nah","begitu","eh", "situ","deng", 
                                           "eh","heh","airy","airi","oye","rp", 
                                           "deh","e","nih","btw","cs"))

#Menghilangkan jarak antar kata yang ditimbulkan 
stopw_2.coragoda <- tm_map(stopw_1.coragoda, stripWhitespace)
#Ubah data  menjadi dataframe 
df.stopw.agoda <- data.frame(text=unlist(sapply(stopw_2.coragoda, `[`)),  
                             stringsAsFactors=F)
#Menghilangkan record pertama 
df.stopw.agoda <- df.stopw.agoda[-c(1),] 

#D. TOKENISASI 
#Proses tokenisasi 
token.agoda <- tokenize_words(df.stopw.agoda) 

#E. STEMMING 
#Membuat fungsi stemming 
stemming <- function(x){paste(lapply(x,katadasaR))} 
stemm.agoda <- lapply(token.agoda, stemming)
#Membuat fungsi untuk menggabungkan setiap kata yang terpotong 
stemming_github <- function(x){paste(lapply(x,katadasaR),collapse = " ")} 
stemm_1.agoda <- lapply(stemm.agoda, stemming_github)
#Ubah data menjadi dataframe 
df.stegit.agoda <- data.frame(text=unlist(sapply(stemm_1.agoda, `[`)),  
                              stringsAsFactors=F) 
# 
# 
# 

#2. PELABELAN KATA ==== 
#Import kamus kata positif 
pos.words <- scan("positive.txt",what="character",comment.char=";")
#Import kamus kata negatif 
neg.words <- scan("negative.txt",what="character",comment.char=";")

#Proses pelabelan kata 
score.sentiment = function(df.stegit.agoda, pos.words, neg.words,  
                           .progress='none') 
{ 
  # we got a vector of sentences. plyr will handle a list 
  # or a vector as an "l" for us 
  # we want a simple array ("a") of scores back, so we use  
  # "l" + "a" + "ply" = "laply": 
  scores = laply(df.stegit.agoda, function(sentence, pos.words, neg.words) { 
    
    # clean up sentences with R's regex-driven global substitute, gsub(): 
    sentence = gsub('[[:punct:]]', '', sentence) 
    sentence = gsub('[[:cntrl:]]', '', sentence) 
    sentence = gsub('\\d+', '', sentence) 
    # and convert to lower case: 
    sentence = tolower(sentence) 
    
    # split into words. str_split is in the stringr package 
    word.list = str_split(sentence, '\\s+') 
    # sometimes a list() is one level of hierarchy too much 
    words = unlist(word.list) 
    
    # compare our words to the dictionaries of positive & negative terms 
    pos.matches = match(words, pos.words) 
    neg.matches = match(words, neg.words) 
    # match() returns the position of the matched term or NA 
    # we just want a TRUE/FALSE: 
    pos.matches = !is.na(pos.matches) 
    neg.matches = !is.na(neg.matches) 
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum(): 
    score = sum(pos.matches) - (1*sum(neg.matches)) 
    return(score) 
  }, pos.words, neg.words, .progress=.progress ) 
  scores.df = data.frame(score=scores, text=df.stegit.agoda) 
  return(scores.df) 
} 
hasil = score.sentiment(df.stegit.agoda$text, pos.words, neg.words)

#Membuat label berdasarkan nilai skor 
hasil$class <- ifelse(hasil$score <0, "neg","pos")
#Ubah urutan kolom 
label.agoda <- hasil[c(2,3,1)]
#Ubah data menjadi dataframe list 
label.agoda1 <- as.data.frame.list(label.agoda)
#Menggabungkan kolom kata 
label_1.agoda <- cbind(name=data1.agoda$nama, label.agoda1,  
                       date=data1.agoda$tanggal)
#Eksport data hasil pelabelan kata 
write.csv(label.agoda1, "agoda_label1.csv") 
# 
# 
# 

#3. PEMBENTUKAN FREKUENSI KATA ==== 
#Import data csv 
label_2.agoda <- read.csv("agoda_label1.csv")
#Perubahan data menjadi koleksi dokumen 
coragoda.label_2 <- Corpus(VectorSource(label_2.agoda$text))
#Membuat struktur kumpulan kata berdasarkan record dokumen 
dtmagoda.label_2 <- DocumentTermMatrix(coragoda.label_2) 
# 
# 
#

#4. IMPLEMENTASI KLASIFIKASI NAIVE BAYES ==== 
#Membuat partisi data 
# 70% : 30% 
#Partisi data csv 
agodalab.train <- label_2.agoda[1:2692,] 
agodalab.test <- label_2.agoda[2693:3846,] 
#Partisi data kumpulan kata 
dtmagodalab.train <- dtmagoda.label_2[1:2692,] 
dtmagodalab.test <- dtmagoda.label_2[2693:3846,] 
#Partisi data koleksi dokumen 
coragodalab.train <- coragoda.label_2[1:2692] 
coragodalab.test <- coragoda.label_2[2693:3846]

#Membuat partisi data 
# 75% : 25% 
#Partisi data csv 
agodalab.train2 <- label_2.agoda[1:2884,] 
agodalab.test2 <- label_2.agoda[2885:3846,] 
#Partisi data kumpulan kata 
dtmagodalab.train2 <- dtmagoda.label_2[1:2884,] 
dtmagodalab.test2 <- dtmagoda.label_2[2885:3846,] 
#Partisi data koleksi dokumen 
coragodalab.train2 <- coragoda.label_2[1:2884] 
coragodalab.test2 <- coragoda.label_2[2885:3846]

#Membuat partisi data 
# 80% : 20% 
#Partisi data csv 
agodalab.train3 <- label_2.agoda[1:3076,] 
agodalab.test3 <- label_2.agoda[3077:3846,] 
#Partisi data kumpulan kata 
dtmagodalab.train3 <- dtmagoda.label_2[1:3076,] 
dtmagodalab.test3 <- dtmagoda.label_2[3077:3846,] 
#Partisi data koleksi dokumen 
coragodalab.train3 <- coragoda.label_2[1:3076] 
coragodalab.test3 <- coragoda.label_2[3077:3846]

#PERCOBAAN 1 (70% latih : 30% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq1 <- findFreqTerms(dtmagodalab.train, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb1 <- DocumentTermMatrix(coragodalab.train, 
                                    control = list(dictionary=fivefreq1)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb1 <- DocumentTermMatrix(coragodalab.test, 
                                   control = list(dictionary=fivefreq1))

#Membuat fungsi dalam mendeteksi kata dalam dokumen 
convert_count <- function(x) { 
  y <- ifelse(x>0, 1,0) 
  y <- factor(y, levels = c(0,1), labels = c("No","Yes")) 
  y 
} 
#sebagai data latih 
trainNB1 <- apply(dtm.train.nb1, 2, convert_count) 
#sebagai data uji 
testNB1 <- apply(dtm.test.nb1, 2, convert_count) 

#Membuat model klasifikasi data latih 
system.time(classifier<-naiveBayes(trainNB1, agodalab.train$class,  
                                   laplace = 1))
#Prediksi label data uji 
system.time(pred<-predict(classifier, newdata=testNB1))
# 
# 
#

#PERCOBAAN 2 (75% latih : 25% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq2 <- findFreqTerms(dtmagodalab.train2, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb2 <- DocumentTermMatrix(coragodalab.train2, 
                                    control = list(dictionary=fivefreq2)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb2 <- DocumentTermMatrix(coragodalab.test2, 
                                   control = list(dictionary=fivefreq2)) 

#Membuat fungsi dalam mendeteksi kata dalam dokumen 
convert_count <- function(x) { 
  y <- ifelse(x>0, 1,0) 
  y <- factor(y, levels = c(0,1), labels = c("No","Yes")) 
  y 
} 
#sebagai data latih 
trainNB2 <- apply(dtm.train.nb2, 2, convert_count) 
#sebagai data uji 
testNB2 <- apply(dtm.test.nb2, 2, convert_count) 

#Membuat model klasifikasi data latih 
system.time(classifier2 <- naiveBayes(trainNB2, agodalab.train2$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred2 <- predict(classifier2, newdata = testNB2)) 
# 
#
#

#PERCOBAAN 3 (80% latih : 20% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq3 <- findFreqTerms(dtmagodalab.train3, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb3 <- DocumentTermMatrix(coragodalab.train3, 
                                    control = list(dictionary=fivefreq3)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb3 <- DocumentTermMatrix(coragodalab.test3, 
                                   control = list(dictionary=fivefreq3)) 

#Membuat fungsi dalam mendeteksi kata dalam dokumen 
convert_count <- function(x) { 
  y <- ifelse(x>0, 1,0) 
  y <- factor(y, levels = c(0,1), labels = c("No","Yes")) 
  y 
} 
#sebagai data latih 
trainNB3 <- apply(dtm.train.nb3, 2, convert_count) 
#Prediksi label data uji 
testNB3 <- apply(dtm.test.nb3, 2, convert_count) 

#Membuat model klasifikasi data latih 
system.time(classifier3 <- naiveBayes(trainNB3, agodalab.train3$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred3 <- predict(classifier3, newdata = testNB3)) 
#
#
#

#5. AKURASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Perhitungan nilai akurasi data 
conf.mat1 <- confusionMatrix(pred, agodalab.test$class) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Perhitungan nilai akurasi data 
conf.mat2 <- confusionMatrix(pred2, agodalab.test2$class) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Perhitungan nilai akurasi data 
conf.mat3 <- confusionMatrix(pred3, agodalab.test3$class) 
# 
# 
# 

#6. VISUALISASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred, agodalab.test$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual'))

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.agodatest1 <- TermDocumentMatrix(coragodalab.test, 
                                     control = list(dictionary=fivefreq1)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.agodatest1 <- as.matrix(tdm.agodatest1) 
#Membuat frekuensi kemunculan kata tertinggi 
v.agodatest1 <- sort(rowSums(m.agodatest1), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.agodatest1 <- data.frame(word = names(v.agodatest1), freq = v.agodatest1)

#Visualisasi diagram batang 
barplot(d.agodatest[1:9,]$freq, las = 2,  
        names.arg = d.agodatest[1:9,]$word, 
        col = "cyan", main = "Percobaan Satu Aplikasi Agoda", 
        ylab = "Jumlah kata") 
#Visualisasi wordclouds 
wordcloud(words = d.agodatest$word, freq = d.agodatest$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2"))

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.agodatest1,  
                   terms = c("hotel", "bayar", "aplikasi", "pesan",  
                             "batal", "uang", "murah", "harga", "mudah",  
                             "bagus"),
                   corlimit = c(0.25, 0.25, 0.25, 0.25, 0.25, 
                                0.25, 0.1, 0.1, 0.1, 0.1))) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred2, agodalab.test2$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.agodatest2 <- TermDocumentMatrix(coragodalab.test2, 
                                     control = list(dictionary=fivefreq2)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.agodatest2 <- as.matrix(tdm.agodatest2) 
#Membuat frekuensi kemunculan kata tertinggi 
v.agodatest2 <- sort(rowSums(m.agodatest2), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.agodatest2 <- data.frame(word = names(v.agodatest2), freq = v.agodatest2)

#Visualisasi diagram batang 
barplot(d.agodatest2[1:9,]$freq, las = 2,  
        names.arg = d.agodatest2[1:9,]$word, 
        col = "cyan", main = "Percobaan Dua Aplikasi Agoda", 
        ylab = "Jumlah Kata") 
#Visualisasi wordclouds 
wordcloud(words = d.agodatest2$word, freq = d.agodatest2$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2")) 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.agodatest2,  
                   terms = c("hotel", "bayar", "aplikasi", "pesan", "batal",  
                             "uang", "murah", "harga", "mudah", "kecewa"),  
                   corlimit = c(0.25, 0.25, 0.25, 0.25, 0.25,  
                                0.25, 0.1, 0.2, 0.1, 0.2)))

#PERCOBAAN 3 (80% latih : 20% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred3, agodalab.test3$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.agodatest <- TermDocumentMatrix(coragodalab.test3, 
                                    control = list(dictionary=fivefreq3)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.agodatest <- as.matrix(tdm.agodatest) 
#Membuat frekuensi kemunculan kata tertinggi 
v.agodatest <- sort(rowSums(m.agodatest), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.agodatest <- data.frame(word = names(v.agodatest), freq = v.agodatest) 

#Visualisasi diagram batang 
barplot(d.agodatest[1:9,]$freq, las = 2,  
        names.arg = d.agodatest[1:9,]$word, 
        col = "cyan", main = "Percobaan Tiga Aplikasi Agoda", 
        ylab = "Jumlah Kata") 
#Visualisasi wordclouds 
wordcloud(words = d.agodatest$word, freq = d.agodatest$freq, min.freq = 1,  
          max.words = 250, random.order = FALSE, rot.per = 0.65,  
          colors = brewer.pal(8, "Dark2"))

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.agodatest,  
                   terms = c("hotel", "bayar", "aplikasi", "pesan", "batal",  
                             "uang", "murah", "harga", "mudah", "kecewa"),  
                   corlimit = c(0.25, 0.25, 0.25, 0.25, 0.25,  
                                0.25, 0.1, 0.1, 0.1, 0.2)))