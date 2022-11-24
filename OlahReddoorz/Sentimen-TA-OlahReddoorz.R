#5. Listing Program Menggunakan Data Tanggapan Aplikasi Reddoorz

#Atur penyimpanan lokal selama program berlangsung 
setwd("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahReddoorz") 
#Import data tanggapan aplikasi agoda 
data.redor <- read.csv("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahReddoorz/Reddoorz_edit.csv",  
                       stringsAsFactors = FALSE, encoding = 'UTF-16')
#Ubah nama kolom 
names(data.redor) <- c("nama", "peringkat", "tanggapan", "tanggal") 
#Hapus kolom peringkat 
data1.redor <- data.redor[, -c(2)] 
#Eksport data csv kolom tanggapan 
write.csv(data1.redor$tanggapan, "data1redor_tanggapan.csv") 

#Import data tanggapan menggunakan readLines 
redor.rL <- readLines("data1redor_tanggapan.csv") 
#Ubah data tanggapan menjadi koleksi dokumen 
redor.corpus <- Corpus(VectorSource(redor.rL)) 
# 
# 
#

#DATA TANGGAPAN APLIKASI REDDOORZ 
#1. STRUKTURISASI DATA ==== 
#A. CLEANING TEXT 
#Membuat fungsi untuk menghilangan tanda /,@,\\| 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) 
clean1.redor <- tm_map(redor.corpus, toSpace, "/") 
clean2.redor <- tm_map(clean1.redor, toSpace, "@") 
clean3.redor <- tm_map(clean2.redor, toSpace, "\\|") 
#Mengubah karakter huruf kecil 
clean11.redor <- tm_map(clean3.redor, content_transformer(tolower)) 
#Menghilangkan punctuation atau tanda hubung 
clean12.redor <- tm_map(clean11.redor, toSpace, "[[:punct:]]") 
#Menghilangkan digit atau penomoran angka 
clean13.redor <- tm_map(clean12.redor, toSpace, "[[:digit:]]") 
#

#Membuat fungsi untuk menghilangkan karakter emoji 
remove.emoticon <- function (x) gsub("[^\x01-\x7F]", "", x) 
clean21.redor <- tm_map(clean13.redor, remove.emoticon) 
#Membuat fungsi untuk menghilangkan karakter kode 
remove.code <- function (x) gsub("<\\S+","",x) 
clean22.redor <- tm_map(clean21.redor, remove.code) 
#

#Membuat fungsi untuk menghilangkan karakter char atau penulisan yang berulang 
remove.char <- function (x) gsub("([[:alpha:]])\\1{2,}", "\\1",x) 
clean31.redor<- tm_map(clean22.redor, remove.char) 
#Menghilangkan jarak antar kata 
clean32.redor <- tm_map(clean31.redor, stripWhitespace) 
#Membuat fungsi untuk menghilangkan karakter alamat situs 
removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x) 
clean33.redor <- tm_map(clean32.redor, removeURL) 
#

#ubah data menjadi dataframe 
df.clean.redor <- data.frame(text=unlist(sapply(clean33.redor, `[`)),  
                             stringsAsFactors=F)

#B. NORMALISASI DATA 
#Import kamus kata normalisasi 
lexiconindo <- read.csv("slangwords.csv") 
#proses normalisasi 
slang.redor <- replace_internet_slang(df.clean.redor$text,  
                                      slang = paste0("\\b", lexiconindo$slang, "\\b"),  
                                      replacement = lexiconindo$formal,  
                                      ignore.case = TRUE) 
#Ubah data menjadi koleksi dokumen 
corslang.redor <- Corpus(VectorSource(slang.redor))

#C. STOPWORDS REMOVAL 
#Import kamus stopwords 
myStopwords = readLines("stopword.txt") 
#Proses stopwords 
stopw.corredor <- tm_map(corslang.redor, removeWords, myStopwords) 
stopw_1.corredor <- tm_map (stopw.corredor,  
                            removeWords, c("agoda","airy rooms","airyrooms","sa", 
                                           "airy room","airyroom","airirooms", 
                                           "airiroom","airi rooms","airi room", 
                                           "oyo","reddoorz","reddoors","redoors", 
                                           "boss","lehuga","lehugha","anjir","skuy", 
                                           "yuks","kuy","yuk","uhuy","asap","rcp", 
                                           "imho","cmiiw","leh uga","leh ugha","deh", 
                                           "mantul","jempol","nah","begitu","eh", 
                                           "situ","deng","eh","heh","airy","airi", 
                                           "oye","rp","deh","e","nih","btw","cs","r")) 

#Menghilangkan jarak antar kata yang ditimbulkan 
stopw_2.corredor <- tm_map(stopw_1.corredor, stripWhitespace) 
#Ubah data  menjadi dataframe 
df.stopw.redor <- data.frame(text=unlist(sapply(stopw_2.corredor, `[`)), 
                             stringsAsFactors=F) 
#Menghilangkan record pertama 
df.stopw.redor <- df.stopw.redor[-c(1),] 

#D. TOKENISASI 
#Proses tokenisasi 
token.redor <- tokenize_words(df.stopw.redor) 

#E. STEMMING 
#Membuat fungsi stemming 
stemming <- function(x){paste(lapply(x,katadasaR))} 
stemm.redor <- lapply(token.redor, stemming) 
#Membuat fungsi untuk menggabungkan setiap kata yang terpotong 
stemming_github <- function(x){paste(lapply(x,katadasaR),collapse = " ")} 
stemm_1.redor <- lapply(stemm.redor, stemming_github) 
#Ubah data menjadi dataframe 
df.stegit.redor <- data.frame(text=unlist(sapply(stemm_1.redor, `[`)), 
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
score.sentiment = function(df.stegit.redor, pos.words, neg.words, .progress='none') 
{ 
  require(plyr) 
  require(stringr) 
  
  # we got a vector of sentences. plyr will handle a list 
  # or a vector as an "l" for us 
  # we want a simple array ("a") of scores back, so we use  
  # "l" + "a" + "ply" = "laply": 
  scores = laply(df.stegit.redor, function(sentence, pos.words, neg.words) { 
    
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
  
  scores.df = data.frame(score=scores, text=df.stegit.redor) 
  return(scores.df) 
} 
hasil = score.sentiment(df.stegit.redor$text, pos.words, neg.words) 

#Membuat label berdasarkan nilai skor 
hasil$class <- ifelse(hasil$score <0, "neg","pos") 
#Ubah urutan kolom 
label.redor <- hasil[c(2,3,1)] 
#Ubah data menjadi dataframe list 
label.redor1 <- as.data.frame.list(label.redor)

#Menggabungkan kolom kata 
label_1.redor <- cbind(name=data1.redor$nama, label.redor1, 
                       date=data1.redor$tanggal) 
#Eksport data hasil pelabelan kata 
write.csv(label_1.redor,  
          file = "F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahReddoorz/redor_label1.csv") 
# 
# 
# 

#3. PEMBENTUKAN FREKUENSI KATA ==== 
#Import data csv 
label_2.redor <- read.csv("redor_label1.csv") 
#Perubahan data menjadi koleksi dokumen 
corredor.label_2 <- Corpus(VectorSource(label_2.redor$text)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen 
dtmredor.label_2 <- DocumentTermMatrix(corredor.label_2) 
# 
# 
#

#4. IMPLEMENTASI KLASIFIKASI NAIVE BAYES ==== 
#Membuat partisi data 
# 70% : 30% 
#Partisi data csv 
redorlab.train <- label_2.redor[1:7256,] 
redorlab.test <- label_2.redor[7257:10366,] 
#Partisi data kumpulan kata 
dtmredorlab.train <- dtmredor.label_2[1:7256,] 
dtmredorlab.test <- dtmredor.label_2[7257:10366,] 
#Partisi data koleksi dokumen 
corredorlab.train <- corredor.label_2[1:7256] 
corredorlab.test <- corredor.label_2[7257:10366] 

#Membuat partisi data 
# 75% : 25% 
#Partisi data csv 
redorlab.train2 <- label_2.redor[1:7774,] 
redorlab.test2 <- label_2.redor[7775:10366,] 
#Partisi data kumpulan kata 
dtmredorlab.train2 <- dtmredor.label_2[1:7774,] 
dtmredorlab.test2 <- dtmredor.label_2[7775:10366,] 
#Partisi data koleksi dokumen 
corredorlab.train2 <- corredor.label_2[1:7774] 
corredorlab.test2 <- corredor.label_2[7775:10366]

#Membuat partisi data 
# 80% : 20% 
#Partisi data csv 
redorlab.train3 <- label_2.redor[1:8292,] 
redorlab.test3 <- label_2.redor[8293:10366,] 
#Partisi data kumpulan kata 
dtmredorlab.train3 <- dtmredor.label_2[1:8292,] 
dtmredorlab.test3 <- dtmredor.label_2[8293:10366,] 
#Partisi data koleksi dokumen 
corredorlab.train3 <- corredor.label_2[1:8292] 
corredorlab.test3 <- corredor.label_2[8293:10366]

#PERCOBAAN 1 (70% latih : 30% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq1 <- findFreqTerms(dtmredorlab.train, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb1 <- DocumentTermMatrix(corredorlab.train, 
                                    control = list(dictionary=fivefreq1)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb1 <- DocumentTermMatrix(corredorlab.test, 
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
system.time(classifier<-naiveBayes(trainNB1, redorlab.train$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred<-predict(classifier, newdata=testNB1)) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq2 <- findFreqTerms(dtmredorlab.train2, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb2 <- DocumentTermMatrix(corredorlab.train2, 
                                    control = list(dictionary=fivefreq2)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb2 <- DocumentTermMatrix(corredorlab.test2, 
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
system.time(classifier2<-naiveBayes(trainNB2, redorlab.train2$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred2<-predict(classifier2, newdata=testNB2)) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq3 <- findFreqTerms(dtmredorlab.train3, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb3 <- DocumentTermMatrix(corredorlab.train3, 
                                    control = list(dictionary=fivefreq3)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb3 <- DocumentTermMatrix(corredorlab.test3, 
                                   control = list(dictionary=fivefreq3)) 

#Membuat fungsi dalam mendeteksi kata dalam dokumen 
convert_count <- function(x) { 
  y <- ifelse(x>0, 1,0) 
  y <- factor(y, levels = c(0,1), labels = c("No","Yes")) 
  y 
} 
#sebagai data latih 
trainNB3 <- apply(dtm.train.nb3, 2, convert_count) 
#sebagai data uji 
testNB3 <- apply(dtm.test.nb3, 2, convert_count) 

#Membuat model klasifikasi data latih 
system.time(classifier3<-naiveBayes(trainNB3, redorlab.train3$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred3<-predict(classifier3, newdata=testNB3)) 
# 
# 
# 

#5. AKURASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Perhitungan nilai akurasi data 
conf.mat1 <- confusionMatrix(pred, redorlab.test$class) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Perhitungan nilai akurasi data 
conf.mat2 <- confusionMatrix(pred2, redorlab.test2$class) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Perhitungan nilai akurasi data 
conf.mat3 <- confusionMatrix(pred3, redorlab.test3$class) 
# 
# 
# 

#6. VISUALISASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred, redorlab.test$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.redortest1 <- TermDocumentMatrix(corredorlab.test, 
                                     control = list(dictionary=fivefreq1)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.redortest1 <- as.matrix(tdm.redortest1) 
#Membuat frekuensi kemunculan kata tertinggi 
v.redortest1 <- sort(rowSums(m.redortest1), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.redortest1 <- data.frame(word = names(v.redortest1), freq = v.redortest1) 

#Visualisasi wordclouds 
wordcloud(words = d.redortest1$word, freq = d.redortest1$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.redortest1[1:9,]$freq, las = 2, names.arg = d.redortest1[1:9,]$word, 
        col = "papaya whip", main = "Percobaan Satu Aplikasi Reddoorz", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.redortest1,  
                   terms = c("good", "bagus", "mantap", "hotel", "mudah",  
                             "bantu", "murah", "aplikasi", "nyaman","kamar"),  
                   corlimit = c(0.05, 0.1, 0.01, 0.25, 0.05,  
                                0.1, 0.1, 0.25, 0.1, 0.25))) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred2, redorlab.test2$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.redortest2 <- TermDocumentMatrix(corredorlab.test2, 
                                     control = list(dictionary=fivefreq2)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.redortest2 <- as.matrix(tdm.redortest2) 
#Membuat frekuensi kemunculan kata tertinggi 
v.redortest2 <- sort(rowSums(m.redortest2), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.redortest2 <- data.frame(word = names(v.redortest2), freq = v.redortest2) 

#Visualisasi wordclouds 
wordcloud(words = d.redortest2$word, freq = d.redortest2$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.redortest2[1:9,]$freq, las = 2, names.arg = d.redortest2[1:9,]$word, 
        col = "papaya whip", main = "Percobaan Dua Aplikasi Reddoorz", 
        ylab = "jumlah kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.redortest2,  
                   terms = c("good", "bagus", "mantap", "hotel","mudah",  
                             "bantu", "murah", "aplikasi", "nyaman", "harga"),  
                   corlimit = c(0.05, 0.1, 0.05, 0.25, 0.1,  
                                0.1, 0.1, 0.25, 0.1, 0.25))) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred3, redorlab.test3$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual'))

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.redortest1 <- TermDocumentMatrix(corredorlab.test, 
                                     control = list(dictionary=fivefreq1)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.redortest1 <- as.matrix(tdm.redortest1) 
#Membuat frekuensi kemunculan kata tertinggi 
v.redortest1 <- sort(rowSums(m.redortest1), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.redortest1 <- data.frame(word = names(v.redortest1), freq = v.redortest1) 

#Visualisasi wordclouds 
wordcloud(words = d.redortest1$word, freq = d.redortest1$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.redortest1[1:9,]$freq, las = 2, names.arg = d.redortest1[1:9,]$word, 
        col = "papaya whip", main = "Percobaan Satu Aplikasi Reddoorz", 
        ylab = "Jumlah Kata")

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.redortest1,  
                   terms = c("good", "bagus", "mantap", "hotel", "mudah",  
                             "bantu", "murah", "aplikasi", "nyaman","kamar"),  
                   corlimit = c(0.05, 0.1, 0.01, 0.25, 0.05,  
                                0.1, 0.1, 0.25, 0.1, 0.25))) 




