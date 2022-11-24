#4. Listing Program Menggunakan Data Tanggapan Aplikasi Oyo 

#Atur penyimpanan lokal selama program berlangsung 
setwd("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahOyo") 
#Import data tanggapan aplikasi agoda 
data.oyo <- read.csv("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahOyo/Oyo_edit.csv",  
                     stringsAsFactors = FALSE, encoding = 'UTF-16') 
#Ubah nama kolom 
names(data.oyo) <- c("nama", "peringkat", "tanggapan", "tanggal")
#Hapus kolom peringkat 
data1.oyo <- data.oyo[, -c(2)] 
#Eksport data csv kolom tanggapan 
write.csv(data1.oyo$tanggapan, "data1oyo_tanggapan.csv")

#Import data tanggapan menggunakan readLines 
oyo.rL <- readLines("data1oyo_tanggapan.csv") 
#Ubah data tanggapan menjadi koleksi dokumen 
oyo.corpus <- Corpus(VectorSource(oyo.rL)) 
# 
# 
#

#DATA TANGGAPAN APLIKASI OYO 
#1. STRUKTURISASI DATA ==== 
#A. CLEANING TEXT 
#Membuat fungsi untuk menghilangan tanda /,@,\\| 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) 
clean1.oyo <- tm_map(oyo.corpus, toSpace, "/") 
clean2.oyo <- tm_map(clean1.oyo, toSpace, "@") 
clean3.oyo <- tm_map(clean2.oyo, toSpace, "\\|") 
#Mengubah karakter huruf kecil 
clean11.oyo <- tm_map(clean3.oyo, content_transformer(tolower)) 
#Menghilangkan punctuation atau tanda hubung 
clean12.oyo <- tm_map(clean11.oyo, toSpace, "[[:punct:]]")
#Menghilangkan digit atau penomoran angka 
clean13.oyo <- tm_map(clean12.oyo, toSpace, "[[:digit:]]") 
#

#Membuat fungsi untuk menghilangkan karakter emoji 
remove.emoticon <- function (x) gsub("[^\x01-\x7F]", "", x) 
clean21.oyo <- tm_map(clean13.oyo, remove.emoticon) 
#Membuat fungsi untuk menghilangkan karakter kode 
remove.code <- function (x) gsub("<\\S+","",x) 
clean22.oyo <- tm_map(clean21.oyo, remove.code) 
#

#Membuat fungsi untuk menghilangkan karakter char atau penulisan yang berulang 
remove.char <- function (x) gsub("([[:alpha:]])\\1{2,}", "\\1",x) 
clean31.oyo<- tm_map(clean22.oyo, remove.char) 
#Menghilangkan jarak antar kata 
clean32.oyo <- tm_map(clean31.oyo, stripWhitespace) 
#Membuat fungsi untuk menghilangkan karakter alamat situs 
removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x) 
clean33.oyo <- tm_map(clean32.oyo, removeURL) 
#

#ubah data menjadi dataframe 
df.clean.oyo <- data.frame(text=unlist(sapply(clean33.oyo, `[`)), 
                           stringsAsFactors=F) 

#B. NORMALISASI DATA 
#Import kamus kata normalisasi 
lexiconindo <- read.csv("slangwords.csv") 
#proses normalisasi 
slang.oyo <- replace_internet_slang(df.clean.oyo$text,  
                                    slang = paste0("\\b", lexiconindo$slang, "\\b"),  
                                    replacement = lexiconindo$formal,  
                                    ignore.case = TRUE) 
#Ubah data menjadi koleksi dokumen 
corslang.oyo <- Corpus(VectorSource(slang.oyo)) 

#C. STOPWORDS REMOVAL 
#Import kamus stopwords 
myStopwords = readLines("stopword.txt") 
#Proses stopwords 
stopw.coroyo <- tm_map(corslang.oyo, removeWords, myStopwords) 
stopw_1.coroyo <- tm_map (stopw.coroyo,  
                          removeWords, c("agoda","airy rooms","airyrooms","sa", 
                                         "airy room","airyroom","airirooms", 
                                         "airiroom","airi rooms","airi room", 
                                         "oyo","reddoorz","reddoors","redoors", 
                                         "boss","lehuga","lehugha","anjir","skuy", 
                                         "yuks","kuy","yuk","uhuy","asap","rcp", 
                                         "imho","cmiiw","leh uga","leh ugha","deh", 
                                         "mantul","jempol","nah","begitu","eh", 
                                         "situ","deng","eh","heh","airy","airi", 
                                         "oye","rp","deh","e","nih","btw","cs","r", 
                                         "ter","unch","nua","wuzt")) 

#Menghilangkan jarak antar kata yang ditimbulkan 
stopw_2.coroyo <- tm_map(stopw_1.coroyo, stripWhitespace) 
#Ubah data  menjadi dataframe 
df.stopw.oyo <- data.frame(text=unlist(sapply(stopw_2.coroyo, `[`)), 
                           stringsAsFactors=F) 
#Menghilangkan record pertama 
df.stopw.oyo <- df.stopw.oyo[-c(1),]

#D. TOKENISASI 
#Proses tokenisasi 
token.oyo <- tokenize_words(df.stopw.oyo)

#E. STEMMING 
#Membuat fungsi stemming 
stemming <- function(x){paste(lapply(x,katadasaR))} 
stemm.oyo <- lapply(token.oyo, stemming) 
#Membuat fungsi untuk menggabungkan setiap kata yang terpotong 
stemming_github <- function(x){paste(lapply(x,katadasaR),collapse = " ")} 
stemm_1.oyo <- lapply(stemm.oyo, stemming_github) 
#Ubah data menjadi dataframe 
df.stegit.oyo <- data.frame(text=unlist(sapply(stemm_1.oyo, `[`)), stringsAsFactors=F) 
# 
# 
#

#2. PELABELAN KATA ==== 
#Import kamus kata positif 
pos.words <- scan("positive.txt",what="character",comment.char=";") 
#Import kamus kata negatif 
neg.words <- scan("negative.txt",what="character",comment.char=";")

#Proses pelabelan kata 
score.sentiment = function(df.stegit.oyo, pos.words, neg.words, .progress='none') 
{ 
  require(plyr) 
  require(stringr) 
  
  # we got a vector of sentences. plyr will handle a list 
  # or a vector as an "l" for us 
  # we want a simple array ("a") of scores back, so we use  
  # "l" + "a" + "ply" = "laply": 
  scores = laply(df.stegit.oyo, function(sentence, pos.words, neg.words) { 
    
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
  
  scores.df = data.frame(score=scores, text=df.stegit.oyo) 
  return(scores.df) 
} 
hasil = score.sentiment(df.stegit.oyo$text, pos.words, neg.words) 

#Membuat label berdasarkan nilai skor 
hasil$class <- ifelse(hasil$score <0, "neg","pos") 
#Ubah urutan kolom 
label.oyo <- hasil[c(2,3,1)] 
#Ubah data menjadi dataframe list 
label.oyo1 <- as.data.frame.list(label.oyo)
#Menggabungkan kolom kata 
label_1.oyo <- cbind(name=data1.oyo$nama, label.oyo1, date=data1.oyo$tanggal) 
#Eksport data hasil pelabelan kata 
write.csv(label_1.oyo,  
          file = "F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahOyo/oyo_label1.csv") 
# 
# 
#

#3. PEMBENTUKAN FREKUENSI KATA ==== 
#Import data csv 
label_2.oyo <- read.csv("oyo_label1.csv") 
#Perubahan data menjadi koleksi dokumen 
coroyo.label_2 <- Corpus(VectorSource(label_2.oyo$text)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen 
dtmoyo.label_2 <- DocumentTermMatrix(coroyo.label_2) 
# 
# 
#

#4. IMPLEMENTASI KLASIFIKASI NAIVE BAYES ==== 
#Membuat partisi data 
# 70% : 30% 
#Partisi data csv 
oyolab.train <- label_2.oyo[1:4751,] 
oyolab.test <- label_2.oyo[4752:6788,] 
#Partisi data kumpulan kata 
dtmoyolab.train <- dtmoyo.label_2[1:4751,] 
dtmoyolab.test <- dtmoyo.label_2[4752:6788,] 
#Partisi data koleksi dokumen 
coroyolab.train <- coroyo.label_2[1:4751] 
coroyolab.test <- coroyo.label_2[4752:6788]

#Membuat partisi data 
# 75% : 25% 
#Partisi data csv 
oyolab.train2 <- label_2.oyo[1:5091,] 
oyolab.test2 <- label_2.oyo[5092:6788,] 
#Partisi data kumpulan kata 
dtmoyolab.train2 <- dtmoyo.label_2[1:5091,] 
dtmoyolab.test2 <- dtmoyo.label_2[5092:6788,] 
#Partisi data koleksi dokumen 
coroyolab.train2 <- coroyo.label_2[1:5091] 
coroyolab.test2 <- coroyo.label_2[5092:6788]

#Membuat partisi data 
# 80% : 20% 
#Partisi data csv 
oyolab.train3 <- label_2.oyo[1:5430,] 
oyolab.test3 <- label_2.oyo[5431:6788,] 
#Partisi data kumpulan kata 
dtmoyolab.train3 <- dtmoyo.label_2[1:5430,] 
dtmoyolab.test3 <- dtmoyo.label_2[5431:6788,] 
#Partisi data koleksi dokumen 
coroyolab.train3 <- coroyo.label_2[1:5430] 
coroyolab.test3 <- coroyo.label_2[5431:6788] 

#PERCOBAAN 1 (70% latih : 30% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq1 <- findFreqTerms(dtmoyolab.train, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb1 <- DocumentTermMatrix(coroyolab.train, 
                                    control = list(dictionary=fivefreq1)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb1 <- DocumentTermMatrix(coroyolab.test, 
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
system.time(classifier <- naiveBayes(trainNB1, oyolab.train$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred <- predict(classifier, newdata = testNB1)) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq2 <- findFreqTerms(dtmoyolab.train2, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb2 <- DocumentTermMatrix(coroyolab.train2, 
                                    control = list(dictionary=fivefreq2)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb2 <- DocumentTermMatrix(coroyolab.test2, 
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
system.time(classifier2 <- naiveBayes(trainNB2, oyolab.train2$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred2 <- predict(classifier2, newdata = testNB2)) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq3 <- findFreqTerms(dtmoyolab.train3, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb3 <- DocumentTermMatrix(coroyolab.train3, 
                                    control = list(dictionary=fivefreq3)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb3 <- DocumentTermMatrix(coroyolab.test3, 
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
system.time(classifier3 <- naiveBayes(trainNB3, oyolab.train3$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred3 <- predict(classifier3, newdata = testNB3)) 
# 
#
#

#5. AKURASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Perhitungan nilai akurasi data 
conf.mat1 <- confusionMatrix(pred, oyolab.test$class) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Perhitungan nilai akurasi data 
conf.mat2 <- confusionMatrix(pred2, oyolab.test2$class) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Perhitungan nilai akurasi data 
conf.mat3 <- confusionMatrix(pred3, oyolab.test3$class) 
# 
# 
# 

#6. VISUALISASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred, oyolab.test$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual'))

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.oyotest1 <- TermDocumentMatrix(coroyolab.test, 
                                   control = list(dictionary=fivefreq1)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.oyotest1 <- as.matrix(tdm.oyotest1) 
#Membuat frekuensi kemunculan kata tertinggi 
v.oyotest1 <- sort(rowSums(m.oyotest1), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.oyotest1 <- data.frame(word = names(v.oyotest1), freq = v.oyotest1) 

#Visualisasi wordclouds 
wordcloud(words = d.oyotest1$word, freq = d.oyotest1$freq, min.freq = 1,  
          max.words = 250, random.order = FALSE, rot.per = 0.65,  
          colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.oyotest1[1:9,]$freq, las = 2, names.arg = d.oyotest1[1:9,]$word, 
        col = "coral", main = "Percobaan Satu Aplikasi Oyo", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.oyotest1,  
                   terms = c("hotel", "aplikasi", "bagus", "mantap", "bayar", 
                             "good", "harga", "murah", "kamar", "kode"),  
                   corlimit = c(0.25, 0.25, 0.1, 0.05, 0.25,  
                                0.05, 0.25, 0.25, 0.25, 0.25))) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred2, oyolab.test2$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.oyotest <- TermDocumentMatrix(coroyolab.test2, 
                                  control = list(dictionary=fivefreq2)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.oyotest <- as.matrix(tdm.oyotest) 
#Membuat frekuensi kemunculan kata tertinggi 
v.oyotest <- sort(rowSums(m.oyotest), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.oyotest <- data.frame(word = names(v.oyotest), freq = v.oyotest) 

#Visualisasi wordclouds 
wordcloud(words = d.oyotest$word, freq = d.oyotest$freq, min.freq = 1,  
          max.words = 250, random.order = FALSE, rot.per = 0.65,  
          colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.oyotest[1:9,]$freq, las = 2, names.arg = d.oyotest[1:9,]$word, 
        col = "coral", main = "Percobaan Dua Aplikasi Oyo", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.oyotest,  
                   terms = c("hotel", "bagus", "aplikasi", "mantap", "good", 
                             "bayar", "harga", "murah", "promo", "kamar"),  
                   corlimit = c(0.25, 0.1, 0.25, 0.01, 0.1,  
                                0.25, 0.25, 0.25, 0.25, 0.25))) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred3, oyolab.test3$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.oyotest3 <- TermDocumentMatrix(coroyolab.test3, 
                                   control = list(dictionary=fivefreq3)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.oyotest3 <- as.matrix(tdm.oyotest3) 
#Membuat frekuensi kemunculan kata tertinggi 
v.oyotest3 <- sort(rowSums(m.oyotest3), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.oyotest3 <- data.frame(word = names(v.oyotest3), freq = v.oyotest3)

#Visualisasi wordclouds 
wordcloud(words = d.oyotest3$word, freq = d.oyotest3$freq, min.freq = 1,  
          max.words = 250, random.order = FALSE, rot.per = 0.65,  
          colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.oyotest3[1:9,]$freq, las = 2, names.arg = d.oyotest3[1:9,]$word, 
        col = "coral", main = "Percobaan Tiga Aplikasi Oyo", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.oyotest3,  
                   terms = c("hotel", "bagus", "aplikasi", "mantap", "good", 
                             "bayar", "harga", "murah", "promo", "kamar"),  
                   corlimit = c(0.25, 0.1, 0.25, 0.05, 0.1,  
                                0.25, 0.25, 0.25, 0.25, 0.25))) 