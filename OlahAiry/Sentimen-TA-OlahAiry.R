#3. Listing Program Menggunakan Data Tanggapan Aplikasi Airyrooms

#Atur penyimpanan lokal selama program berlangsung
setwd("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahAiry") 
#Import data tanggapan aplikasi agoda 
data.airy <- read.csv("F:/Softskills/Linkedin/Portofolio/AnSent-TestiApliPenginapan-NBC/OlahAiry/AiryRooms_rev.csv",  
                      stringsAsFactors = FALSE, encoding = 'UTF-16')
#Ubah nama kolom 
names(data.airy) <- c("nama", "peringkat", "tanggapan", "tanggal") 
#Hapus kolom peringkat 
data1.airy <- data.airy[, -c(2)] 
#Eksport data csv kolom tanggapan 
write.csv(data1.airy$tanggapan, "data1airy_tanggapan.csv") 
#Import data tanggapan menggunakan readLines 
airy.rL <- readLines("data1airy_tanggapan.csv") 
#Ubah data tanggapan menjadi koleksi dokumen 
airy.corpus <- Corpus(VectorSource(airy.rL)) 
# 
# 
#

#DATA TANGGAPAN APLIKASI AIRYROOMS 
#1. STRUKTURISASI DATA ==== 
#A. CLEANING TEXT 
#Membuat fungsi untuk menghilangan tanda /,@,\\| 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) 
clean1.airy <- tm_map(airy.corpus, toSpace, "/") 
clean2.airy <- tm_map(clean1.airy, toSpace, "@") 
clean3.airy <- tm_map(clean2.airy, toSpace, "\\|") 
#Mengubah karakter huruf kecil 
clean11.airy <- tm_map(clean3.airy, content_transformer(tolower))
#Menghilangkan punctuation atau tanda hubung 
clean12.airy <- tm_map(clean11.airy, toSpace, "[[:punct:]]") 
#Menghilangkan digit atau penomoran angka 
clean13.airy <- tm_map(clean12.airy, toSpace, "[[:digit:]]") 
# 

#Membuat fungsi untuk menghilangkan karakter emoji 
remove.emoticon <- function (x) gsub("[^\x01-\x7F]", "", x) 
clean21.airy <- tm_map(clean13.airy, remove.emoticon) 
#Membuat fungsi untuk menghilangkan karakter kode 
remove.code <- function (x) gsub("<\\S+","",x) 
clean22.airy <- tm_map(clean21.airy, remove.code) 
# 

#Membuat fungsi untuk menghilangkan karakter char atau penulisan yang berulang 
remove.char <- function (x) gsub("([[:alpha:]])\\1{2,}", "\\1",x) 
clean31.airy<- tm_map(clean22.airy, remove.char) 
#Menghilangkan jarak antar kata 
clean32.airy <- tm_map(clean31.airy, stripWhitespace) 
#Membuat fungsi untuk menghilangkan karakter alamat situs 
removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x) 
clean33.airy <- tm_map(clean32.airy, removeURL) 
# 

#ubah data menjadi dataframe 
df.clean.airy <- data.frame(text=unlist(sapply(clean33.airy, `[`)), 
                            stringsAsFactors=F) 

#B. NORMALISASI DATA 
#Import kamus kata normalisasi 
lexiconindo <- read.csv("slangwords.csv") 
#proses normalisasi 
slang.airy <- replace_internet_slang(df.clean.airy$text,  
                                     slang = paste0("\\b", lexiconindo$slang, "\\b"),  
                                     replacement = lexiconindo$formal,  
                                     ignore.case = TRUE) 

#Ubah data menjadi koleksi dokumen 
corslang.airy <- Corpus(VectorSource(slang.airy)) 

#C. STOPWORDS REMOVAL 
#Import kamus stopwords 
myStopwords = readLines("stopword.txt") 
#Proses stopwords 
stopw.corairy <- tm_map(corslang.airy, removeWords, myStopwords) 
stopw_1.corairy <- tm_map (stopw.corairy,  
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
stopw_2.corairy <- tm_map(stopw_1.corairy, stripWhitespace) 
#Ubah data  menjadi dataframe 
df.stopw.airy <- data.frame(text=unlist(sapply(stopw_2.corairy, `[`)),  
                            stringsAsFactors=F) 
#Menghilangkan record pertama 
df.stopw.airy <- df.stopw.airy[-c(1),] 

#D. TOKENISASI 
#Proses tokenisasi 
token.airy <- tokenize_words(df.stopw.airy) 

#E. STEMMING 
#Membuat fungsi stemming 
stemming <- function(x){paste(lapply(x,katadasaR))} 
stemm.airy <- lapply(token.airy, stemming) 
#Membuat fungsi untuk menggabungkan setiap kata yang terpotong 
stemming_github <- function(x){paste(lapply(x,katadasaR),collapse = " ")} 
stemm_1.airy <- lapply(stemm.airy, stemming_github) 
#Ubah data menjadi dataframe 
df.stegit.airy <- data.frame(text=unlist(sapply(stemm_1.airy, `[`)), 
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
score.sentiment = function(df.stegit.airy, pos.words, neg.words, .progress='none') 
{ 
  # we got a vector of sentences. plyr will handle a list 
  # or a vector as an "l" for us 
  # we want a simple array ("a") of scores back, so we use  
  # "l" + "a" + "ply" = "laply": 
  scores = laply(df.stegit.airy, function(sentence, pos.words, neg.words) { 
    
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
  scores.df = data.frame(score=scores, text=df.stegit.airy) 
  return(scores.df) 
} 
hasil = score.sentiment(df.stegit.airy$text, pos.words, neg.words) 

#Membuat label berdasarkan nilai skor 
hasil$class <- ifelse(hasil$score <0, "neg","pos") 
#Ubah urutan kolom 
label.airy <- hasil[c(2,3,1)] 
#Ubah data menjadi dataframe list 
label.airy1 <- as.data.frame.list(label.airy) 
#Menggabungkan kolom kata 
label_1.airy <- cbind(name=data1.airy$nama, label.airy, date=data1.airy$tanggal) 
#Eksport data hasil pelabelan kata 
write.csv(label_1.airy, "airy_label1.csv") 
# 
# 
#

#3. PEMBENTUKAN FREKUENSI KATA ==== 
#Import data csv 
label_2.airy <- read.csv("airy_label1.csv") 
#Perubahan data menjadi koleksi dokumen 
corairy.label_2 <- Corpus(VectorSource(label_2.airy$text)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen 
dtmairy.label_2 <- DocumentTermMatrix(corairy.label_2) 
# 
# 
#

#4. IMPLEMENTASI KLASIFIKASI NAIVE BAYES ==== 
#Membuat partisi data 
# 70% : 30% 
#Partisi data csv 
airylab.train <- label_2.airy[1:3070,] 
airylab.test <- label_2.airy[3071:4386,] 
#Partisi data kumpulan kata 
dtmairylab.train <- dtmairy.label_2[1:3070,] 
dtmairylab.test <- dtmairy.label_2[3071:4386,] 
#Partisi data koleksi dokumen 
corairylab.train <- corairy.label_2[1:3070] 
corairylab.test <- corairy.label_2[3071:4386] 

#Membuat partisi data 
# 75% : 25% 
#Partisi data csv 
airylab.train2 <- label_2.airy[1:3289,] 
airylab.test2 <- label_2.airy[3290:4386,] 
#Partisi data kumpulan kata 
dtmairylab.train2 <- dtmairy.label_2[1:3289,] 
dtmairylab.test2 <- dtmairy.label_2[3290:4386,] 
#Partisi data koleksi dokumen 
corairylab.train2 <- corairy.label_2[1:3289] 
corairylab.test2 <- corairy.label_2[3290:4386]

#Membuat partisi data 
# 80% : 20% 
#Partisi data csv 
airylab.train3 <- label_2.airy[1:3508,] 
airylab.test3 <- label_2.airy[3509:4386,] 
#Partisi data kumpulan kata 
dtmairylab.train3 <- dtmairy.label_2[1:3508,] 
dtmairylab.test3 <- dtmairy.label_2[3509:4386,] 
#Partisi data koleksi dokumen 
corairylab.train3 <- corairy.label_2[1:3058] 
corairylab.test3 <- corairy.label_2[3509:4386] 
# 
# 
# 

#PERCOBAAN 1 (70% latih : 30% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq1 <- findFreqTerms(dtmairylab.train, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb1 <- DocumentTermMatrix(corairylab.train, 
                                    control = list(dictionary=fivefreq1)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb1 <- DocumentTermMatrix(corairylab.test, 
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
system.time(classifier<-naiveBayes(trainNB1, airylab.train$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred<-predict(classifier, newdata=testNB1)) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq2 <- findFreqTerms(dtmairylab.train2, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb2 <- DocumentTermMatrix(corairylab.train2, 
                                    control = list(dictionary=fivefreq2)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb2 <- DocumentTermMatrix(corairylab.test2, 
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
system.time(classifier2<-naiveBayes(trainNB2, airylab.train2$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred2<-predict(classifier2, newdata=testNB2))

#PERCOBAAN 3 (80% latih : 20% uji) 
#Membuat daftar kumpulan kata yang memiliki frekuensi kemunculan sebanyak 2 kali 
fivefreq3 <- findFreqTerms(dtmairylab.train3, 2) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data latih 
dtm.train.nb3 <- DocumentTermMatrix(corairylab.train3, 
                                    control = list(dictionary=fivefreq3)) 
#Membuat struktur kumpulan kata berdasarkan record dokumen data uji 
dtm.test.nb3 <- DocumentTermMatrix(corairylab.test3, 
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
system.time(classifier3 <- naiveBayes(trainNB3, airylab.train3$class, laplace = 1)) 
#Prediksi label data uji 
system.time(pred3 <- predict(classifier3, newdata=testNB3)) 
#
#
#

#5. AKURASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Perhitungan nilai akurasi data 
conf.mat1 <- confusionMatrix(pred, airylab.test$class) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Perhitungan nilai akurasi data 
conf.mat2 <- confusionMatrix(pred2, airylab.test2$class) 

#PERCOBAAN 3 (80% latih : 20% uji) 
#Perhitungan nilai akurasi data 
conf.mat3 <- confusionMatrix(pred3, airylab.test3$class) 
# 
# 
# 

#6. VISUALISASI DATA ==== 
#PERCOBAAN 1 (70% latih : 30% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred, airylab.test$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual'))

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.airytest1 <- TermDocumentMatrix(corairylab.test, 
                                    control = list(dictionary=fivefreq1)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.airytest1 <- as.matrix(tdm.airytest1) 
#Membuat frekuensi kemunculan kata tertinggi 
v.airytest1 <- sort(rowSums(m.airytest1), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.airytest1 <- data.frame(word = names(v.airytest1), freq = v.airytest1) 

#Visualisasi wordclouds 
wordcloud(words = d.airytest1$word, freq = d.airytest1$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.airytest1[1:9,]$freq, las = 2,  
        names.arg = d.airytest1[1:9,]$word, col = "light blue",  
        main = "Percobaan Satu Aplikasi Airyrooms", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.airytest1,  
                   terms = c("murah", "bagus", "mantap", "aplikasi", "good",  
                             "harga", "mudah", "tiket", "bantu", "hotel"),  
                   corlimit = c(0.25, 0.1, 0.05, 0.25, 0.05,  
                                0.25, 0.1, 0.25, 0.1, 0.25))) 

#PERCOBAAN 2 (75% latih : 25% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred2, airylab.test2$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.airytest2 <- TermDocumentMatrix(corairylab.test2, 
                                    control = list(dictionary=fivefreq2)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.airytest2 <- as.matrix(tdm.airytest2) 
#Membuat frekuensi kemunculan kata tertinggi 
v.airytest2 <- sort(rowSums(m.airytest2), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.airytest2 <- data.frame(word = names(v.airytest2), freq = v.airytest2) 

#Visualisasi wordclouds 
wordcloud(words = d.airytest2$word, freq = d.airytest2$freq, min.freq = 1,  
          max.words = 250, random.order = FALSE, rot.per = 0.65,  
          colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.airytest2[1:9,]$freq, las = 2,  
        names.arg = d.airytest2[1:9,]$word, 
        col = "light blue", main = "Percobaan Dua Aplikasi Airyrooms", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.airytest2,  
                   terms = c("murah", "bagus", "mantap", "good", "harga",  
                             "aplikasi", "mudah", "tiket", "bantu", "hotel"),  
                   corlimit = c(0.25, 0.1, 0.05, 0.05, 0.25,  
                                0.25, 0.1, 0.25, 0.1, 0.25)))

#PERCOBAAN 3 (80% latih : 20% uji) 
#Mengetahui jumlah prediksi pengujian data 
CrossTable(pred3, airylab.test3$class, 
           prop.chisq = FALSE,  
           prop.t = FALSE, 
           dnn = c('Predicted', 'Actual')) 

#Membuat struktur kumpulan kata berdasarkan frekuensi kemunculan kata 
tdm.airytest1 <- TermDocumentMatrix(corairylab.test, 
                                    control = list(dictionary=fivefreq1)) 
#Membuat frekuensi kemunculan kata dalam bentuk matriks 
m.airytest1 <- as.matrix(tdm.airytest1) 
#Membuat frekuensi kemunculan kata tertinggi 
v.airytest1 <- sort(rowSums(m.airytest1), decreasing = TRUE) 
#Membuat frekuensi kemunculan kata tertinggi pada dataframe 
d.airytest1 <- data.frame(word = names(v.airytest1), freq = v.airytest1)

#Visualisasi wordclouds 
wordcloud(words = d.airytest1$word, freq = d.airytest1$freq,  
          min.freq = 1, max.words = 250, random.order = FALSE,  
          rot.per = 0.65, colors = brewer.pal(8, "Dark2")) 
#Visualisasi diagram batang 
barplot(d.airytest1[1:9,]$freq, las = 2,  
        names.arg = d.airytest1[1:9,]$word, col = "light blue",  
        main = "Percobaan Satu Aplikasi Airyrooms", 
        ylab = "Jumlah Kata") 

#Mencari hubungan kata berdasarkan kata tertinggi dengan corlimit 
as.list(findAssocs(tdm.airytest1,  
                   terms = c("murah", "bagus", "mantap", "aplikasi", "good",  
                             "harga", "mudah", "tiket", "bantu", "hotel"),  
                   corlimit = c(0.25, 0.1, 0.05, 0.25, 0.05,  
                                0.25, 0.1, 0.25, 0.1, 0.25))) 