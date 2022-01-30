library(sentencepiece)
library(tokenizers.bpe)
library(easyPSID)
library(spacyr)
# Creating a BPE model-------------------------
my_data=readRDS("\Assignment Unit_5\data_corpus_LMRD.rds")
data_corpus_LMRD <- readRDS("\Assignment Unit_5\data_corpus_LMRD.rds")
my_data=my_data[1:50]
#UD for R
library(udpipe)

model <- udpipe_download_model(language = "english") 
udmodel_es <- udpipe_load_model(file = model$file_model)

anno <- udpipe_annotate(udmodel_es, x = my_data )
df <- as.data.frame(anno)

df[,5:14]
text_part1 <- paste(unlist(my_data[1:40]), collapse="\n")


text_part2 <- paste(unlist(my_data[41:50]), collapse="\n")
#I can't use a single line with all the text (text_part1), but I can use a vector of chapters
model <- bpe(unlist(my_data[1:40]))
#We apply the model to the second part of the text (here we can use a single string)
subtoks2 <- bpe_encode(model, x = text_part2, type = "subwords")            
head(unlist(subtoks2), n=20)
#VAMOS A VER LAS PALABRAS MÁS COMUNES
plot(head(sort(table(unlist(subtoks2)), decreasing = TRUE), n = 10),
     Xlab = "Token",
     Ylab = "Ocurrences"
)


spacy_initialize(model = "en_core_web_sm-3.0.0") 
sentences_part1 <- spacy_tokenize(text_part1, what="sentence") #Returns a list
v_sentences_part1 <- unlist(sentences_part1) 
#Write the sentences in a file
train_file <- "Qsentencepiece.BPE.part1.txt"
writeLines(text = v_sentences_part1, con = train_file)
#Create a BPE model
model <- sentencepiece(train_file, #File with sentences
                       type = "bpe", #A BPE model. There are other models, like unigram
                       coverage = 0.9999, #Default value 0.9999
                       vocab_size = 5000, #Default value 5000
                       #threads = 1, #By defaul it is 1. However, tokenizers.bpe uses all available.
                       model_dir = getwd(), #Current directory. Creates two files:
                       # sentencepiece.model and sentencepiece.vocab.
                       verbose = FALSE) #Useful for debugging
#We apply the model to the second part of the text (here we can use a single string with whole text)
subtoks2_sentencepiece <- sentencepiece_encode(model, x = text_part2, type = "subwords")
head(unlist(subtoks2_sentencepiece), n=20) #tokenization made by sentencepiece
plot(head(sort(table(unlist(subtoks2_sentencepiece)), decreasing = TRUE), n = 10),
     Xlab = "Token",
     Ylab = "Ocurrences"
)
model <- bpe(v_sentences_part1)
subtoks2_alt <- bpe_encode(model, x = text_part2, type = "subwords")
head(unlist(subtoks2_alt), n=20)
plot(head(sort(table(unlist(subtoks2_alt)), decreasing = TRUE), n = 10),
     Xlab = "Token",
     Ylab = "Ocurrences"
)
v <- unlist(subtoks2_alt)
strwrap(substring(paste(v, collapse = " "), 1, 200), exdent = 2)
#The pipe quivalent is this
library(magrittr) #although it is included by many pckages, this is the original
paste(v, collapse = " ") %>% #Creates a string with the tokens
  substring(1, 200) %>% #Keep the first 200 characters
  strwrap(exdent = 2) #A nice resulting string with indent
#Distance between texts------------------------
#Creates a quanteda corpus
library(quanteda)
texts_my_data <- unlist(my_data)
names(texts_my_data) <- paste("Chap.", 1:length(texts_my_data)) #assigns a name to each string
corpus_my_dataQ <- corpus(texts_my_data)
docvars(corpus_my_dataQ, field="Chapter") <- 1:length(texts_my_data) #docvar with chapter number
corpus_my_dataQ
#Creates the dfm (document-feature matrix)
dfm_my_dataQ <- dfm(tokens(corpus_my_dataQ),
                 #Default values:
                 # tolower = TRUE #Convers to lowercase
                 # remove_padding = FALSE #Does padding (fills with blanks)
)
#Does a dendrogram
distMatrix <-dist(as.matrix(dfm_my_dataQ),
                  method="euclidean")
groups <-hclust(distMatrix , method="ward.D")
plot(groups,
     cex =0.25, #Size of labels
     hang= -1, #Same hight labels
     xlab = "", #Text of axis x
     ylab = "", #Text of axis y
     main = "" #Text of drawing
)
rect.hclust(groups, k=5)
topfeatures(dfm_my_dataQ)#provides the 10 most frequent (or infrequent) features (tokens) from a tfm.
#Notice that the most common features include punctuation marks and stop words. To remove all this we can do:
  #Without puntuation marks
  dfm_my_dataQ_1 <- dfm(tokens(corpus_my_dataQ,
                            remove_punct = TRUE
                            #Default values:
                            # remove_punct = FALSE,
                            # remove_symbols = FALSE,
                            # remove_numbers = FALSE,
                            # remove_url = FALSE,
                            # remove_separators = TRUE,
                            # split_hyphens = FALSE
  ),
  #Default values:
  # tolower = TRUE #Convert to lowercase
  # remove_padding = FALSE #Does padding (fill up blanks)
  )
#Without stop words
dfm_my_dataQ_2 <- dfm_remove(dfm_my_dataQ_1, stopwords("en"))
topfeatures(dfm_my_dataQ_2)
topfeatures(dfm_my_dataQ_2,
            decreasing = FALSE #By default it is TRUE
)

#Using docvars
corpus_part1 <- corpus_subset(corpus_my_dataQ,
                              Chapter < 40 #I keep chaps from 1 to 5
)
corpus_part2 <- corpus_subset(corpus_my_dataQ,
                              Chapter > 41 #I keep chaps from 6 to end
)
dfm_part1_noPunct <- dfm(tokens(corpus_part1, remove_punct = TRUE))
dfm_part2_noPunct <- dfm(tokens(corpus_part2, remove_punct = TRUE))
dfm_part1_noPunct_noSW <- dfm_remove(dfm_part1_noPunct, stopwords("en"))
dfm_part2_noPunct_noSW <- dfm_remove(dfm_part2_noPunct, stopwords("en"))
library(quanteda.textplots)
#Most frequent feat
topfeatures(dfm_part1_noPunct)
topfeatures(dfm_part1_noPunct, decreasing = FALSE)
textplot_wordcloud(dfm_part1_noPunct)
topfeatures(dfm_part2_noPunct)
topfeatures(dfm_part2_noPunct, decreasing = FALSE)
textplot_wordcloud(dfm_part2_noPunct)
#Less frequent feat
topfeatures(dfm_part1_noPunct_noSW)
topfeatures(dfm_part1_noPunct_noSW, decreasing = FALSE)
textplot_wordcloud(dfm_part1_noPunct_noSW)
topfeatures(dfm_part2_noPunct_noSW)
topfeatures(dfm_part2_noPunct_noSW, decreasing = FALSE)
textplot_wordcloud(dfm_part2_noPunct_noSW)
#Document classification
#Naive Bayes
#We can be interested in knowing the values of precision, recall and accuracy for both alternatives.
library(quanteda) #Required to read a corpus object

dfmat <- dfm(tokens(data_corpus_LMRD)) #50.000 docs x 149.653 feats
#Only uses 106MB RAM

dfmat_train <- dfm_subset(dfmat, set == "train")
dfmat_test <- dfm_subset(dfmat, set == "test")
library(quanteda.textmodels) #For textmodel_nb()
library(caret) #For confusionMatrix()
library(tidyverse)  # for data manipulation
library(dlstats)    # for package download stats
library(pkgsearch)  # for searching packages
library(ROCR)
library(pROC)
library(randomForest)
nbPredictions <- function(dist){ #dist = "multinomial" or "Bernoulli"
  #Compute a nb (Naive Bayes) model
  multi <- textmodel_nb(dfmat_train,
                        dfmat_train$polarity,
                        distribution = dist)
  #Predictions with the model
  pred <- predict(multi, #the computed model
                  newdata = dfmat_test)
  #Compute the confusion matriz for our prediction
  confM <- confusionMatrix(pred, docvars(dfmat_test)$polarity)
  
  #Accuracy is the number of labels (strings) that match...
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$polarity))
  #...divided by the total number of labels
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  my_acc #Sale 0.82876. Con "multinomial" era 0.81304
  #Precision
  precision <- confM$byClass['Pos Pred Value']
  precision #Sale 0.7951591 (antes 0.878327)
  #Specifity
  specificity<-confM$byClass['Neg Pred Value']
  specificity
  #Recall
  recall <- confM$byClass['Sensitivity']
  recall #Sale 0.88568 (antes 0.8953488)
  #f1-measure
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  glm_fit <- glm(docvars(dfmat_test)$polarity ~ pred , family = "binomial", maxit=100)
  fit<-glm_fit$fitted.values
  par(pty = "s") # square
  plotRoc<- roc(dfmat_train$polarity,glm_fit$fitted.values, plot = TRUE, legacy.axes = TRUE,
      percent = TRUE, xlab = "Porcentaje Falsos positivos",
      ylab = "Porcentaje verdaderos postivios", col = "#377eb8", lwd = 2,
      print.auc = TRUE)
  
  list(matrix=confM, acc = my_acc, p = precision, r = recall, e = specificity, f1=f1_val, fit, plotRoc)
}
nbPredictions("multinomial")
nbPredictions("Bernoulli")
#We can see that Bernoulli provides slightly better values than multinomial.
#If i use tf-idf, will get better results?(tf)
  #Like before but using tf-idf
  dfmat <- dfm_tfidf(dfmat, #Over the previous dfmat, compute tf-idf
                     scheme_tf = "prop" #By default it is "count". The good one for TF-IDF is "prop"
  )
dfmat_train <- dfm_subset(dfmat, set == "train")
dfmat_test <- dfm_subset(dfmat, set == "test")
nbPredictions("multinomial")
nbPredictions("Bernoulli")
#SVM Model
#Compute a SVM model
set.seed(123) #Reproducible results
svmPredictions <- function(x, #Sample size
                           weight){ #weight can be "uniform", "docfreq" or "termfreq".
  #Instead of using all the documents marked as train, I take only x documents
  dfmat_train <- dfm_sample(dfm_subset(dfmat, set == "train"),
                            x #Sample size
  )
  dfmat_test <- dfm_subset(dfmat, set == "test")
  multi <- textmodel_svm(dfmat_train,
                         dfmat_train$polarity,
                         weight = weight)
  pred <- predict(multi,
                  newdata = dfmat_test)
  
  confM <- confusionMatrix(pred, docvars(dfmat_test)$polarity)
  #Accuracy is the number of labels (strings) that match...
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$polarity))
  #...divided by the total number of labels
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  my_acc #Sale 0.82876. Con "multinomial" era 0.81304
  #Precision
  precision <- confM$byClass['Pos Pred Value']
  precision #Sale 0.7951591 (antes 0.878327)
  #Specifity
  specificity<-confM$byClass['Neg Pred Value']
  specificity
  #Recall
  recall <- confM$byClass['Sensitivity']
  recall #Sale 0.88568 (antes 0.8953488)
  #f1-measure
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  prediction<-prediction(as.numeric(pred),as.numeric(docvars(dfmat_test)$polarity))
  perf <- performance(prediction, "tpr","fpr")
  plotRoc<-plot(perf)
  list(matrix=confM, acc = my_acc, p = precision, r = recall, e = specificity, f1=f1_val,prediction,perf)
}
svmPredictions(1000, "uniform")
svmPredictions(1000, "docfreq")
svmPredictions(1000, "termfreq")


