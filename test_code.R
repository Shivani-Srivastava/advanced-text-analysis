nokia = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')

Document = read.csv('F:/amazon_laptop_reviews_4.csv' ,header=TRUE, sep = ",", stringsAsFactors = F,encoding="UTF-8")

laptop <- read.csv('E:\\Eruditis BA\\Capstone\\Dataset\\laptop_sales.csv')


Document<-Document[complete.cases(Document), ]
Document$Doc.ID = seq(1:nrow(Document))

Document = readLines("data/Nokia_Lumia_reviews.txt")
Doc.id=seq(1:length(Document))
calib=data.frame(Doc.id,Document)

clean_doc <- text.clean(Document$review_title,TRUE,TRUE)

#english_model = udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")  # file_model only needed
model <- udpipe_download_model(language= "hindi")

udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')

x <- udpipe_annotate(udmodel_english, x = Document$review_title,doc_id = Document$review_id,provide_lemma=0) #%>% as.data.frame() %>% head()
x <- as.data.frame(x)
#   })  # 13.76 secs

# str(x);  
head(x, 4)

## NOUNS
stats <- subset(x, upos %in% c("NOUN","ADJ")) 
stats <- txt_freq(stats$token)


stats <- stats %>%
  select(key, freq, freq_pct) %>% 
  filter(!key %in% c("Phone", "Windows", "phone"))
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

library(wordcloud)
wordcloud(words = stats$key, 
          freq = stats$freq, 
          min.freq = 5, 
          max.words = 100,
          random.order = FALSE, 
          colors = brewer.pal(6, "Dark2"),
          scale=c(10,0.5)
        )




#------------------ keyword extraction---------------#
## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

#----Noun verb phrase------------#
## Using a sequence of POS tags (noun phrases / verb phrases)

phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

#-----------Trying out other language---#

dl <- udpipe_download_model(language = "sanskrit", udpipe_model_repo = "jwijffels/udpipe.models.ud.2.0")
udmodel_sanskrit <- udpipe_load_model(file = dl$file_model)
txt <- "ततः असौ प्राह क्षत्रियस्य तिस्रः भार्या धर्मम् भवन्ति तत् एषा कदाचिद् वैश्या सुता भविष्यति तत् अनुरागः ममास्याम् ततः रथकारः तस्य निश्चयम् विज्ञायावदत् वयस्य किम् अ धुना कर्तव्यम् कौलिकः आह किम् अहम् जानामि त्वयि मित्रे यत् अभिहितं मया ततः"
x <- udpipe_annotate(udmodel_sanskrit, x = txt)
Encoding(x$conllu)
x <- as.data.frame(x)


#------list language model---#
shinyWidgets::shinyWidgetsGallery()

as.data.frame(table(x$upos)) # UD based postags




puncts = c(",", "\\.", "!", "\\?");

text.clean = function(x,html_tags,numbers,lower)                    # text data
{ #require("tm")
  x  = iconv(x, 'UTF-8', 'ASCII')
  # x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  if(lower){
    x  =  tolower(x)
  }
  
  if(html_tags){
    x  =  gsub("<.*?>", " ", x)             # regex for removing HTML tags
  }
  if(numbers){
    x  =  removeNumbers(x) 
  }# convert to lower case characters
  
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
  
  
  # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}


t = text.clean(calib,html_tags = TRUE,numbers = TRUE)



