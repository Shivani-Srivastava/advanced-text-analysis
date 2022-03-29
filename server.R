#
# Advanced Text Analysis - Using UDPIPE
#
# Parts-of-speech Tagging 
# 
#
library(tools)
library(shiny)
library(htmltools)
library("nametagger")
library(dplyr)
library(udpipe)
library(stringr)

options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output) {

    dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
        else {
            if(file_ext(input$file$datapath)=="txt"){
                Document = readLines(input$file$datapath)
                Doc.id=seq(1:length(Document))
                calib=data.frame(Doc.id,Document)
                return(calib)
            }
            else{
                Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F,encoding="UTF-8")
                
                Document$Doc.id = seq(1:nrow(Document))
                return(Document)
                }
            }
    })
    
    output$sam_data <- DT::renderDataTable({
      datatable(head(dataset()),rownames = FALSE)
    })
    
#    df2 <- x_tabtab %>%
#select(doc_id, token, upos) %>%
#group_by(upos) %>%
#mutate(POS = paste0(token, collapse = ",")) %>%
#distinct(doc_id, POS, .keep_all = TRUE)

    
    text_summ <- reactive({summary(quanteda::corpus(dataset()[,input$y]))})
    quant_mod <- reactive({quanteda::corpus(dataset()[,input$y])})
    
    output$text <- renderUI({
        req(input$file$datapath)
        str1 <- paste("Total no of documets:", nrow(dataset()))
        str2 <- paste("Range of sentences per document: ",min(text_summ()$Sentences),"-",max(text_summ()$Sentences))
        #str3 <- paste("Maximum number of sentence: ",)
        str4 <- paste("Average number of sentences per document: ",round(mean(text_summ()$Sentences),2))
        HTML(paste(str1, str2,str4, sep = '<br/>'))
    })

    output$text2 <- renderUI({
        req(input$file$datapath)
        str2 <- paste("Range of words per document: ",min(text_summ()$Tokens),'-',max(text_summ()$Tokens))
        #str3 <- paste("range of words per document:: ",max(text_summ()$Tokens))
        str4 <- paste("Average number of words: ",round(mean(text_summ()$Tokens),2))
        HTML(paste(str2,str4, sep = '<br/>'))
    })
    
    
    
    
    cols <- reactive({colnames(dataset())})
    
    output$pre_proc1 <- renderUI({if(is.null(dataset())){
        return(NULL)
    }else{
        
        checkboxInput('html',"Remove HTML tags",value = TRUE)
        
    }
    })
    
    output$pre_proc2 <- renderUI({if(is.null(dataset())){
        return(NULL)
    }else{
        checkboxInput('num',"Remove Numbers",value = TRUE)
        
    }
    })
    
    
    output$pre_proc3 <- renderUI({if(is.null(dataset())){
        return(NULL)
    }else{
        checkboxInput('Id077',"Lemmatize",value = TRUE)
        
    }
    })
    
    y_col <- reactive({if(is.null(dataset())){
        return(NULL)
    }else{
        x <- match(input$x,cols())
        y_col <- cols()[-x]
        return(y_col)
    }
    })
    
    output$id_var <- renderUI({
        print(cols())
        selectInput("x","Select ID Column",choices = cols())
    })
    
    
    output$doc_var <- renderUI({
        selectInput("y","Select Text Column",choices = y_col())
    })
    
    dataset1 <- reactive({
        if (is.null(input$file)) { return(NULL) }
        else{
           cleaned_col <- text.clean(dataset()[,input$y],
                                              html_tags = input$html,
                                              numbers = input$num)
        }
        return(cleaned_col)
    })
    
    output$wc <- renderPlot({
        req(input$file$datapath)
        ds <- dataset()
        ds[,'cleaned_col'] <- dataset1()
        
        ds_words <- ds %>%
            unnest_tokens(word, cleaned_col) %>%
            anti_join(stop_words)
        
        count_word <- ds_words %>%
            count(word, sort = TRUE)
        
        # define a nice color palette
        pal <- brewer.pal(8,"Dark2")
        
        # plot the 50 most common words
        count_word%>% 
            with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
    })
    
   dtm <- reactive({
        
       x <- anotated_data()
       #x[,doc_id] <- unique_identifier(x[,input$x])
       dtf <- subset(x, upos %in% input$pos_select)
       dtf <- document_term_frequencies(dtf,document = "doc_id",term = "lemma")
       dtm <- document_term_matrix(x = dtf)
       dtm1 <- as.matrix(dtm)
       dtm2 <- data.frame(Doc.Id = row.names(dtm1),dtm1)
      # print(dtm1[1:5,1:5])
       return(dtm2)
    })
    
    #
    output$dtm_text <- renderText({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            size = dim(dtm())
            dtm_size = paste("DTM has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
        }
       
    })
    
    output$download_dtm <- downloadHandler(
        filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_dtm.csv",collapse = "") },
        content = function(file) {
            new_dtm <- dtm()
            write.csv(new_dtm, file, row.names=F)
        }
    )
    
    
    anotated_data <- reactive({
        if (is.null(input$model)) {return(NULL)}
        else{
            model <- udpipe_load_model(file = input$model$datapath)
            x <- udpipe_annotate(model, x = dataset1(),doc_id = dataset()[,input$x])
            x <- as.data.frame(x)
           # updateProgressBar(session = session, id = "pb4", value = input$slider)
            
        }
        return(x)
        
    })
    
    #ner_model <- nametagger_download_model("english-conll-140408", model_dir = tempdir())
    
    #x00 = reactive({ anotated_data() %>% select(c("doc_id", "sentence_id", "token")) %>% rename(text = token) })
    #entities <- reactive({ predict(ner_model, x00()) %>% filter(., entity != "O")}) # 0.03s
    
    #output$entity_DF <- renderDataTable({
      #req(input$file)
      #as.data.frame(entities())
      #cat('Yay')
    #},
    #options = list(
    #  autoWidth = TRUE,
    #  columnDefs = list(list(width = '200px', targets = "_all"))
    #))
    
    
    # Create a Corpus wise DF of POS distribution
    # D * 5; where D is the number of documents and 5 POS is shown: With the sentence? Without the sentence?
    
    corpus_DF_creation <- reactive({
      if(is.null(input$model)) {return(NULL)}
      else{
        #x <- annotated_data()
        filtered_DF <- select(anotated_data(), c('doc_id','token','upos'))
        df = NULL
        for (i in unique(filtered_DF$doc_id)){
          
          corpus_specific_DF <- filtered_DF %>% filter(filtered_DF$doc_id == i)
          df4 <- corpus_specific_DF %>% select(doc_id, token, upos) %>% group_by(upos) %>% mutate( POS = paste0(token, collapse = ",")) %>% distinct(doc_id, POS, .keep_all = TRUE)
          #print(head(df4))
          df = rbind(df, df4)
          
        }
        df = df %>% select(doc_id, upos, POS)
        return(df)  
      }
    })
    
    build_char_string <- function(df1, upos0){
        a1 = df1 %>% filter(upos == upos0) %>% select(token) %>% 
            distinct(token) %>% # de-duplicate tokens
            as_tibble() %>% # necessary
            as.character() %>% str_c(., sep=", ") %>%
            str_replace_all(., "\"","") %>% str_replace(., "c", "") %>% 
            str_replace_all(., "[()]","")
        return(a1) }

    
    
    output$corpus_DF <- DT::renderDataTable({
      req(input$file)
        
       df0 = anotated_data() %>% select(., c('doc_id','token','upos')) %>%
            filter(upos %in% c("PROPN", "NOUN", "VERB", "ADJ")) 
        
       a0 = unique(df0$doc_id)
       
       df_out = data.frame(doc_id = a0, PROPN = character(length(a0)), 
                     NOUN = character(length(a0)), VERB = character(length(a0)), 
                     ADJ = character(length(a0)))  

          
        for (i0 in 1:length(a0)){  
  
            docid0 = a0[i0]
            df1 = df0 %>% filter(doc_id == docid0) 
  
            df_out0 = data.frame(doc_id = docid0, 
                       PROPN = build_char_string(df0, "PROPN"), 
                       NOUN = build_char_string(df0, "NOUN"), 
                       VERB = build_char_string(df0, "VERB"), 
                       ADJ = build_char_string(df0, "ADJ"))
  
            df_out[i0, ] = df_out0
  
    } 
       
      #dat <- corpus_DF_creation() %>% group_by(doc_id) %>% select(doc_id, upos, POS) %>% filter(upos == "NOUN" | upos == "ADV" | upos == "VERB" | upos == "ADJ" | upos == "PROPN")
      # aa = df_null  %>% 
      df_out
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '200px', targets = "_all"))
    ))
    
    output$download_corpus <- downloadHandler(
      filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_DF.csv",collapse = "") },
      content = function(file) {
        new_dtm <- corpus_DF_creation()
        write.csv(new_dtm, file, row.names=F)
      }
    )
    
    #Code to Merge Rows in Sentence wise DataFrame
    
    output$an_df <- DT::renderDataTable({
      req(input$file)
      dat <- anotated_data()%>%filter(doc_id==input$d_sel & sentence_id==input$s_sel)
      
      dtable <- datatable(dat, rownames = FALSE, 
                          options = list(
                            rowsGroup = list(0,1,2,3) # merge cells of column 1
                          ))
      path <- "www" # folder containing dataTables.rowsGroup.js
      dep <- htmltools::htmlDependency(
        "RowsGroup", "2.0.0", 
        path, script = "dataTables.rowsGroup.js")
      dtable$dependencies <- c(dtable$dependencies, list(dep))
      dtable
      
    })
    
    # Select variables:
    #output$pos_select_ui <- renderUI({
       # if (is.null(input$file)) { return(NULL) }
       # else{
            
            # radioButtons("pos_select", "Display most frequent",
            #                    choiceNames = 
            #                        list('Noun', 'Verb','Adjective', 'Adverb',"Proper Noun"),
            #                    choiceValues =
            #                        list("NOUN", "VERB", "ADJ", "ADV","NNPS")
            # )
            
        #    checkboxGroupInput("pos_select",
         #                      "Choose most frequent (For Word Cloud)",
          #                     choiceNames = list("Noun","Verb","Adjective","Adverb","Proper Noun"),
           #                    choiceValues = list("NOUN","VERB","ADJ","ADV","NNPS"),selected = "NOUN")
        #}})
    
    stopw <- reactive({
        stpw <- unlist(strsplit(input$stopw,","))
        return(stpw)
    })

    output$pos_plot <- renderPlot({
        
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        
        else{
            #if(input$pos_select=="NNPS"){
               # stats <- subset(anotated_data(), xpos %in% input$pos_select)
               
           # }else{
                choices <- paste(input$pos_select,collapse = ",")
                stats <- subset(anotated_data(), upos %in% input$pos_select)
                
           # }
            
            if(input$Id077){
                stats <- txt_freq(stats$lemma)
            }else{
                stats <- txt_freq(stats$token)   
            }
            
            
            if(length(stopw()!=0)){
            stats <- stats %>%
                    select(key, freq, freq_pct) %>% 
                    filter(!key %in% stopw())
            }
            
            stats$key <- factor(stats$key, levels = rev(stats$key))
            
            barchart(key ~ freq, data = head(stats, input$pos_slider), col = "cadetblue", 
                     main = paste0("Top 20 Most occurring"," ",choices), xlab = "Freq")
        }
        
    })
    

    #-----code for document level analysis---#
    
    output$doc_sel <- renderUI({
        choices <- as.numeric(unique(anotated_data()$doc_id))
        selectInput('d_sel',label = "Select Document",choices = choices,multiple = F)
    })
    
    output$sent_sel <- renderUI({
            
            req(input$d_sel)
            t <- anotated_data()%>%filter(doc_id==as.numeric(input$d_sel))
            max_sent <- max(t$sentence_id)
            selectInput('s_sel',label = "Select sentence",choices = 1:max_sent,multiple = F)
    })
    
    output$sel_sent1 <- renderText({
        req(input$d_sel)
        temp <- anotated_data()%>%filter(doc_id==input$d_sel & sentence_id==input$s_sel)
        unique(temp$sentence)
    })
    
    output$dep_tre <- renderPlot({
        req(input$d_sel)
        sent_annotation(anotated_data(),doc_num = input$d_sel,sent_num = input$s_sel,size=input$size_sel)
    })
    
    
    
    #--------#
    
    
    output$a_table <- DT::renderDataTable({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            summ_table <- as.data.frame(table(anotated_data()$upos))
            colnames(summ_table) <- c("POS TAG", "Unique Count")
            summ_table <- replace_abb(summ_table)
            
            #----added code---#
            x <- anotated_data()
            # collect all upos tokens, deduplicated
            a0 = anotated_data()$upos %>% unique; a0
            stor_list = vector(mode="list", length=length(a0))
            for (i0 in 1:length(a0)){
              stor_list[[i0]] = x %>% filter(upos == a0[i0]) %>% 
                select(token) %>% unique() %>% as.list() %>%
                unlist() %>% as.character() %>% str_c(., collapse=", ")
            }
            
            a2=stor_list %>% as.data.frame(); dim(a2)
            a3=str_replace_all(a2, "\\.\\.", ", "); a3[1]
            a1 = data.frame(upos = a0, tokens = a3); dim(a1)
            
            dict <- c('ADJ'= 'adjective',
                      'ADP'= 'adposition',
                      'ADV'= 'adverb',
                      'AUX'= 'auxiliary',
                      'CCONJ'= 'coordinating conjunction',
                      'DET'= 'determiner',
                      'INTJ'= 'interjection',
                      'NOUN'= 'noun',
                      'NUM'= 'numeral',
                      'PART'= 'particle',
                      'PRON'= 'pronoun',
                      'PROPN'= 'proper noun',
                      'PUNCT'= 'punctuation',
                      'SCONJ'= 'subordinating conjunction',
                      'SYM'= 'symbol',
                      'VERB'= 'verb',
                      'X'= 'other')
            
            a4 = as.data.frame(dict)
            a5 = data.frame(upos = rownames(a4), desc = a4); a5
            rownames(a5) = NULL; a5
            outp = dplyr::left_join(a5, a1, by="upos")
            outp1 <- outp %>% left_join(summ_table,by = c("dict" = "POS TAG"))
            datatable(outp1,rownames = FALSE)
        }
        
    },options = list(pageLength = 5))
    
    
    
    output$word_cloud <- renderPlot({
        
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            if(input$pos_select=="NNPS"){
                stats <- subset(anotated_data(), xpos %in% input$pos_select)
            }else{
                stats <- subset(anotated_data(), upos %in% input$pos_select)
                
            }
            if(input$Id077){
                stats <- txt_freq(stats$lemma)
            }else{
                stats <- txt_freq(stats$token)   
            }
            
            
            if(length(stopw()!=0)){
                stats <- stats %>%
                    select(key, freq, freq_pct) %>% 
                    filter(!key %in% stopw())
            }
            
            stats$key <- factor(stats$key, levels = rev(stats$key))
            
            wordcloud(words = stats$key, 
                      freq = stats$freq, 
                      min.freq = input$min_freq, 
                      max.words = input$max_word,
                      random.order = FALSE, 
                      colors = brewer.pal(6, "Dark2"),
                      scale=c(5,0.5))
        }
        
    })
    
    #---Keyword tab code
    
    output$ext_df <- DT::renderDataTable({
      if (is.null(input$file) | is.null(input$model)) { return(NULL) }
      else{
        if(input$key_algo=="RAKE"){
          stats <- keywords_rake(x = anotated_data(), term = "lemma", group = "doc_id", 
                                 relevant = anotated_data()$upos %in% c("NOUN", "ADJ"))
          
          stats<- stats %>%mutate_if(is.numeric,round,digits = 2)
          stats <- subset(stats, freq > input$min_freq )
          return(datatable(stats,rownames = FALSE))
          # stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
          # p1 <- barchart(key ~ rake, data = head(subset(stats, freq > 3), input$key_slider), col = "red", 
          #                main = "Top Keywords identified by RAKE", 
          #                xlab = "Rake")
          #print(p1)
        }
        if(input$key_algo=="Noun-Verb Phrase"){
          phrase_tags <- as_phrasemachine(anotated_data()$upos, type = "upos")
          stats <- keywords_phrases(x = phrase_tags, term = tolower(anotated_data()$token),
                                    pattern = "(A|N)*N(P+D*(A|N)*N)*",
                                    is_regex = TRUE, detailed = FALSE)
          stats <- subset(stats, ngram > 1 & freq > input$min_freq)
          return(datatable(stats,rownames = FALSE))
          # stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
          # p2<- barchart(key ~ freq, data = head(stats, input$key_slider), col = "magenta",
          #               main = "Keywords - simple noun phrases", xlab = "Frequency")
          # print(p2)
          
        }
      }
      
    })
    
    
    output$key_plot <- renderPlot({
        if (is.null(input$file) | is.null(input$model)) { return(NULL) }
        else{
            if(input$key_algo=="RAKE"){
                stats <- keywords_rake(x = anotated_data(), term = "lemma", group = "doc_id", 
                                       relevant = anotated_data()$upos %in% c("NOUN", "ADJ"))
                stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                p1 <- barchart(key ~ rake, data = head(subset(stats, freq > input$min_freq), input$key_slider), col = "red", 
                               main = "Top Keywords identified by RAKE", 
                               xlab = "Rake")
                print(p1)
            }
            if(input$key_algo=="Noun-Verb Phrase"){
                phrase_tags <- as_phrasemachine(anotated_data()$upos, type = "upos")
                stats <- keywords_phrases(x = phrase_tags, term = tolower(anotated_data()$token),
                                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                                          is_regex = TRUE, detailed = FALSE)
                stats <- subset(stats, ngram > 1 & freq > input$min_freq)
                stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                p2<- barchart(key ~ freq, data = head(stats, input$key_slider), col = "magenta",
                              main = "Keywords - simple noun phrases", xlab = "Frequency")
                print(p2)
                
            }
        }
        
    })
    
})
