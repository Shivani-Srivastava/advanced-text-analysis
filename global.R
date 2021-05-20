replace_abb <- function(x){
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
  x[,1] <- stringr::str_replace_all(string = x[,1],
                                       pattern= dict)
  return(x)
}

text.clean = function(x,html_tags,numbers)                    # text data
{ #require("tm")
  x  = iconv(x, 'UTF-8', 'ASCII')
  # x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  #x  =  tolower(x)
  if(html_tags){
    x  =  gsub("<.*?>", " ", x)             # regex for removing HTML tags
  }
  if(numbers){
    x  =  removeNumbers(x) 
  }# convert to lower case characters
  
  #x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
  
  
  # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

# code for summary stats
summ_stats <- function(doc,text_col){
  my_corpus <- quanteda::corpus(doc[,text_col])
  return(summary(my_corpus))
}


library(igraph)
library(ggraph)
library(ggplot2)

# define func plot_annotation()
plot_annotation <- function(x, size = 3){
  
  # check if input is of right format
  stopifnot(is.data.frame(x) & all(c("sentence_id", "token_id", "head_token_id", "dep_rel",
                                     "token_id", "token", "lemma", "upos", "xpos", "feats") %in% colnames(x)))
  
  # map node and edge relations
  x <- x[!is.na(x$head_token_id), ]
  x <- x[x$sentence_id %in% min(x$sentence_id), ]
  
  edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
  edges$label <- edges$dep_rel
  
  # invoking igraph functionality
  g <- graph_from_data_frame(edges,
                             vertices = x[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
                             directed = TRUE)
  
  # prettifying further with ggraph
  ggraph(g, layout = "linear") +
    
    geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                  arrow = grid::arrow(length = unit(1.5, 'mm'), ends = "last", type = "closed"),
                  end_cap = ggraph::label_rect("wordswordswords"),
                  label_colour = "red", check_overlap = TRUE, label_size = size) +
    
    geom_node_label(ggplot2::aes(label = token), col = "darkgreen", size = size, fontface = "bold") +
    
    geom_node_text(ggplot2::aes(label = upos), nudge_y = -0.35, size = size) +
    
    theme_graph(base_family = "Arial Narrow") +
    
    labs(title = "", subtitle = "")
}


sent_annotation <- function(an_df,doc_num,sent_num,size){
  x1 <- an_df %>% filter(doc_id==doc_num & sentence_id==sent_num)
  plot_annotation(x1, size = size)
}
