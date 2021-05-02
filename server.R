#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load Relevant Libraries
library(shiny)
library(tm)
library(RWeka)
library(textcat)
library(ggplot2)
library(stringi)
library(gridExtra)
library(data.table)
library(dplyr)
library(rsconnect)

#Set Seed
set.seed(371293)

#Function to Clean Data of Irrelevant Formats and Characters
cleaning_func <- function(clean){
    spacing <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    cleaned <- tm_map(clean, spacing, "(f|ht)tp(s?)://(.*)[.][a-z]+")
    cleaned <- tm_map(cleaned, spacing, "@[^\\s]+")
    cleaned <- tm_map(cleaned, tolower)
    cleaned <- tm_map(cleaned, removeWords, stopwords("en"))
    cleaned <- tm_map(cleaned, removePunctuation)
    cleaned <- tm_map(cleaned, removeNumbers)
    cleaned <- tm_map(cleaned, stripWhitespace)
    cleaned <- tm_map(cleaned, PlainTextDocument)
    return(cleaned)
}

#Calculate Counts of Words and Patterns of Words
options(mc.cores=1)

calc_freq <- function(cleaned) {
    wordfreq <- sort(rowSums(as.matrix(cleaned)), decreasing = TRUE)
    return(data.frame(word = names(wordfreq), wordfreq = wordfreq))
}

word_pairs <- function(x)NGramTokenizer(x, Weka_control(min=2, max=2))
word_triples <- function(x)NGramTokenizer(x, Weka_control(min=3, max=3))

freq_plot <- function(freq, label) {
    ggplot(freq[1:25,], aes(reorder(word, -wordfreq), wordfreq))+
        labs(x = label, y = "Word Count")+
        theme(axis.text.x = element_text(angle = 60, size = 8, hjust = 1))+
        geom_bar(stat="identity", fill = I("black"))
}


#Calculate tables with related word groupings
ngrm_tbl <- function(w){
    s <- strsplit(as.character(w$word), " ")
    w$mnsgrm <- NA
    w$lstwrd <- NA
    for (l in 1:nrow(w)){
        wrd <- vector()
        for (k in 1:(length(s[[l]])-1)){
            wrd <- c(wrd, s[[l]][k])
        }
        w$mnsgrm[l] <- paste(wrd, collapse = " ")
        w$lstwrd[l] <- tail(s[[l]],1)
    }
    return(as.data.frame(w, row.names = NULL, stringsAsFactors = FALSE))
}

#Function for calling the next word based off selected tables
prd_nxt_wrd <- function(x,y,z){
    t <- tolower(x)
    q <- paste(tail(unlist(strsplit(t,' ')),2), collapse=" ")
    r <- paste(tail(unlist(strsplit(t,' ')),1), collapse=" ")
    if(stri_count_words(x)==2){
        if (q %in% y$mnsgrm){
            w <- y %>% filter(mnsgrm==q) %>% .$lstwrd
            return(w[1])
            } else if (r %in% z$mnsgrm==q){
                w <- z %>% filter(mnsgrm==q) %>% .$lstwrd
                return(w[1])
                } else {return('no prediction from input')}
        } else if (stri_count_words(x)==1){
            if (r %in% z$mnsgrm){
                w <- z %>% filter(mnsgrm==q) %>% .$lstwrd
                return(w[1])
                } else {return('no prediction from input')}
            } else {print('no prediction from input')}
    }

#set input file size
options(shiny.maxRequestSize = 999*1024^2)

#rsconnect::configureApp("Capstone_Project_Prediction_Tool", size = "large")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
#Input first data sample and complete cleaning/preprocessing
        rw_txt_1 <- reactive({
            req(input$file1)
            rwdt_1 <- input$file1
            if (is.null(rwdt_1)){
                return(NULL)
            }else{
                tmp <-readLines(rwdt_1$datapath)
                Encoding(tmp)<-"UTF-8"
                return(gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp))
            }
        })
        rw_txt_2 <- reactive({
            req(input$file2)
            rwdt_2 <- input$file2
            if (is.null(rwdt_2)){
                return(NULL)
            }else{
                tmp <-readLines(rwdt_2$datapath)
                Encoding(tmp) <- "UTF-8"
                return(gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp))
            }
        })
        rw_txt_3 <- reactive({
            req(input$file3)
            rwdt_3 <- input$file3
            if (is.null(rwdt_3)){
                return(NULL)
            }else{
                tmp <-readLines(rwdt_3$datapath)
                Encoding(tmp) <- "UTF-8"
                return(gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp))
            }
        })
            
#Code intially designed to take any file size, but rpubs caps memory uses. With more memory allocated to the prcoesses, the following code could be used to sample the data inside the tool        
#        smpl_txt_1 <- reactive({
#            sample(rw_txt_1(), length(rw_txt_1())*0.01)
#            })
#        smpl_txt_2 <- reactive({
#            sample(rw_txt_2(), length(rw_txt_2())*0.01)
#        })
#        smpl_txt_3 <- reactive({
#            sample(rw_txt_3(), length(rw_txt_3())*0.01)
#        })
        
#Calculate size of file        
        rw_txt_1_sz <- reactive({
            rwdt_1 <- input$file1
            if (is.null(rwdt_1)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_1$datapath)
                return(object.size(tmp))
            }
        })
        rw_txt_2_sz <- reactive({
            rwdt_2 <- input$file2
            if (is.null(rwdt_2)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_2$datapath)
                return(object.size(tmp))
            }
            
        })
        rw_txt_3_sz <- reactive({
            rwdt_3 <- input$file3
            if (is.null(rwdt_3)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_3$datapath)
                return(object.size(tmp))
            }
        })

#Count Words in sample        
        rw_txt_1_cnt <- reactive({
            rwdt_1 <- input$file1
            if (is.null(rwdt_1)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_1$datapath)
                Encoding(tmp) <- "UTF-8"
                gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp)
                return(sum(stri_count_words(tmp)))
            }
        })
        rw_txt_2_cnt <- reactive({
            rwdt_2 <- input$file2
            if (is.null(rwdt_2)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_2$datapath)
                Encoding(tmp) <- "UTF-8"
                gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp)
                return(sum(stri_count_words(tmp)))
            }
        })
        rw_txt_3_cnt <- reactive({
            rwdt_3 <- input$file3
            if (is.null(rwdt_3)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_3$datapath)
                Encoding(tmp) <- "UTF-8"
                gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp)
                return(sum(stri_count_words(tmp)))
            }
        })
#Count mean number of words in each entry in sample        
        rw_txt_1_mn <- reactive({
            rwdt_1 <- input$file1
            if (is.null(rwdt_1)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_1$datapath)
                Encoding(tmp) <- "UTF-8"
                gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp)
                return(mean(stri_count_words(tmp)))
            }
        })
        
        rw_txt_2_mn <- reactive({
            rwdt_2 <- input$file2
            if (is.null(rwdt_2)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_2$datapath)
                Encoding(tmp) <- "UTF-8"
                gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp)
                return(mean(stri_count_words(tmp)))
            }
        })
        
        rw_txt_3_mn <- reactive({
            rwdt_3 <- input$file3
            if (is.null(rwdt_3)){
                return(NULL)
            }else{
                tmp <- readLines(rwdt_3$datapath)
                Encoding(tmp) <- "UTF-8"
                gsub("[^[:alnum:][:blank:]?&/\\-]", "", tmp)
                return(mean(stri_count_words(tmp)))
            }
        })
#Produce Summary of Source Sample Characteristics        
        dt_smmry <- reactive({
            data.frame(source = c("Source I", "Source II", "Source III"),
                       file_size_kb = c(rw_txt_1_sz(), rw_txt_2_sz(), rw_txt_3_sz()),
                       Lines = c(length(rw_txt_1()), length(rw_txt_2()), length(rw_txt_3())),
                       Words = c(rw_txt_1_cnt(), rw_txt_2_cnt(), rw_txt_3_cnt()),
                       Words_Per_Entry = c(rw_txt_1_mn(), rw_txt_2_mn(), rw_txt_3_mn()))

        })
 #Create Complete Sample of all the Source Samples       
        smpl_dt <- reactive({
            c(rw_txt_1(), rw_txt_2(), rw_txt_3())
        })
#Reformat and clean the source samples and complete data sample        
        rw_txt_1_cln <- reactive({
            VCorpus(VectorSource(rw_txt_1()))
        })
        
        rw_txt_1_clnd <- reactive({
            cleaning_func(rw_txt_1_cln())
        })
        
        txt_1_1wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_1_clnd()), 0.99))
        })
        
        txt_1_2wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_1_clnd(), control = list(tokenize = word_pairs)), 0.999))
        })
        
        txt_1_3wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_1_clnd(), control = list(tokenize = word_triples)), 0.9999))
        })
        rw_txt_2_cln <- reactive({
            VCorpus(VectorSource(rw_txt_2()))
        })
        
        rw_txt_2_clnd <- reactive({
            cleaning_func(rw_txt_2_cln())
        })
        
        txt_2_1wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_2_clnd()), 0.99))
        })
        
        txt_2_2wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_2_clnd(), control = list(tokenize = word_pairs)), 0.999))
        })
        
        txt_2_3wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_2_clnd(), control = list(tokenize = word_triples)), 0.9999))
        })
        rw_txt_3_cln <- reactive({
            VCorpus(VectorSource(rw_txt_3()))
        })
        
        rw_txt_3_clnd <- reactive({
            cleaning_func(rw_txt_3_cln())
        })
        
        txt_3_1wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_3_clnd()), 0.99))
        })
        
        txt_3_2wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_3_clnd(), control = list(tokenize = word_pairs)), 0.999))
        })
        
        txt_3_3wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(rw_txt_3_clnd(), control = list(tokenize = word_triples)), 0.9999))
        })
        smpl_dt_cln <- reactive({
            VCorpus(VectorSource(smpl_dt()))
        })
        
        smpl_dt_clnd <- reactive({
            cleaning_func(smpl_dt_cln())
        })
        
        txt_dt_1wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(smpl_dt_clnd()), 0.99))
        })
        
        txt_dt_2wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(smpl_dt_clnd(), control = list(tokenize = word_pairs)), 0.999))
        })
        
        txt_dt_3wrd <- reactive({
            calc_freq(removeSparseTerms(TermDocumentMatrix(smpl_dt_clnd(), control = list(tokenize = word_triples)), 0.9999))
        })
#Generate Plots of Source Samples and Complete Sample        
        txt_1_plt_1wrd <- reactive({
            freq_plot(txt_1_1wrd(), "25 Top Common Words in Sample Text 1")
        })
        
        txt_1_plt_2wrd <- reactive({
            freq_plot(txt_1_2wrd(), "25 Top Common Word Pairs in Sample Text 1")
        })
        
        txt_1_plt_3wrd <- reactive({
            freq_plot(txt_1_3wrd(), "25 Top Common Word Triples in Sample Text 1")
        })
        
        
        txt_2_plt_1wrd <- reactive({
            freq_plot(txt_2_1wrd(), "25 Top Common Words in Sample Text 2")
        })
        
        txt_2_plt_2wrd <- reactive({
            freq_plot(txt_2_2wrd(), "25 Top Common Word Pairs in Sample Text 2")
        })
        
        txt_2_plt_3wrd <- reactive({
            freq_plot(txt_2_3wrd(), "25 Top Common Word Triples in Sample Text 2")
        })
        
        txt_3_plt_1wrd <- reactive({
            freq_plot(txt_3_1wrd(), "25 Top Common Words in Sample Text 3")
        })
        
        txt_3_plt_2wrd <- reactive({
            freq_plot(txt_3_2wrd(), "25 Top Common Word Pairs in Sample Text 3")
        })
        
        txt_3_plt_3wrd <- reactive({
            freq_plot(txt_3_3wrd(), "25 Top Common Word Triples in Sample Text 3")
        })
        
        txt_dt_plt_1wrd <- reactive({
            freq_plot(txt_dt_1wrd(), "25 Top Common Words in Compiled Data Sample")
        })
        
        txt_dt_plt_2wrd <- reactive({
            freq_plot(txt_dt_2wrd(), "25 Top Common Word Pairs in Compiled Sample Data")
        })
        
        txt_dt_plt_3wrd <- reactive({
            freq_plot(txt_dt_3wrd(), "25 Top Common Word Triples in Compiled Sample Data")

        })
#Calculate Related Word Tables        
        txt_1_2wrd_tbl <- reactive({
            ngrm_tbl(txt_1_2wrd())
        })
        
        txt_1_3wrd_tbl <- reactive({
            ngrm_tbl(txt_1_3wrd())
        })
        
        txt_2_2wrd_tbl <- reactive({
            ngrm_tbl(txt_2_2wrd())
        })
        
        txt_2_3wrd_tbl <- reactive({
            ngrm_tbl(txt_2_3wrd())
        })
        
        txt_3_2wrd_tbl <- reactive({
            ngrm_tbl(txt_3_2wrd())
        })
        
        txt_3_3wrd_tbl <- reactive({
            ngrm_tbl(txt_3_3wrd())
        })
        
        txt_dt_2wrd_tbl <- reactive({
            ngrm_tbl(txt_dt_2wrd())
        })
        
        txt_dt_3wrd_tbl <- reactive({
            ngrm_tbl(txt_dt_3wrd())
        })
#Generate Selection for Source Table 1 (WORD TRIPLETS)        
       srcinput <- reactive({
           switch(input$table,
                  "Text 1" = txt_1_3wrd_tbl(),
                  "Text 2" = txt_2_3wrd_tbl(),
                  "Text 3" = txt_3_3wrd_tbl(),
                  "Complete Data" = txt_dt_3wrd_tbl()
                  )
           
       })
#Generate Selection for Source Table 2 (WORD PAIRS)       
       srctbl2 <- reactive({
           switch(input$table2,
                  "Text 1" = txt_1_2wrd_tbl(),
                  "Text 2" = txt_2_2wrd_tbl(),
                  "Text 3" = txt_3_2wrd_tbl(),
                  "Complete Data" = txt_dt_2wrd_tbl()
           )
       })
#Generate Plot Selection        
        graphinput <- reactive({
            switch(input$graph,
                   "Text 1 Single Words" = txt_1_plt_1wrd(),
                   "Text 1 Word Pairs" = txt_1_plt_2wrd(),
                   "Text 1 Word Triples" = txt_1_plt_3wrd(),
                   "Text 2 Single Words" = txt_2_plt_1wrd(),
                   "Text 2 Word Pairs" = txt_2_plt_2wrd(),
                   "Text 2 Word Triples" = txt_2_plt_3wrd(),
                   "Text 3 Single Words" = txt_3_plt_1wrd(),
                   "Text 3 Word Pairs" = txt_3_plt_2wrd(),
                   "Text 3 Word Triples" = txt_3_plt_3wrd(),
                   "Complete Text Sample Single Words" = txt_dt_plt_1wrd(),
                   "Complete Text Sample Word Pairs" = txt_dt_plt_2wrd(),
                   "Complete Text Sample Word Triples" = txt_dt_plt_3wrd()
                   )
        })
#Create User Text Input        
            updateTextInput(session, "text", value = ("new text"))
        
#Generate Outputs        
    output$selected_graph <- renderPlot({ graphinput()
        
    })
    output$smmry_tbl <- renderTable ({ dt_smmry()
    })
    output$selected_source <- renderTable({
        srcinput()
    })
    output$selected_source2 <- renderTable({
        srctbl2()
    })
    output$text <- renderPrint({ prd_nxt_wrd(input$text, srcinput(), srctbl2())
        
    })
    
})