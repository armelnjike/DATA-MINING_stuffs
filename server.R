#VRAI
library(shiny)
library(arules)
library(cluster)
library(neuralnet)
library(Matrix)
setwd("/home/thepride/Documents/SEMESTRE7/FOUILLE DE DONNEES/Projet R/FINAL/")

shinyServer(function(input, output){
 # ****RETRIEVE DATASET ************ 
  data <- reactive({
    req(input$fileobs)
    
    ext <- tools::file_ext(input$fileobs$name)
    switch(ext,
           csv = vroom::vroom(input$fileobs$datapath, delim = ","),
           tsv = vroom::vroom(input$fileobs$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  #var <- reactive({input$variables})
  
  output$dataset <- renderTable({
    if(input$showdata){
      head(data(), 300)
    }
    else{ if (input$na) {
      output$test <- renderText({
        which(is.na(data()),arr.ind=TRUE)
       
      })
    }else{
     if(input$viz){
      output$plotvar <- renderPlot({
        boxplot(data())
      })
      }
    }
    }
    }
    ,
    options = list(scrollX = TRUE)
  )
  output$resum <- renderText({
      summary(data())
    
  })
  # FIN AFFICHAGE DES DONNEES **********************
  #c = reactive({input$rpna})
  #if(isTRUE(c)){
    cr <-read.csv("heart.csv")
    #moy = mean(data_nv$chol)
    
  
  # *********************************************************************
  #  *********  TRANSACTION RULES ITEMS FREQUENT ASSOCIATION RULE *******
    library(arules)
    library(tree)
    
 cr$age <- discretize(cr$age, method = "interval", breaks = 5, 
                      labels = c("trentenaire","quarantenaire","cinquantenaire",
                                 "soixantenaire","septentenaire"))
 cr$sex <-  discretize(cr$sex, method = "interval", breaks = 2, 
                       labels = c("feminin","masculin"))
 cr$cp <- discretize(cr$cp, method = "interval", breaks = 4, 
                     labels = c("dolor type 0","dolor type 1","dolor type 2","dolor type 3"))
 cr$trestbps <- cut(cr$trestbps,breaks = c(93,99,120,130,139,159,179,200),
                    labels = c("faible","Optimale","normale","normale haute","degré 1 (hypertention légère)",
                               "degré 2 (hypertention modéré)","degré 3 (hypertention sévère)"))
 # ****** REPERAGE D'INDIVIDUS A UNE CONDITION ********************************
 # 
 
 cr$chol <- discretize(cr$chol, method = "interval", breaks = 9, 
                       labels = c("[126-176]","[176-226]","[226-276]","[276-326]","[326-376]",
                                  "[376-426]","[426-476]","[476-526]","[526-564]"))
 cr$fbs <-  discretize(cr$fbs, method = "interval", breaks = 2, 
                       labels = c("oui","non"))
 
 
 cr$thalach <- discretize(cr$thalach, method = "interval", breaks = 5, 
                          labels = c("[70-97]","[97-123]","[123-146]","[146-179]","[179-202"))
 
 cr$exang <-  discretize(cr$exang, method = "interval", breaks = 2, 
                         labels = c("non","oui"))
 cr$thal <- discretize(as.numeric(as.character(cr$thal)), method = "interval", breaks = 4, 
                       labels = c("normal","thal::detecterCorrigé","défaut reversible corrigé","4"))
 
 cr$target <- discretize(cr$target, method = "interval", breaks = 2, 
                         labels = c("risque bas","risque élevé"))
 

 trans = as(cr, "transactions")
 output$ddd <- renderTable({
   if(!is.null(input$file$name)){
     cr
   }
 })
 
  output$Transactions <- renderPrint({
    inspect(trans)
  })
  
  # *******  PRINT FREQUENTS ITEMSET ***************
  output$frequents <- renderDataTable({
    item_freq <- apriori(trans,parameter = list(supp = as.numeric(input$support), target = "frequent itemsets"))
    inspect(item_freq)})
  
  # **********  PRINT RULES ************************
  output$rule <- renderDataTable({
    rules <- apriori(trans,parameter = list(supp = as.numeric(input$support), conf = as.numeric(input$confiance), target = "rule"))
    inspect(rules)})
  
  # ********************************************************************************
  # ************************  ARBRE DE DECISION  ***********************************
  
  
#  selection=sample(1:nrow(cr),2*nrow(cr)/3)
# apprentissage=cr[selection,] #Donnees d'apprentissage
 # test=cr[-selection,]         #Donnees de test
  
  
  #Debut-----Pour compter les colonnes
  #Training_tree = apprentissage
  #Test_tree = test
  
  
  #Train_f_tree <- data.frame( No= selection, Training_tree)
  #total = 1:155
  
  #n_bar_tree = total[-selection]
  #Test_f_tree = data.frame( No= n_bar_tree, Test_tree) #-----fin
  # ****************************************************
  
  ## 75% of the sample size
  #smp_size <- floor(0.75 * nrow(cr))
  
  ## set the seed to make your partition reproducible
  #set.seed(123)
 # train_ind <- sample(seq_len(nrow(cr)), size = smp_size)
  
  #train <- cr[train_ind, ]
  #test <- cr[-train_ind, ]
  dat <- read.csv("heart.csv",header = TRUE)
  data_tree <- dat
  data_tree$target <- discretize(data_tree$target, method = "interval", breaks = 2, 
                                 labels = c("risque bas","risque élevé"))
  
  #  *********  partitioning data to train and test **************
  set.seed(1234)
  pd <- sample(2,nrow(data_tree),replace = TRUE,prob = c(0.7,0.3))
  train_tree =data_tree[pd==1,]
  test_tree = data_tree[pd==2,]
  # ****tree
  
  
  
  # ***************************************************
  output$dd1 <- renderTable({
    if(!is.null(input$fil$name)){
      dat
    }
  })
  output$traindata <- renderDataTable({train_tree})
  output$TestData <- renderDataTable({test_tree})
  
  # **********************************************************
  # *********** build the decision TREE   ******************
  library(rpart)
  library(rpart.plot)
  library(nnet)
  
  #arbre=rpart(target~.,train)
  arbre <- rpart(target~.+age+sex+cp+trestbps+chol+thalach,train_tree)
  
  output$plottree <- renderPlot({
    rpart.plot(arbre,extra = 2)
  })
  output$dt <- renderPrint({
    print(arbre)
  })
  # ********  prediction  ******************************
  Pred=predict(arbre,newdata=test_tree)
  output$prediction <- renderDataTable({
        
    df <- table(test_tree,Pred)# EUREUR
  })
  
# **************************************************************************
# *******************  RESEAU DE NEURONES  ********************************
  library(neuralnet)
  library(nnet)
  output$dataneural <- renderPrint({
    if(!is.null(input$filen$name)){
    print(str(dat))
    }
  })
  # *************  normalisation des données (min-max) **************
  
  datN <- dat
  
  datN$age <- (datN$age-min(datN$age)) /
    (max(datN$age)-min(datN$age))
  datN$cp <- (datN$cp-min(datN$cp)) /
    (max(datN$cp)-min(datN$cp))
  datN$trestbps <- (datN$trestbps-min(datN$trestbps)) /
    (max(datN$trestbps)-min(datN$trestbps))
  datN$chol <- (datN$chol-min(datN$chol)) /
    (max(datN$chol)-min(datN$chol))
  datN$thalach <- (datN$thalach-min(datN$thalach)) /
    (max(datN$thalach)-min(datN$thalach))
  datN$oldpeak <- (datN$oldpeak-min(datN$oldpeak)) /
    (max(datN$oldpeak)-min(datN$oldpeak))
  datN$slope <- (datN$slope-min(datN$slope)) /
    (max(datN$slope)-min(datN$slope))
  datN$ca <- (datN$ca-min(datN$ca)) /
    (max(datN$ca)-min(datN$ca))
  datN$thal <- (datN$thal-min(datN$thal)) /
    (max(datN$thal)-min(datN$thal))
  
  #  intégrer les noms de colonnes dans une formule. Nous devons combiner les noms de colonne (séparés par un symbole plus),
  # puis ajouter la variable de réponse.
  
  col_list <-  paste(c(colnames(datN[,-c(1,28)])),collapse="+")
  col_list <-  paste(c("target~",col_list),collapse="")
  f <-  formula(col_list)  # une description symbolique du modèle à monter. (|| LISTE DE VARIABLES ||)
  
  # DIVIDE DATA *******
  n <- sample(2,nrow(datN),replace = TRUE,prob = c(0.7,0.3))
  ndataTrain = datN[n==1,]
  nnet_test = datN[n==2,]
  ndataTrain
  nnet_test
  
  
# ************   CONSTRUCTION DU MODELE  NEURONAL********************
  set.seed(7896129)
  nmodel <- neuralnet(f,data = ndataTrain,hidden = 1,threshold = 0.01,learningrate.limit = NULL,
                      learningrate.factor = list(minus = 0.5,plus=1.2),algorithm = "rprop+")
  
  
  observeEvent(input$filen,{
    output$dataneur <- renderTable({
        ndataTrain
      })
   
    output$datant <- renderTable({nnet_test})
   
  })
  output$model <- renderPlot({
    nmodel <- neuralnet(f,data = ndataTrain,hidden = as.integer(input$hidd),threshold = as.numeric(input$treshold),learningrate.limit = NULL,
                        learningrate.factor = list(minus = 0.5,plus=1.2),algorithm = toString(input$algo))
    plot(nmodel)
  })
  
  # **  Predictions **********************
  #sorti <-  compute(nmodel, nnet_test,rep=1)
  
  output$mesures_neuralnet <- renderPrint({
    nmodel <- neuralnet(f,data = ndataTrain,hidden = as.integer(input$hidd),threshold = as.numeric(input$treshold),learningrate.limit = NULL,
                        learningrate.factor = list(minus = 0.5,plus=1.2),algorithm = toString(input$algo))
    sorti <-  compute(nmodel, nnet_test,rep=1)
    summary(sorti)
  })
  
  # *************  Matrice de confusion  **************
  output$Mc_conf <- renderPrint({
    nmodel <- neuralnet(f,data = ndataTrain,hidden = as.integer(input$hidd),threshold = as.numeric(input$treshold),learningrate.limit = NULL,
                        learningrate.factor = list(minus = 0.5,plus=1.2),algorithm = toString(input$algo))
    pred <- predict(nmodel, nnet_test)
    table(nnet_test$target, apply(pred, 1, which.max))
  })
  
# ***********   CLASSIFICATION NON SUPERVISE   **********************
# *******************************************************************
  library(ClusterR)
  library(cluster)
  
  # ******  DATA ***********
  datcl <- dat
  # suppression des categgories
  data_clu <- dat[,-14]
  
  #Ajustement du modèle de clustering K-Means # à l'ensemble de données d'entraînement
  set.seed(240) # Setting seed
  
 
  
  output$rkmeans <- renderPrint({
    kmeans.re <- kmeans(data_clu, centers = as.integer(input$nbcluster), nstart = 20)
    if(!is.null(input$filekkm$name)){
    kmeans.re}
    })
  
  
  #Identification de cluster pour # chaque observation
  
  
    output$obso <- renderPrint({
      kmeans.re <- kmeans(data_clu, centers = as.integer(input$nbcluster), nstart = 20)
      kmeans.re$cluster
        
  })
  
 
  
  # Confusion Matrix
    kmeans.re <- kmeans(data_clu, centers = 3, nstart = 20)
  cm <- table(datcl$target, kmeans.re$cluster)
  output$matconf <- renderPrint({cm})
  
  
  # Model Evaluation and visualization
  #plot(data_clu[c("age", "chol")])
  
  output$evav <- renderPlot({
    kmeans.re <- kmeans(data_clu, centers = as.integer(input$nbcluster), nstart = 20)
    plot(data_clu[c(toString(input$axex), toString(input$axeY))], 
         col = kmeans.re$cluster)
  })
 
  output$evam <- renderPlot({
    kmeans.re <- kmeans(data_clu, centers = as.integer(input$nbcluster), nstart = 20)
    plot(data_clu[c(toString(input$axex), toString(input$axeY))], 
         col = kmeans.re$cluster, 
         main = "K-means with 3 clusters")
  })
  
  
  ## Plotiing cluster centers
  #kmeans.re <- kmeans(data_clu, centers = as.integer(input$nbcluster), nstart = 20)
  kmeans.re$centers
  kmeans.re$centers[, c("age", "chol")]
  
  # cex is font size, pch is symbol
  output$clus <- renderPlot({
    kmeans.re <- kmeans(data_clu, centers = as.integer(input$nbcluster), nstart = 20)
    points(kmeans.re$centers[, c(toString(input$axex), toString(input$axeY))], 
           col = 1:3, pch = 8, cex = 3) 
    
    ## Visualizing clusters
    y_kmeans <- kmeans.re$cluster
    clusplot(data_clu[, c(toString(input$axex), toString(input$axeY))],
             y_kmeans,
             lines = 0,
             shade = TRUE,
             color = TRUE,
             labels = 2,
             plotchar = FALSE,
             span = TRUE,
             main = paste("Cluster heart.csv"),
             xlab = toString(input$axex),
             ylab =  toString(input$axeY))
  })
  
 # **************  END OF SERVER **************************** 
})