#VRAI
library(shiny)
library(arules)
library(cluster)
library(nnet)

ui <- fluidPage(
  # ENTETE
  fluidRow(column(6,offset =3 ,h1("TP DE FOUILLE DE DONNEES")),column(3,offset = 9,"****presenter par**** :",
                                                                      fluidRow("MATRICULE|------NAME"),
                                                                      fluidRow("21S2802|_______NYA NJIKE ARMEL"),
                                                                      fluidRow(". . . . . . . .|_______TAKOU JOEL"),
                                                                      fluidRow(". . . . . . . .|_______??????????"))),
    
      navlistPanel(
        id = "tabset",
        
        tabPanel("OBSERVATIONS SUR LES DONNEES", "CARACTERISTIQUES DES DONNEES D'ETUDES",
                 tabsetPanel(
                   tabPanel("Import data", 
                            column(3,fileInput("fileobs", "Data", buttonLabel = "Upload...",accept = c(
                              ".csv","text/csv"
                              ),multiple = FALSE),
                              checkboxInput("showdata","print dataset",value = FALSE),
                              fluidRow(strong("NA values & outliers"),checkboxInput("na","show NA",value = FALSE),
                                       checkboxInput("rpna","Replace outier ",value = FALSE)
                                       ,div(style="background-color:gray"))
                              ,
                              # SELECT VARIABLE *******************************************************
                              checkboxInput("viz","visualiser",value = FALSE),
                              numericInput("rows", "Rows to preview", 10, min = 1))
                            ,
                            column(9,
                                   # donnes pour les data********************
                                   fluidRow(
                                   tableOutput("dataset"),
                                   textOutput("test"),
                                   plotOutput("plotvar")))
                            
                   ),
                   tabPanel("Observations",
                  # oBSERVA TIONS SUR LES DONNES
                      textOutput("resum")
                            )
                 )),
        # *****************************************************************************************
        tabPanel("ASSOCIATION RULES", "REGLES D'ASSOCIATION",
            tabsetPanel(
                tabPanel("Import data",
                   # *********** column ******************
                   column(3,fileInput("file", "Data", buttonLabel = "Upload...",accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),
                     fluidRow(strong("PARAMETRES")),
                    
                        numericInput("support","entrrez la valeur du support",
                                           value=0.4,min = 0.2,max = 1,step = 0.05),
                         numericInput("confiance","entrrez la valeur de la confiance",
                                           value=0.4,min = 0.2,max = 1,step = 0.05),
                         checkboxInput("showtrans","Afficher les transactions",value = FALSE),
                         checkboxInput("showrules","Afficher les regles",value = FALSE)
                         
                      
                     # *********** column ******************
                     )
                   ,
                   column(9,tableOutput("ddd"))
                   
              ),
          tabPanel("transactions",
                  
                          # donnes pour les data********************
                          fluidRow(
                            verbatimTextOutput("Transactions")
                            )
                   ),
          tabPanel("Items frequents",
                   dataTableOutput("frequents")
          )
          ,
                            
          tabPanel("regles",
                   dataTableOutput("rule")
          ),
                   
          tabPanel("Observations")
        )
        ),
        # *****************************************************************************************
        
        tabPanel("DECISION/CLASSIFICATION TREE", "DECISION/CLASSIFICATION TREE",
                 tabsetPanel(
                   tabPanel("Import data", 
                            column(3,fileInput("fil", "Data", buttonLabel = "Upload...",accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                              fluidRow(strong("PARAMETRES")),
                              textInput("delim", "Delimiter (leave blank to guess)", ""),
                              numericInput("skip", "Rows to skip", 0, min = 0),
                              numericInput("rows", "Rows to preview", 10, min = 1))
                            ,
                            column(9,tableOutput("dd1"))
                            
                   ),
                   tabPanel("Trainingset data", dataTableOutput("traindata")),
                   tabPanel("Test Data", dataTableOutput("TestData")),
                   tabPanel("Print tree", plotOutput("plottree") ),
                   tabPanel("resume", verbatimTextOutput("dt") ),
                   tabPanel("train results",dataTableOutput("prediction") ),
                   tabPanel("Matrice de confusion", verbatimTextOutput("matrice_confusion") ),
                   tabPanel("Observation", dataTableOutput("mesur") )
                   
                 )),
        # *****************************************************************************************
        
        tabPanel("NEURAL NETWORK", "RESEAU DE NEURONES",
                 tabsetPanel(
                   tabPanel("Import data", 
                            column(3,fileInput("filen", "Data", buttonLabel = "Upload...",accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                              fluidRow(strong("PARAMETRES")),
                              sliderInput("hidd", "Nombre de neuronnes caché ?",min = 1,max = 20,step = 1,value = 1),
                              numericInput("treshold", " seuil pour les dérivées partielles", 0, min = 0,max = 1,value = 0.01),
                              numericInput("ratelimi","Limite du taux d'apprentissage",value = 1),
                              numericInput("rate", " taux d'apprentissage", 0, min = 0,max = 1,value = 0.01)
                              ,selectInput("algo","Algorithme",choices = c("rprop+","backprop", "rprop+", "rprop-", "sag","slr",
                                                                           "rprop+","rprop-","backprop"),
                                           selected = "rprop+")
                              )
                            ,
                            column(9,verbatimTextOutput("dataneural"))
                            
                   ),
                   tabPanel("Données normalisé", tableOutput("dataneur")),
                   tabPanel("Données de Test", tableOutput("datant")),
                   tabPanel("Modèle", plotOutput("model")),
                   tabPanel("Predictions", verbatimTextOutput("mesures_neuralnet")),
                   #tabPanel("Prédiction_Données", verbatimTextOutput("DataPredict")),
                   tabPanel("Matrice de confusion", verbatimTextOutput("Mc_conf"))
                   
                 )),
      
        # **************************************************************************************
        # CLASSIFICATION NON SUPERVISE *********************************************************
        # **************************************************************************************
        "CLUSTERING",
        tabPanel("K-MEANS", "K-MEANS",
                 tabsetPanel(
                   tabPanel("Import data -PARAMETERS", 
                            column(3,fileInput("filekkm", "Data", buttonLabel = "Upload...",accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                              fluidRow(strong("PARAMETRES")),
                              numericInput("nbcluster", "Nombre de Cluster", min = 2,max = 20,step = 1,
                                           value = 3),
                              selectInput("axex", "variable in Xaxis",
                                          choices = c("sex",	"cp",	"trestbps",	"chol",	"fbs",	"restecg",
                                                            "thalach",	"exang",	"oldpeak","slope",	"ca",
                                                      "thal",	"target"),selected="age"),
                              selectInput("axeY", "variable in Yaxis",choices = c("sex",	"cp",	"trestbps",	
                                                                      "chol",	"fbs",	"restecg",
                                                                      "thalach",	"exang",	"oldpeak",
                                                                      "slope",	"ca",	"thal",	"target"),
                                          selected="chol")),
                            
                            column(9,verbatimTextOutput("rkmeans"))
                            
                   ),
                   tabPanel("identification pour chaque observation",verbatimTextOutput("obso")),
                   tabPanel("matrice de confusion",verbatimTextOutput("matconf")),
                   tabPanel("Evaluation et visualisation",plotOutput("evav")),
                   tabPanel("visualisation multi-cluster",plotOutput("evam")),
                   tabPanel("visualisation of clusters",plotOutput("clus")),
                   tabPanel("Observations")
                 )),
        # *****************************************************************************************
        
        
        tabPanel("HIERARCHICAL CLUSTERING", "CLASSIFICATION HIERARCHIQUE",
                 tabsetPanel(
                   tabPanel("Import data", 
                            column(3,fileInput("g", "Data", buttonLabel = "Upload...",accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                              fluidRow(strong("PARAMETRES")),
                              textInput("delim", "Delimiter (leave blank to guess)", ""),
                              numericInput("skip", "Rows to skip", 0, min = 0),
                              numericInput("rows", "Rows to preview", 10, min = 1))
                            ,
                            column(9,tableOutput("e"),"dd")
                            
                   ),
                   tabPanel("Set parameters"),
                   tabPanel("Visualise results"),
                   tabPanel("Observations")
                 ))
        
      )
    
  
)







