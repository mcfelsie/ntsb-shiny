library(shiny)

# Define UI for random distribution application 
shinyUI(navbarPage("NTSB Aircraft Reports",
  #titlePanel("NTSB Aircraft Reports"),
  tabPanel("Narratives", dataTableOutput("table")),
  tabPanel("Frequencies", 
    sidebarLayout(
      sidebarPanel(sliderInput("n", "Number of Terms:", min = 3, max = 30, value = 10),  
                   helpText("Determine most frequently occuring terms in all narratives.")),       
      mainPanel(plotOutput("freq"))
    )
  ),
  tabPanel("Associations",
     sidebarLayout(
       sidebarPanel(
         textInput("term", "Enter a Term...", value = "student"),
         #selectInput("term", "Pick a term:", c("student", "stall", "failur", "fuel", "land", "suicid")),
         numericInput('corr', 'Minimum Correlation', 0.10, min = 0, max = 1) # for findAssocs
       ),
       mainPanel(verbatimTextOutput("assoc"))
     )
  ),
  # tabPanel("Word Cloud", plotOutput("cloud")),         
  tabPanel("SVD", plotOutput("svd"), 
                  helpText("Singular Value Decomposition reduces the DTM to a dense matrix with fewer columns.  The new columns are orthogonal and
                            linear combinations of the rows in the original DTM, selected to preserve as much structure of the original DTM as possible."),
                  helpText("The words appearing frequently in the same documents or with commen sets of words are plotted together. 
                                               We also look for themes describing the spread of terms in this plot."),
                  helpText("Next step: Perform CART as a function of SVDs to obtain records with high likelihood of fatality.")
  ),
  tabPanel("Fatal Report", plotOutput("fatal")),
  tabPanel("Cluster Cloud", 
    sidebarLayout(
     sidebarPanel(numericInput('clusters', 'Enter Cluster Number', 15, min = 1, max = 50)),  #for k-means
     mainPanel(plotOutput("cluster"))  
    )
  ),
  tabPanel("Network of Terms", plotOutput("network"))
  
))   
