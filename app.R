library(shiny)
library(tidyverse)
library(DT)
library(writexl)

ui <- fluidPage(
    
    # App title ----
    titlePanel("EURO 2020"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            h3('Mise à jour des résultats'),
            markdown('Pour mettre à jour le classement, les nouveaux résultats \
            des matchs peuvent etre uploadés ici. Ils doivent être en format \
            *.csv* (texte avec séparation par virgules), et la première ligne 
            doit correpondre au noms de colonnes suivants: team1, score1, \
            score2, team2. Les noms des équipes doivent correspondre à ceux \
            dans le fichier Excel contenant les pronostics. Les matchs qui n\'ont
            pas encore eu lieu peuvent rester dans le fichier s\'ils indiquent \
            *NA*. \n Exemple:

    ```
    team1,score1,score2,team2
    Turquie,0,3,Italie
    Pays de Galles,1,1,Suisse
    Danemark,0,1,Finlande
    Belgique,3,0,Russie
    Danemark,NA,NA,Belgique

                     '),
            # Input: Select a file ----
            fileInput("datafile", "Sélectionner le fichier",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3('Classement général'),
            p('Si aucun nouveau fichier n\'a été selectionné à gauche, le tableau
            montre les derniers résultats disponibles (maj: mer 16/6, 23h).'),
            # Output: Data file ----
            DT::dataTableOutput("table"),
            # Button
            downloadButton("downloadData", "Télécharger le classement général")
            
        )
    )
)

server <- function(input, output) {
    
    
    dataframe <- reactive({
        if (is.null(input$datafile))
            #return(NULL)
            path <-  'data/results.csv'
        else  
            path <- input$datafile$datapath
        
        results <- read_csv(path, #input$datafile$datapath,
                            col_types = cols(team1 = col_character(),
                                             score1 = col_integer(),
                                             score2 = col_integer(),
                                             team2 = col_character()))
        pronos <- read_csv('data/pronos.csv') %>% 
            rename(Nom=sheet)
        joined <- left_join(pronos, results, by =c("team1", "team2")) %>%
            drop_na()
        
        # Calculate some variables
        points <- joined %>%
            # Calculate total goal difference
            mutate(diff = abs(score1-prono1) + abs(score2-prono2),
                   predicts_win = (prono1-prono2!=0),
                   result_is_win = (score1-score2!=0),
                   different_sign = sign(prono1-prono2)!=sign(score1-score2),  
                   wrong_team = predicts_win==TRUE & result_is_win==TRUE & different_sign==TRUE) %>%
            mutate(match_points = case_when(
                (wrong_team) ~ 0,
                (diff == 0) ~ 40,
                (diff == 1) ~ 20,
                (diff == 2) ~ 10,
                (diff > 2) ~ 0)
            )
        
        #Summary table
        final_table <- points %>%
            group_by(Nom) %>%
            summarise(`Score Total` = sum(match_points),
                      `Goals d'écart` = sum(diff),
                      `Matchs devinés` = sum(diff==0),
                      `Pronostics opposés` = sum(wrong_team)) %>%
            arrange(-`Score Total`)
        final_table
    })
    
    
    output$table <- renderDataTable({
        dataframe()}, options = list(lengthMenu = list(c(10, -1), c('10', '45')),
                                     pageLength = 10))
    
    # Downloadable csv classification table ----
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         paste('final_table', ".csv", sep = ",")
    #     },
    #     content = function(file) {
    #         write.csv(dataframe(), file, row.names = FALSE)
    #     }
    # )
    output$downloadData <- downloadHandler(
        filename = function() {'classement.xlsx'},
        content = function(file) {
            write_xlsx(dataframe(), path=file,  col_names = TRUE, format_headers = TRUE)
        }
    )
    
}

#output$plot <- renderPlot({
#    if(!is.null(dataframe()))
#        ggplot(dataframe(),aes(x=X,y=add))+geom_point()
#})


shinyApp(ui, server)

#library(rsconnect)
#rsconnect::deployApp('D:\\Dropbox\\MyCode\\foot\\euro2020')