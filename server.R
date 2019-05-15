#server.R

# código START

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    session$onSessionEnded(stopApp)
    
    cat(file=stderr(),"** Start :", as.character.Date(Sys.time()) , "\n")
    cat(file=stderr(),"getwd() ", getwd(), "\n")
    
    res <- reactiveValues(data = NULL)
    go_click <- observeEvent(input$go, {
        temLdap = temComando('ldapsearch')
        cat(file=stderr(), "temldap:", temLdap,'\n')
        if (temLdap) {
            res$data <- consultaLdap(input$user,input$senha)
            res$table_data <- userLdap(input$user,input$senha)
            
        } else { 
            res$data <- 'Não tem LDAP instalado!'
        }
    })
    
    output$info <- renderPrint(res$data);
    output$table <- renderTable(res$table_data);


})
# fim Start
