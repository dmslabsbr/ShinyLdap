#ui.R
ui0 <- fluidPage(
  titlePanel("Login Shiny"),
  fluidRow(textInput("user","Usuário",""),passwordInput("senha","Senha","")),
  actionButton("go", "go"),
  h2( verbatimTextOutput("info"),
  tableOutput('table'))
)


ui <- ui0