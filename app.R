library(shiny)
library(readxl)
library(dplyr)
library(stringr)

df <- readxl::read_excel("C:/Users/Win10/Documents/dashboard_r/dashboard/dashboard/dados_de_caminhada_corrida.xlsx") %>%
  mutate(time = format(Hora, format="%H:%M:%S")) %>%
  filter(time >= "18:45:18" & time < "18:49:23") %>%
  select(-time) %>% mutate(Velocidade = as.numeric(str_remove(Velocidade,'km/h'))) 

x = df$Velocidade
n = length(x)

df_teste <- readxl::read_excel("C:/Users/Win10/Documents/dashboard_r/dashboard/dashboard/dados_de_caminhada_corrida.xlsx") %>%
  mutate(time = format(Hora, format="%H:%M:%S")) %>%
  filter(time >= "18:40:53" & time < "18:45:12") %>%
  select(-time) %>% mutate(Velocidade = as.numeric(str_remove(Velocidade,'km/h')))

x_teste = df_teste$Velocidade
n_teste = length(x_teste)

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
    # Application title
    titlePanel("Intervalo de Confiança"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("alfa",
                        "Nível de confiança:",
                        min = 0.01,
                        max = 0.1,
                        value = 0.03)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("IC")
        )
    )
    ),
    tabPanel(
      titlePanel("Teste de hipotese"),
      sidebarLayout(
        sidebarPanel(
          numericInput("variancia", "Variância:", 4, min=1, max = 5),
          radioButtons("rb","Tipo de Teste:", c("Bilateral" = "bilateral",
                                                "Unilateral esquerda" = "uni_e",
                                                "Unilateral direita"  = "uni_d")),
          sliderInput(
            "mu0",
            "mu0:",
            min = 1,
            max = 10,
            value = 4
          ),
          sliderInput(
            "alfa_teste",
            "Nível de significância",
            min = 0.01,
            max = 0.5,
            value = 0.05
          )
        ),
        mainPanel(
          textOutput("variancia")
        )
      )  
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$IC <- renderText({ z = qnorm(input$alfa/2 + 1 - input$alfa)
    ICmin = round(mean(x) - z*sd(x)/sqrt(n), 2)
    ICmax = round(mean(x) + z*sd(x)/sqrt(n), 2)
    paste0("IC(mu) = ", "[", ICmin, "; ", ICmax, "]")})
    
    output$variancia <- renderText({
      paste(input$variancia)
      paste(input$rb)
      paste(input$mu0)
      paste(input$alfa_teste)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
