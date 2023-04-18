library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(OpenStreetMap)

df <- readxl::read_excel("C:/Users/Win10/Documents/dashboard_r/dashboard/dashboard/dados_de_caminhada_corrida.xlsx")

df_map <- df %>%
  mutate(Velocidade = as.numeric(str_remove(Velocidade,'km/h'))) 

df_confianca <- df %>%
  mutate(time = format(Hora, format="%H:%M:%S")) %>%
  filter(time >= "18:45:18" & time < "18:49:23") %>%
  select(-time) %>% mutate(Velocidade = as.numeric(str_remove(Velocidade,'km/h'))) 

x = df_confianca$Velocidade
n = as.numeric(length(x))

df_teste <- df %>%
  mutate(time = format(Hora, format="%H:%M:%S")) %>%
  filter(time >= "18:40:53" & time < "18:45:12") %>%
  select(-time) %>% mutate(Velocidade = as.numeric(str_remove(Velocidade,'km/h')))

x_teste = df_teste$Velocidade
n_teste = as.numeric(length(x_teste))

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
          numericInput("variancia", "Variância (0.1~5.0):", 0.4, min=0.1, max = 5.0, 0.01),
          radioButtons("rb","Tipo de Teste:", c("Bilateral" = "bilateral",
                                                "Unilateral esquerda" = "uni_e",
                                                "Unilateral direita"  = "uni_d")),
          sliderInput(
            "mu0",
            "mu0:",
            min = 4.1,
            max = 14.1,
            value = 5.7
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
          textOutput("teste_hipotese")
        )
      )  
    ),
    tabPanel(
      titlePanel("Regressão Linear"),
      plotOutput("plotcar")
      ),
    tabPanel(
      titlePanel("Mapa"),
      plotOutput("plotmap")
    )
    )
    
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$IC <- renderText({ z = qnorm(input$alfa/2 + 1 - input$alfa)
    ICmin = round(mean(x) - z*sd(x)/sqrt(n), 2)
    ICmax = round(mean(x) + z*sd(x)/sqrt(n), 2)
    paste0("IC(mu) = ", "[", ICmin, "; ", ICmax, "]")})
    
    output$teste_hipotese <- renderText({
      mu0 = input$mu0
      
      sigxb = sqrt(input$variancia)/sqrt(n_teste)
      
      xb = mean(x_teste)
      
      alfa_teste = input$alfa_teste
      
      prob = 1 - alfa_teste + alfa_teste/2
      
      ztab = qnorm(prob) 
      
      zcalc = (xb - mu0)/sigxb
      
      if (input$rb == "bilateral") {
        
        if(zcalc <= -ztab | zcalc >= ztab){
          print(paste0('Rejeitamos H0 ao nível de ', alfa_teste*100, '%'))
        }else{
          print(paste0('Aceitamos H0 ao nível de ', alfa_teste*100, '%'))
        }
      } else if (input$rb == "uni_e") {
        prob =  alfa_teste 
        
        ztab = qnorm(prob)         
        
        if( zcalc < -ztab){
          print(paste0('Rejeitamos H0 ao nível de ', alfa_teste*100, '%'))
        }else{
          print(paste0('Aceitamos H0 ao nível de ', alfa_teste*100, '%'))
        }
      } else {
        prob = 1 - alfa_teste 
        
        ztab = qnorm(prob) 
        
        if( zcalc > ztab){
          print(paste0('Rejeitamos H0 ao nível de ', alfa_teste*100, '%'))
        }else{
          print(paste0('Aceitamos H0 ao nível de ', alfa_teste*100, '%'))
        }
      }
    })
    
    output$plotcar <- renderPlot({
  
      y = cars$speed
      x = cars$dist
      n = length(x)
      
      num = n*sum(x*y) - sum(x)*sum(y)
      den1 = sqrt(n*sum(x^2) - sum(x)^2)
      den2 = sqrt(n*sum(y^2) - sum(y)^2)
      
      r = num/(den1*den2)
      r2 = r^2
      
      plot(x,y)
      reta = lm(y~x)
      
      b = round((num/(n*sum(x^2) - sum(x)^2)),2)
      a = round(((sum(y) - b*sum(x))/n),2)
    #  a = round(reta$coefficients[1], 2)
    #  b = round(reta$coefficients[2], 2) 
      
      abline(a = a, b = b)
      text(100,15, paste0('y(Eq. reta)= ', a, ' + ', b, '*x'))
      text(100,13, paste0('r(coef. relação) = ',round(r,3)))
      text(100,11, paste0('R2(coef. determ) = ',round(r2,3)))
    })
  
    
    output$plotmap <- renderPlot({
      long <- c()
      lat <- c()
      
      for(i in 1:length(df_map$Coordenadas)){
        
        lista <- as.list(strsplit(df_map$Coordenadas[[i]], ", "))
        lat[i] <-as.numeric(lista[[1]][1]) 
        long[i] <- as.numeric(lista[[1]][2]) 
      }
      
      #long = df2$lat_long
      #lat = df2$lat_long[2]
      
      bb = matrix(c(-34.955, -34.945, 
                    -8.019, -8.014), 2,2, byrow=T)
      rownames(bb) = c('long', 'lat')
      colnames(bb) = c('min', 'max')
      
      df1 = data.frame(df_map$Velocidade)
      
      df1$long = long
      df1$lat = lat
      lonr = bb[1,2]; latu = bb[2,2] 
      lonl = bb[1,1]; latd = bb[2,1]
      
      sa_map = openmap(c(latu+0.001, lonl-0.001), 
                       c(latd-0.001, lonr+0.001),
                       type = "osm", mergeTiles = TRUE, minNumTiles = 9L)
      
      sa_map2 = openproj(sa_map)
      
      sa_map2_plt = OpenStreetMap::autoplot.OpenStreetMap(sa_map2) + 
        geom_point(data = df,
                   aes(x = long, y = lat), 
                   colour = "red", size =  2.5) +
        xlab("Longitude") + ylab("Latitude")
      sa_map2_plt
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

