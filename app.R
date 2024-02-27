#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

Sys.setlocale("LC_ALL", "es_CO.UTF-8")
library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(readr)
library(tidyverse)
library(scales)
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "Dominancia y Prestigio"),
  HTML("<center><a href='https://investigaciones.unbosque.edu.co/codec'><img src='Logo_EvoCo.png'' width='100'></a></center>"),
  tags$h3(HTML("<center>Escala de autoevaluación de Dominancia y Prestigio</center>")),
  p(HTML("<center>Creado por
      <a style=color:#ff5555;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a><br> 
      <b><i>EvoCo</i>: Laboratorio de Evolución y Comportamiento Humano</b><br>
      Universidad El Bosque
      · 2024</center>")),
  hr(),
  fluidRow(
    column(3,
           p(HTML("<center>Indica en qué medida cada afirmación te describe escribiendo el
                  número correspondiente de la escala siguiente en el espacio proporcionado.</center><br>"),
             HTML("<center><img src='escala.png'' width='300'></center>")),
           numericInput(inputId = "i01",
                        label = "Los miembros de mi grupo de compañeros me respetan y me admiran",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i02",
                        label = "Los miembros de mi grupo de compañeros NO quieren ser como yo",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i03",
                        label = "Me gusta tener control sobre los demás",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i04",
                        label = "Los demás siempre esperan que tenga éxito",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i05",
                        label = "A menudo intento salirme con la mía sin importarme lo que quieran los demás",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i06",
                        label = "Los demás NO valoran mi opinión",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i07",
                        label = "Estoy dispuesto a utilizar tácticas agresivas para salirme con la mía",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i08",
                        label = "Mis conocidos me tienen en alta estima",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i09",
                        label = "Intento controlar a los demás en lugar de permitir que me controlen",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i10",
                        label = "NO tengo una personalidad enérgica ni dominante",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1)
    ),
    column(3,
           p(HTML("<center>Indica en qué medida cada afirmación te describe escribiendo el
                  número correspondiente de la escala siguiente en el espacio proporcionado.</center><br>"),
             HTML("<center><img src='escala.png'' width='300'></center>")),
           numericInput(inputId = "i11",
                        label = "Los demás saben que es mejor dejar que me salga con la mía",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i12",
                        label = "NO disfruto teniendo autoridad sobre otras personas",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i13",
                        label = "Los demás reconocen mis talentos y habilidades únicos",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i14",
                        label = "Los demás me consideran un experto en algunos temas",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i15",
                        label = "Los demás me piden consejo sobre diversos temas",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i16",
                        label = "Algunas personas me tienen miedo",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           numericInput(inputId = "i17",
                        label = "A otros NO les gusta estar conmigo",
                        min = 1,
                        max = 7,
                        value = NA,
                        step = 1),
           hr(),
           tags$h4("Demográficos (opcionales)"),
           numericInput(inputId = "edad",
                        label = "Edad",
                        min = 1,
                        max = 99,
                        value = NA,
                        step = 1),
           selectInput(inputId = "gender", label = "Género",
                       choices = c("Mujer", 
                                   "Hombre", 
                                   "Mujer trans", 
                                   "Hombre trans", 
                                   "No binario",
                                   "Otra"))
    ),
    column(4,
           nextGenShinyApps::submitButton("runSim", text = "¿Todo listo? ¡Calcula tus puntajes!", 
                                          icon("paper-plane"), bg.type = "danger"),
           tags$h1("Tus resultados"),
           tags$h3("Este es tu nivel de Dominancia y Prestigio, en relación con el de las demás personas que han respondido"),
           plotOutput("DomPresPlot") %>%
             withSpinner(color = "#ff5555")
    )
  )
)

server <- function(input, output, session) {
  
  # Simulate population
  
  dat <- reactive({
    datos <- read_csv("Dominancia_Prestigio.csv") |> 
      add_row(num = max(num)+1, 
              Date = format(Sys.time(), "%Y-%m-%d %X"),
              i01 = input$i01,
              i02 = input$i02,
              i03 = input$i03,
              i04 = input$i04,
              i05 = input$i05,
              i06 = input$i06,
              i07 = input$i07,
              i08 = input$i08,
              i09 = input$i09,
              i10 = input$i10,
              i11 = input$i11,
              i12 = input$i12,
              i13 = input$i13,
              i14 = input$i14,
              i15 = input$i15,
              i16 = input$i16,
              i17 = input$i17,
              gen = input$gender,
              age = nput$edad)
    write_csv(datos, "Dominancia_Prestigio.csv")
    return(datos)
  })

  dpLong <- reactive({
    dat2 <- dat() |> 
      select(num, Dominancia, Prestigio) |> 
      pivot_longer(cols = Dominancia:Prestigio, names_to = "Variable", values_to = "Puntaje") 
    return(dat2)
  })
  
  dpLongMax <- reactive({
    dat3 <- dpLong() |> 
      group_by(Variable) |> 
      mutate(Percentil = ntile(Puntaje, 100)) |> 
      ungroup() |> 
      filter(num == max(num, na.rm=TRUE))
    return(dat3)
  })
  
  # Plot 
  output$DomPresPlot <- renderPlot({
    ggplot(dpLong(), aes(y = Puntaje, x = Variable)) +
      geom_violin(aes(fill = Variable, color = Variable), alpha = 0.1) +
      geom_boxplot(aes(color = Variable), width = 0.1) +
      geom_jitter(alpha = 0.1, width = 0.1) +
      geom_point(data = dpLongMax(), aes(color = Variable), size = 5, color = "black") +
      geom_label_repel(data = dpLongMax(), 
                       aes(label = paste0("Tú: ", Percentil, "%"), 
                           fill = Variable, color = Variable), 
                       color = "black",
                       point.padding = NA,
                       box.padding = 0.5) +
      ylim(c(1,7)) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(x = "") +
      theme(legend.position = "none")
  })
}

# Same theme for plots
thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)