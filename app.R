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
library(shinythemes)
library(shinycssloaders)
library(readr)
library(tidyverse)
library(scales)
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "Dominancia y Prestigio"),
  tags$h2(HTML("<center>Escala de autoevaluación de <b style=color:#428bca;>Dominancia</b> y <b style=color:#ff8080;>Prestigio</b></center>")),
  p(HTML("<center>Aplicación creada por
      <a style=color:#ff8080;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>, PhD, MSc<br>
      <a style=color:#428bca;  href='https://www.psicologia.unbosque.edu.co/lach'><i><b>EvoCo</i></b>: Laboratorio de Evolución y Comportamiento Humano</a><br>
      Facultad de Psicología · Universidad El Bosque · 2024<br>
      </center>")),
  hr(),
  fluidRow(
    column(8,
           style='margin-bottom:30px;border:1px solid; padding: 20px;',
           tags$h4(HTML("<center>Indica en qué medida cada afirmación te describe escribiendo el
                  número correspondiente de la escala siguiente en el espacio proporcionado.</center><br>"),
                  HTML("<center><img src='escala.png'' width='500'></center>")),
           column(4,
                  sliderInput(inputId = "i01",
                              label = "Los miembros de mi grupo de compañeros me respetan y me admiran",
                              min = 1,
                              max = 7,
                              value = 1,               
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i02",
                              label = "Los miembros de mi grupo de compañeros NO quieren ser como yo",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i03",
                              label = "Me gusta tener control sobre los demás",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i04",
                              label = "Los demás siempre esperan que tenga éxito",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i05",
                              label = "A menudo intento salirme con la mía sin importarme lo que quieran los demás",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i06",
                              label = "Los demás NO valoran mi opinión",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto")
           ),
           column(4,
                  sliderInput(inputId = "i07",
                              label = "Estoy dispuesto/dispuesta a utilizar tácticas agresivas para salirme con la mía",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i08",
                              label = "Mis conocidos/conocidas me tienen en alta estima",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i09",
                              label = "Intento controlar a los demás en lugar de permitir que me controlen",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i10",
                              label = "NO tengo una personalidad enérgica ni dominante",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i11",
                              label = "Los demás saben que es mejor dejar que me salga con la mía",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i12",
                              label = "NO disfruto teniendo autoridad sobre otras personas",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto")
           ),
           column(4,
                  sliderInput(inputId = "i13",
                              label = "Los demás reconocen mis habilidades y talentos únicos",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i14",
                              label = "Los demás me consideran un experto en algunos temas",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i15",
                              label = "Los demás me piden consejo sobre diversos temas",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i16",
                              label = "Algunas personas me tienen miedo",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  sliderInput(inputId = "i17",
                              label = "A otras personas NO les gusta estar conmigo",
                              min = 1,
                              max = 7,
                              value = 1,
                              ticks = TRUE,
                              step = 1,
                              width = "auto"),
                  hr(),
                  p(HTML("<font size='2'>
                  <b>Adaptado de:</b><br>
                  Cheng, J. T., Tracy, J. L., & Henrich, J. (2010). 
                  Pride, personality, and the evolutionary foundations of human social status. 
                  <i>Evolution and Human Behavior, 31</i>(5), 334–347. 
                  <a style=color:#428bca;  href='https://doi.org/10.1016/j.evolhumbehav.2010.02.004'>https://doi.org/10.1016/j.evolhumbehav.2010.02.004</a></font>")
                  )
           )
    ),
    column(4,
           tags$h4("Demográficos (opcionales)"),
           numericInput(inputId = "edad",
                        label = "Edad",
                        min = 1,
                        max = 99,
                        value = 18,
                        step = 1),
           selectInput(inputId = "gender", label = "Género",
                       choices = c("Mujer", 
                                   "Hombre", 
                                   "Mujer trans", 
                                   "Hombre trans", 
                                   "No binario",
                                   "Otra")),
           hr(),
           tags$h1("Tus resultados"),
           br(),
           actionButton("add_graph", 
                        label = "¿Respondiste todas las preguntas? ¡Haz clic acá y calcula tus puntajes!", 
                        icon("play")),
           htmlOutput("powText1"),
           plotOutput("DomPresPlot") |> 
             withSpinner(color = "#ff8080",
                         image = "Logo_EvoCo.png",
                         image.width = 300, image.height = 300),
           htmlOutput("powText2")
    )
  )
)

server <- function(input, output, session) {
  
  # datosFULL <- reactive({
  #   datRAW <- read_csv("Data/Dominancia_Prestigio.csv") |> 
  #     mutate(Date = as.character(Date))
  #   return(datRAW)
  # })
  
  datosFULL <- read_csv("Data/Dominancia_Prestigio.csv") |> 
    mutate(Date = as.character(Date))
  
  pnum <- reactive({
    thispart <- max(datosFULL$num)+1
    return(thispart)
  })
  
  dat <- reactive({
    datos <- datosFULL |> 
      add_row(num = pnum(), 
              Date = as.character(format(Sys.time(), "%d/%m/%Y %H:%M")),
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
              age = input$edad) |> 
      rowwise() |> 
      mutate(Dominancia = sum(i03, i05, i07, i09, (8-i10), i11, (8-i12), i16)/8,
             Prestigio = sum(i01, (8-i02), i04, (8-i06), i08, i13, i14, i15, (8-i17))/9)
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
      filter(num == max(num, na.rm = TRUE))
    return(dat3)
  })
  
  # Plot 
  observeEvent(input$add_graph,{
    plot1 <- ggplot(dpLong(), aes(y = Puntaje, x = Variable)) +
      geom_violin(aes(fill = Variable, color = Variable), alpha = 0.1) +
      geom_boxplot(aes(color = Variable), width = 0.1) +
      geom_jitter(aes(color = Variable), alpha = 0.15) +
      geom_point(data = dpLongMax(), aes(color = Variable), size = 5) +
      geom_label_repel(data = dpLongMax(), 
                       aes(label = paste0("Tú: ", round(Puntaje,2)), 
                           fill = Variable, color = Variable), 
                       size = 8,
                       position = position_nudge_repel(x = c(0.3, -0.3),
                                                       y = 0),
                       arrow = arrow(length = unit(0.05, "npc"), type = "closed"),
                       color = "black",
                       point.padding = 1,
                       box.padding = 0.5) +
      ylim(c(1,7)) +
      scale_fill_manual(values = c("#428bca", "#ff8080")) +
      scale_color_manual(values = c("#428bca", "#ff8080")) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(x = "") +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            text = element_text(size = 16)) +
      facet_wrap(~Variable, scales = "free_x") +
      theme(strip.text = element_text(
        size = 20))
    
    output$DomPresPlot <- renderPlot({
      input$add_graph
      plot1
    })
  })
  
  observeEvent(input$add_graph, {
    explText1 <- paste("<font size='5'><b style=color:#ffffff;>Puntajes:</b> <b style=color:#428bca;>Dominancia = ", round(dpLongMax()$Puntaje[1], 2),
                       "</b> <b style=color:#ffffff;>|</b> <b style=color:#ff8080;>Prestigio = ", round(dpLongMax()$Puntaje[2], 2), "</b>.")
    output$powText1 <- renderText({
      input$add_graph
      explText1
    })
  })
  
  observeEvent(input$add_graph, {
    explText2 <- paste("<font size='3'><a style=color:#ffffff;>Esto te ubica en el</a><b style=color:#428bca;>", dpLongMax()$Percentil[1], "%</b>
                       <a style=color:#ffffff;>y</a><b style=color:#ff8080;>", dpLongMax()$Percentil[2], "%</b><a style=color:#ffffff;>, 
                       respectivamente, con respecto a las otras<b>",
                      pnum()-1, "</b>personas que han respondido. Tú te sientes más dominante que el", 
                      dpLongMax()$Percentil[1], "% de las personas (o menos dominante que el", 
                      100-dpLongMax()$Percentil[1], "%), y más prestigioso o prestigiosa que el", dpLongMax()$Percentil[2], "% 
                      (menos prestigioso o prestigiosa que el", 100-dpLongMax()$Percentil[1], "%).</a>")
    output$powText2 <- renderText({
      input$add_graph
      explText2
    })
  })
  
  observeEvent(input$add_graph, {
    saveData <- function(data){
      data <- dat()
      # Write the file to the local system
      write.csv(
        x = data,
        file = file.path("Data", "Dominancia_Prestigio.csv"),
        row.names = FALSE,
        append = FALSE)
    }
    saveData()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)