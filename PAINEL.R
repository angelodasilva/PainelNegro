
#setwd("~/Estudos/Cursos_feitos/Shiny/PainelNegro")

#install.packages("tidyr")
#install.packages("sf")
library(shiny)
library(ggplot2)
library(tidyr)
library(bslib)
library(sf)

options(scipen = 999)  # desativando notacao cientifica

dados <- readRDS("dadosPNAD.RDS")
dados$estado <- substr(dados$UPA,1,2)
#dados$ano <- paste0("20",dados$ano)
dados$ano <- factor(dados$ano, order=T)
dados$V2007 <- ifelse(dados$V2007 == "Masculino", "Homem", dados$V2007)
dados$V2007 <- ifelse(dados$V2007 == 2 | dados$V2007 == 4, "Mulher", dados$V2007)
dados$V2007 <- ifelse(dados$V2007 == 3, "Homem", dados$V2007)
dados$VD3005 <- substr(dados$VD3005, 1, 2)
dados$VD3005 <- ifelse(dados$VD3005 == "Se", 0, dados$VD3005)
dados$VD3005 <- as.numeric(dados$VD3005)
dados$VD3004 <- ifelse(dados$VD3004 == "Sem instrução e menos de 1 ano de estudo" | 
                         dados$VD3004 == "Fundamental incompleto ou equivalente", "1-Sem fundamental",
                       ifelse(dados$VD3004 == "Fundamental completo ou equivalente" | 
                                dados$VD3004 == "Médio incompleto ou equivalente", "2-Fundamental completo",
                              ifelse(dados$VD3004 == "Médio completo ou equivalente" | 
                                       dados$VD3004 == "Superior incompleto ou equivalente", "3-Médio completo",
                                     ifelse(dados$VD3004 == "Superior completo","4-Superior completo",dados$VD3004)
                              )
                       )
)
dados$VD3004 <- factor(dados$VD3004, order=T, levels = c("1-Sem fundamental",
                                                         "2-Fundamental completo", "3-Médio completo", "4-Superior completo"))

br <- st_read("brasil.shp")
br$estado <- c("11","12","13","14","15","16","17","21","22","23","24","25","26","27","28","29",
               "31","32","33","35","41","42","43","50","51","52","53")
br$ID <- NULL

corpreta <- "black"
corparda <- "brown4"
corbranca <- "gray41"
coramarela <- "yellow4"
corindia <- "green4"


ui <- navbarPage(
  theme = bs_theme(version = 4,
                   bootswatch = "sketchy",
                   primary = "#2F4F4F",
                   secondary = "#000080",
                   success = "#dadeba",
                   info = "#800000"),
  title = h1(em("Pai"),strong("NE"),em("l"), strong("GRO")),
  tabPanel(
    title = "Principal",
    fluidPage(
      h3("População em 2022"),
      hr(),
      sidebarLayout(
        sidebarPanel(
          radioButtons("radio_raca", label = h5("Raça"), 
                             choices = list("Preta" = "Preta", "Parda" = "Parda",
                                            "Branca" = "Branca", "Amarela" = "Amarela",
                                            "Indígena" = "Indígena"),
                             selected = "Preta")
        ),
        mainPanel(
          h3(strong("população", textOutput(outputId = "raca")), align = "center"),
          h5(textOutput(outputId = "estat_raca"), align = "center"),
          h5(textOutput(outputId = "estat_raca1"), align = "center"),
          hr(),
          plotOutput(outputId = "raca_map"),
          hr(),
          tableOutput(outputId = "raca_tabela")
        )
      )
    )
  ),
  navbarMenu(
    title = "Escolaridade",
    tabPanel(
      title="Analfabetismo",
      fluidPage(
        h3("Analfabetismo em 2022"),
        hr(),
        h5(strong("Analfabetos")," são definidos pelas pessoas com 15 anos ou mais de idade que não sabem ler ou escrever."),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_analf", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_analf", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            h3(strong("população analfabeta", textOutput(outputId = "analf")), align = "center"),
            h5(textOutput(outputId = "estat_analf"), align = "center"),
            h5(textOutput(outputId = "estat_analf1"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_analf_map"),
            plotOutput(outputId = "analf_map"),
            tableOutput(outputId = "analf_tabela")
          )
        )
      )
    ),
    tabPanel(
      title="Nível de Instrução",
      fluidPage(
        h3("Nível de Instrução em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(checkboxGroupInput("checkGroup_instrucao", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            textOutput(outputId = "aviso_instrucao"),
            plotOutput(outputId = "instrucao")
          )
        )
      )
    ),
    tabPanel(
      title="Anos de Estudo",
      fluidPage(
        h3("Anos de estudo em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_estudo", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_estudo", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            h3(strong("população", textOutput(outputId = "estudo")), align = "center"),
            h5("média de anos de estudo:", align = "center"),
            h5(textOutput(outputId = "estat_estudo"), align = "center"),
            h5(textOutput(outputId = "estat_estudo1"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_estudo_map"),
            plotOutput(outputId = "estudo_map"),
            tableOutput(outputId = "estudo_tabela")
          )
        )
      )
    )
  ),
  navbarMenu(
    title = "Emprego",
    tabPanel(
      title = "Força de Trabalho",
      fluidPage(
        h3("Participação na Força de Trabalho (FT) em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_partic", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_partic", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            h3(strong("população", textOutput(outputId = "partic")), align = "center"),
            h5("fora da força de trabalho:", align = "center"),
            h5(textOutput(outputId = "estat_partic"), align = "center"),
            h5(textOutput(outputId = "estat_partic1"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_partic_map"),
            plotOutput(outputId = "partic_map"),
            tableOutput(outputId = "partic_tabela")
          )
        )
      )
    ),
    tabPanel(
      title = "Desemprego",
      fluidPage(
        h3("Taxa de Desemprego em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_desemp", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_desemp", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            h3(strong("população", textOutput(outputId = "desemp")), align = "center"),
            h5("desempregados:", align = "center"),
            h5(textOutput(outputId = "estat_desemp"), align = "center"),
            h5(textOutput(outputId = "estat_desemp1"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_desemp_map"),
            plotOutput(outputId = "desemp_map"),
            tableOutput(outputId = "desemp_tabela")
          )
        )
      )
    ),
    tabPanel(
      title = "Informalidade",
      fluidPage(
        h3("Taxa de Informalidade em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_informal", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_informal", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher")),
            hr(),
            h5(strong("Informais")," são os empregados sem carteira assinada, os empregadores e os trabalhadores
                      conta-própria sem registro de CNPJ e os trabalhadores familiares.")
          ),
          mainPanel(
            h3(strong("população", textOutput(outputId = "informal")), align = "center"),
            h5("informais:", align = "center"),
            h5(textOutput(outputId = "estat_informal"), align = "center"),
            h5(textOutput(outputId = "estat_informal1"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_informal_map"),
            plotOutput(outputId = "informal_map"),
            tableOutput(outputId = "informal_tabela")
          )
        )
      )
    ),
    tabPanel(
      title = "Desalento",
      fluidPage(
        h3("Taxa de Desalento em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_desal", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_desal", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            h3(strong("população", textOutput(outputId = "desal")), align = "center"),
            h5("desalentados:", align = "center"),
            h5(textOutput(outputId = "estat_desal"), align = "center"),
            h5(textOutput(outputId = "estat_desal1"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_desal_map"),
            plotOutput(outputId = "desal_map"),
            tableOutput(outputId = "desal_tabela")
          )
        )
      )
    ),
    tabPanel(
      title = "Rendimentos",
      fluidPage(
        h3("Rendimentos em 2022"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            radioButtons("radio_rend", label = h5("Raça"), 
                               choices = list("Preta" = "Preta", "Parda" = "Parda",
                                              "Branca" = "Branca", "Amarela" = "Amarela",
                                              "Indígena" = "Indígena"),
                               selected = "Preta"),
            checkboxGroupInput("checkGroup_rend", label = h5("Sexo"), 
                               choices = list("Homem" = "Homem", "Mulher" = "Mulher"),
                               selected = c("Homem","Mulher"))
          ),
          mainPanel(
            h3(strong("população", textOutput(outputId = "rend")), align = "center"),
            h5("média de rendimentos:", align = "center"),
            h5(textOutput(outputId = "estat_rend"), align = "center"),
            hr(),
            textOutput(outputId = "aviso_rend_map"),
            plotOutput(outputId = "rend_map"),
            tableOutput(outputId = "rend_tabela")
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # RAÇA -----
  
  dadosraca <- as.data.frame(t(tapply(dados$one * dados$V1032,dados$V2010,sum)))
  
  output$raca <- renderText({print(tolower(input$radio_raca))})
  
  output$estat_raca <- renderText({print(paste0(round(dadosraca[,input$radio_raca],0)," pessoas"))})
  output$estat_raca1 <- renderText({
    print(paste0(round(dadosraca[,input$radio_raca]/sum(dadosraca)*100,2), "% da população"))
  })
  output$raca_map <- renderPlot({
    dadosraca <- tapply(dados$one * dados$V1032,list(dados$estado,dados$V2010),sum)
    dadosraca <- ifelse(is.na(dadosraca), 0, dadosraca)
    dadosraca <- as.data.frame(round(prop.table(dadosraca,1)*100,2))
    dadosraca$estado <- rownames(dadosraca)
    dadosraca <- as.data.frame(dadosraca[,c(input$radio_raca, "estado")])
    dadosraca <- merge(br,dadosraca,all.x = TRUE) 
    
    if (input$radio_raca == "Preta"){
      paleta <- c("white", corpreta)
    } else if(input$radio_raca == "Parda") {
      paleta <- c("white", corparda)
    } else if(input$radio_raca == "Branca") {
      paleta <- c("white", corbranca)
    } else if(input$radio_raca == "Amarela") {
      paleta <- c("white", coramarela)
    } else {
      paleta <- c("white", corindia)
    }
    
    plot(dadosraca[input$radio_raca], 
         main = paste0("% da referida população por UF"),
         pal = colorRampPalette(paleta))
  })
  output$raca_tabela <- renderTable({
    dadosraca <- tapply(dados$one * dados$V1032,list(dados$UF,dados$V2010),sum)
    dadosraca <- ifelse(is.na(dadosraca), 0, dadosraca)
    dadosraca <- round(dadosraca,0)
    dadosraca1 <- as.data.frame(round(prop.table(dadosraca,1)*100,2))
    dadosraca1$UF <- rownames(dadosraca1)
    dadosraca <- cbind(dadosraca1[,c("UF",input$radio_raca)],dadosraca[,input$radio_raca])
    rm(dadosraca1)
    names(dadosraca) <- c("UF","% da população","população")
    dadosraca <- dadosraca[order(dadosraca[,"% da população"], decreasing = T),]
    rownames(dadosraca) <- NULL
    dadosraca$pos <- paste0(as.character(rownames(dadosraca)),"º")
    dadosraca <- dadosraca[,c("pos","UF","% da população","população")]
    dadosraca$população <- as.integer(dadosraca$população)
    print(dadosraca)
  })

  
  # ANALFABETISMO -----
  
  
  dadosanalf <- dados[dados$V3001 == "Não" & dados$V2009 >= 15,]
  dadosanalf <- tapply(dadosanalf$one * dadosanalf$V1032,list(dadosanalf$V2010,dadosanalf$V2007),sum)
  dadosanalf <- as.data.frame(t(dadosanalf))
  dadosmais15 <- dados[dados$V2009 >= 15,]
  dadosmais15 <- tapply(dadosmais15$one * dadosmais15$V1032,list(dadosmais15$V2010,dadosmais15$V2007),sum)
  dadosmais15 <- as.data.frame(t(dadosmais15))
  dadosmais15 <- dadosmais15 + dadosanalf
  
  output$analf <- renderText({
    print(tolower(input$radio_analf))
    })
  output$estat_analf <- renderText({
    print(paste0(round(dadosanalf[1,input$radio_analf],0)," homens e ",
                 round(dadosanalf[2,input$radio_analf],0)," mulheres"))
  })
  output$estat_analf1 <- renderText({
    print(paste0(round(dadosanalf[1,input$radio_analf]/dadosmais15[1,input$radio_analf]*100,2)," % entre os homens e ",
                 round(dadosanalf[2,input$radio_analf]/dadosmais15[2,input$radio_analf]*100,2)," % entre as mulheres"))
  })
  output$analf_map <- renderPlot({
    if (length(input$checkGroup_analf) > 0){
      dadosanalf <- dados[dados$V3001 == "Não" & dados$V2009 >= 15,]
      dadosanalf <- tapply(dadosanalf$one * dadosanalf$V1032,list(dadosanalf$estado,dadosanalf$V2010,dadosanalf$V2007),sum)
      dadosmais15 <- dados[dados$V2009 >= 15,]
      dadosmais15 <- tapply(dadosmais15$one * dadosmais15$V1032,list(dadosmais15$estado,dadosmais15$V2010,dadosmais15$V2007),sum)
      dadosanalf <- ifelse(is.na(dadosanalf), 0, dadosanalf)
      dadosmais15 <- ifelse(is.na(dadosmais15), 0, dadosmais15)
      if (length(input$checkGroup_analf) == 2) {
        dadosanalf <- dadosanalf[,,1] + dadosanalf[,,2]
        dadosmais15 <- dadosmais15[,,1] + dadosmais15[,,2]
      } else if (input$checkGroup_analf == "Homem") {
        dadosanalf <- dadosanalf[,,1]
        dadosmais15 <- dadosmais15[,,1]
      } else {
        dadosanalf <- dadosanalf[,,2]
        dadosmais15 <- dadosmais15[,,2]
      }
      dadosanalf <- dadosanalf/dadosmais15
      rm(dadosmais15)
      dadosanalf <- ifelse(is.na(dadosanalf), 0, dadosanalf)
      dadosanalf <- as.data.frame(dadosanalf)
      dadosanalf$estado <- rownames(dadosanalf)
      dadosanalf <- as.data.frame(dadosanalf[,c(input$radio_analf, "estado")])
      dadosanalf <- merge(br,dadosanalf,all.x = TRUE) 
      
      if (input$radio_analf == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_analf == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_analf == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_analf == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadosanalf[input$radio_analf], 
           main = paste0("% da população referida por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$analf_tabela <- renderTable({
    if (length(input$checkGroup_analf) > 0){
      dadosanalf <- dados[dados$V3001 == "Não" & dados$V2009 >= 15,]
      dadosanalf <- tapply(dadosanalf$one * dadosanalf$V1032,list(dadosanalf$UF,dadosanalf$V2010,dadosanalf$V2007),sum)
      dadosmais15 <- dados[dados$V2009 >= 15,]
      dadosmais15 <- tapply(dadosmais15$one * dadosmais15$V1032,list(dadosmais15$UF,dadosmais15$V2010,dadosmais15$V2007),sum)
      dadosanalf <- ifelse(is.na(dadosanalf), 0, dadosanalf)
      dadosmais15 <- ifelse(is.na(dadosmais15), 0, dadosmais15)
      if (length(input$checkGroup_analf) == 2) {
        dadosanalf <- dadosanalf[,,1] + dadosanalf[,,2]
        dadosmais15 <- dadosmais15[,,1] + dadosmais15[,,2]
      } else if (input$checkGroup_analf == "Homem") {
        dadosanalf <- dadosanalf[,,1]
        dadosmais15 <- dadosmais15[,,1]
      } else {
        dadosanalf <- dadosanalf[,,2]
        dadosmais15 <- dadosmais15[,,2]
      }
      dadosanalf1 <- dadosanalf/dadosmais15
      rm(dadosmais15)
      dadosanalf <- round(dadosanalf,0)
      dadosanalf1 <- round(dadosanalf1,2)
      dadosanalf1 <- as.data.frame(dadosanalf1)
      dadosanalf1$UF <- rownames(dadosanalf1)
      dadosanalf <- cbind(dadosanalf1[,c("UF",input$radio_analf)],dadosanalf[,input$radio_analf])
      rm(dadosanalf1)
      names(dadosanalf) <- c("UF","% da população","analfabetos")
      dadosanalf <- dadosanalf[order(dadosanalf[,"% da população"], decreasing = T),]
      rownames(dadosanalf) <- NULL
      dadosanalf$pos <- paste0(as.character(rownames(dadosanalf)),"º")
      dadosanalf <- dadosanalf[,c("pos","UF","% da população","analfabetos")]
      dadosanalf$analfabetos <- as.integer(dadosanalf$analfabetos)
      print(dadosanalf)
    }
  })
  output$aviso_analf_map <- renderText({
    if (length(input$checkGroup_analf) == 0) {
      print("Para plotar o mapa apenas uma categoria de raça deve estar assinalada...")}
  })
  
  # INSTRUÇÃO -----
  
  output$instrucao <- renderPlot({
    if (length(input$checkGroup_instrucao) > 0){
      dadosinstr <- dados[!is.na(dados$VD3004),]
      dadosinstr <- tapply(dadosinstr$one * dadosinstr$V1032,
                           list(dadosinstr$V2010,dadosinstr$VD3004,dadosinstr$V2007),sum)
      if (length(input$checkGroup_instrucao) == 2) {
        dadosinstr <- dadosinstr[,,1] + dadosinstr[,,2]
      } else if (input$checkGroup_instrucao == "Homem") {
        dadosinstr <- dadosinstr[,,1]
      } else {
        dadosinstr <- dadosinstr[,,2]
      }
      dadosinstr <- as.data.frame(prop.table(t(dadosinstr),2))
      dadosinstr$Ignorado <- NULL
      dadosinstr <- dadosinstr*100
      dadosinstr$instrucao <- rownames(dadosinstr)
      dadosinstr <- dadosinstr %>% gather(raca, proporcao, -instrucao)
      
      varcor <- c(coramarela, corbranca, corindia, corparda, corpreta)
      
      ggplot(data = dadosinstr, aes(x = instrucao, y = proporcao, fill = raca)) + 
        geom_bar(stat = "identity", position="dodge") +
        scale_fill_manual(values = varcor) +
        geom_text(aes(label=round(proporcao,1)), vjust=-1, position=position_dodge(width=1)) +
        ggtitle("Instrução da referida população por raça") +
        ylab("proporção do nível de instrução em cada raça")
    }
  })
  output$aviso_instrucao <- renderText({
    if (length(input$checkGroup_instrucao) == 0) {
      print("Marque ao menos uma categoria de cada opção para que o gráfico seja desenhado...")}
  })
  
  # ANOS DE ESTUDO -----
  
  output$estudo <- renderText({
    print(tolower(input$radio_estudo))
  })
  output$estat_estudo <- renderText({
    dadosestudo <- dados[!is.na(dados$VD3005),]
    dadosestudon <- tapply(dadosestudo$VD3005 * dadosestudo$V1032,list(dadosestudo$V2010,dadosestudo$V2007),sum)
    dadosestudon <- as.data.frame(t(dadosestudon))
    dadosestudod <- tapply(dadosestudo$one * dadosestudo$V1032,list(dadosestudo$V2010,dadosestudo$V2007),sum)
    dadosestudod <- as.data.frame(t(dadosestudod))
    dadosestudo <- dadosestudon/dadosestudod
    print(paste0(round(dadosestudo[1,input$radio_estudo],2)," para os homens e ",
                 round(dadosestudo[2,input$radio_estudo],2)," para as mulheres"))
  })
  output$estudo_map <- renderPlot({
    if (length(input$checkGroup_estudo) > 0){
      dadosestudo <- dados[!is.na(dados$VD3005),]
      dadosestudon <- tapply(dadosestudo$VD3005 * dadosestudo$V1032,
                             list(dadosestudo$estado,dadosestudo$V2010,dadosestudo$V2007),sum)
      dadosestudod <- tapply(dadosestudo$one * dadosestudo$V1032,
                             list(dadosestudo$estado,dadosestudo$V2010,dadosestudo$V2007),sum)
      dadosestudon <- ifelse(is.na(dadosestudon), 0, dadosestudon)
      dadosestudod <- ifelse(is.na(dadosestudod), 0, dadosestudod)
      if (length(input$checkGroup_estudo) == 2) {
        dadosestudon <- dadosestudon[,,1] + dadosestudon[,,2]
        dadosestudod <- dadosestudod[,,1] + dadosestudod[,,2]
      } else if (input$checkGroup_estudo == "Homem") {
        dadosestudon <- dadosestudon[,,1]
        dadosestudod <- dadosestudod[,,1]
      } else {
        dadosestudon <- dadosestudon[,,2]
        dadosestudod <- dadosestudod[,,2]
      }
      dadosestudo <- dadosestudon/dadosestudod
      rm(dadosestudon,dadosestudod)
      dadosestudo <- ifelse(is.na(dadosestudo), 0, dadosestudo)
      dadosestudo <- as.data.frame(dadosestudo)
      dadosestudo$estado <- rownames(dadosestudo)
      dadosestudo <- as.data.frame(dadosestudo[,c(input$radio_estudo, "estado")])
      dadosestudo <- merge(br,dadosestudo,all.x = TRUE)
      
      if (input$radio_estudo == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_estudo == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_estudo == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_estudo == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadosestudo[input$radio_estudo], 
           main = paste0("Anos de estudo da referida população por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$estudo_tabela <- renderTable({
    if (length(input$checkGroup_estudo) > 0){
      dadosestudo <- dados[!is.na(dados$VD3005),]
      dadosestudon <- tapply(dadosestudo$VD3005 * dadosestudo$V1032,
                             list(dadosestudo$UF,dadosestudo$V2010,dadosestudo$V2007),sum)
      dadosestudod <- tapply(dadosestudo$one * dadosestudo$V1032,
                             list(dadosestudo$UF,dadosestudo$V2010,dadosestudo$V2007),sum)
      dadosestudon <- ifelse(is.na(dadosestudon), 0, dadosestudon)
      dadosestudod <- ifelse(is.na(dadosestudod), 0, dadosestudod)
      if (length(input$checkGroup_estudo) == 2) {
        dadosestudon <- dadosestudon[,,1] + dadosestudon[,,2]
        dadosestudod <- dadosestudod[,,1] + dadosestudod[,,2]
      } else if (input$checkGroup_estudo == "Homem") {
        dadosestudon <- dadosestudon[,,1]
        dadosestudod <- dadosestudod[,,1]
      } else {
        dadosestudon <- dadosestudon[,,2]
        dadosestudod <- dadosestudod[,,2]
      }
      dadosestudo <- dadosestudon/dadosestudod
      rm(dadosestudon,dadosestudod)
      dadosestudo <- ifelse(is.na(dadosestudo), 0, dadosestudo)
      dadosestudo <- as.data.frame(dadosestudo)
      dadosestudo$UF <- rownames(dadosestudo)
      dadosestudo <- dadosestudo[,c("UF",input$radio_estudo)]
      dadosestudo <- dadosestudo[order(dadosestudo[,input$radio_estudo], decreasing = T),]
      rownames(dadosestudo) <- NULL
      dadosestudo$pos <- paste0(as.character(rownames(dadosestudo)),"º")
      dadosestudo <- dadosestudo[,c("pos","UF",input$radio_estudo)]
      names(dadosestudo) <- c("pos","UF","Média de anos de Estudo")
      print(dadosestudo)
    }
  })
  output$aviso_estudo_map <- renderText({
    if (length(input$checkGroup_estudo) == 0) {
      print("Para plotar o mapa apenas uma categoria de raça deve estar assinalada...")}
  })
  
  # TAXA DE PARTICIPAÇÃO -----
  
  dadospartic <- dados[dados$VD4001 == "Pessoas fora da força de trabalho" & !is.na(dados$VD4001),]
  dadospartic <- tapply(dadospartic$one * dadospartic$V1032,list(dadospartic$V2010,dadospartic$V2007),sum)
  dadospartic <- as.data.frame(t(dadospartic))
  dadosPIA <- dados[!is.na(dados$VD4001),]
  dadosPIA <- tapply(dadosPIA$one * dadosPIA$V1032,list(dadosPIA$V2010,dadosPIA$V2007),sum)
  dadosPIA <- as.data.frame(t(dadosPIA))
  
  output$partic <- renderText({
    print(tolower(input$radio_partic))
  })
  output$estat_partic <- renderText({
    print(paste0(round(dadospartic[1,input$radio_partic],0)," homens e ",
                 round(dadospartic[2,input$radio_partic],0)," mulheres"))
  })
  output$estat_partic1 <- renderText({
    print(paste0(round(dadospartic[1,input$radio_partic]/dadosPIA[1,input$radio_partic]*100,2)," % entre os homens e ",
                 round(dadospartic[2,input$radio_partic]/dadosPIA[2,input$radio_partic]*100,2)," % entre as mulheres"))
  })
  output$partic_map <- renderPlot({
    if (length(input$checkGroup_partic) > 0){
      dadospartic <- dados[dados$VD4001 == "Pessoas fora da força de trabalho" & !is.na(dados$VD4001),]
      dadospartic <- tapply(dadospartic$one * dadospartic$V1032,list(dadospartic$estado,dadospartic$V2010,dadospartic$V2007),sum)
      dadosPIA <- dados[!is.na(dados$VD4001),]
      dadosPIA <- tapply(dadosPIA$one * dadosPIA$V1032,list(dadosPIA$estado,dadosPIA$V2010,dadosPIA$V2007),sum)
      dadospartic <- ifelse(is.na(dadospartic), 0, dadospartic)
      dadosPIA <- ifelse(is.na(dadosPIA), 0, dadosPIA)
      if (length(input$checkGroup_partic) == 2) {
        dadospartic <- dadospartic[,,1] + dadospartic[,,2]
        dadosPIA <- dadosPIA[,,1] + dadosPIA[,,2]
      } else if (input$checkGroup_partic == "Homem") {
        dadospartic <- dadospartic[,,1]
        dadosPIA <- dadosPIA[,,1]
      } else {
        dadospartic <- dadospartic[,,2]
        dadosPIA <- dadosPIA[,,2]
      }
      dadospartic <- dadospartic/dadosPIA
      rm(dadosPIA)
      dadospartic <- ifelse(is.na(dadospartic), 0, dadospartic)
      dadospartic <- as.data.frame(round(dadospartic*100,2))
      dadospartic$estado <- rownames(dadospartic)
      dadospartic <- as.data.frame(dadospartic[,c(input$radio_partic,"estado")])
      dadospartic <- merge(br,dadospartic,all.x = TRUE)
      
      if (input$radio_partic == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_partic == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_partic == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_partic == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadospartic[input$radio_partic], 
           main = paste0("% Fora da FT na população referida por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$partic_tabela <- renderTable({
    if (length(input$checkGroup_partic) > 0){
      dadospartic <- dados[dados$VD4001 == "Pessoas fora da força de trabalho" & !is.na(dados$VD4001),]
      dadospartic <- tapply(dadospartic$one * dadospartic$V1032,list(dadospartic$UF,dadospartic$V2010,dadospartic$V2007),sum)
      dadosPIA <- dados[!is.na(dados$VD4001),]
      dadosPIA <- tapply(dadosPIA$one * dadosPIA$V1032,list(dadosPIA$UF,dadosPIA$V2010,dadosPIA$V2007),sum)
      dadospartic <- ifelse(is.na(dadospartic), 0, dadospartic)
      dadosPIA <- ifelse(is.na(dadosPIA), 0, dadosPIA)
      if (length(input$checkGroup_partic) == 2) {
        dadospartic <- dadospartic[,,1] + dadospartic[,,2]
        dadosPIA <- dadosPIA[,,1] + dadosPIA[,,2]
      } else if (input$checkGroup_partic == "Homem") {
        dadospartic <- dadospartic[,,1]
        dadosPIA <- dadosPIA[,,1]
      } else {
        dadospartic <- dadospartic[,,2]
        dadosPIA <- dadosPIA[,,2]
      }
      dadospartic1 <- dadospartic/dadosPIA
      rm(dadosPIA)
      dadospartic <- round(dadospartic,0)
      dadospartic1 <- round(dadospartic1*100,)
      dadospartic1 <- as.data.frame(dadospartic1)
      dadospartic1$UF <- rownames(dadospartic1)
      dadospartic <- cbind(dadospartic1[,c("UF",input$radio_partic)],dadospartic[,input$radio_partic])
      rm(dadospartic1)
      names(dadospartic) <- c("UF","% da população","Fora da FT")
      dadospartic <- dadospartic[order(dadospartic[,"% da população"], decreasing = T),]
      rownames(dadospartic) <- NULL
      dadospartic$pos <- paste0(as.character(rownames(dadospartic)),"º")
      dadospartic <- dadospartic[,c("pos","UF","% da população","Fora da FT")]
      dadospartic[,4] <- as.integer(dadospartic[,4])
      print(dadospartic)
    }
  })
  output$aviso_partic <- renderText({
    if (length(input$checkGroup_partic) == 0) {
      print("Marque ao menos uma categoria de cada opção para que o gráfico seja desenhado...")}
  }) 
  
  # TAXA DE DESEMPREGO -----
  
  dadosdesemp <- dados[dados$VD4002 == "Pessoas desocupadas" & !is.na(dados$VD4002),]
  dadosdesemp <- tapply(dadosdesemp$one * dadosdesemp$V1032,list(dadosdesemp$V2010,dadosdesemp$V2007),sum)
  dadosdesemp <- as.data.frame(t(dadosdesemp))
  dadosFT <- dados[!is.na(dados$VD4002),]
  dadosFT <- tapply(dadosFT$one * dadosFT$V1032,list(dadosFT$V2010,dadosFT$V2007),sum)
  dadosFT <- as.data.frame(t(dadosFT))
  
  output$desemp <- renderText({
    print(tolower(input$radio_desemp))
  })
  output$estat_desemp <- renderText({
    print(paste0(round(dadosdesemp[1,input$radio_desemp],0)," homens e ",
                 round(dadosdesemp[2,input$radio_desemp],0)," mulheres"))
  })
  output$estat_desemp1 <- renderText({
    print(paste0(round(dadosdesemp[1,input$radio_desemp]/dadosFT[1,input$radio_desemp]*100,2)," % entre os homens e ",
                 round(dadosdesemp[2,input$radio_desemp]/dadosFT[2,input$radio_desemp]*100,2)," % entre as mulheres"))
  })
  output$desemp_map <- renderPlot({
    if (length(input$checkGroup_desemp) > 0){
      dadosdesemp <- dados[dados$VD4002 == "Pessoas desocupadas" & !is.na(dados$VD4002),]
      dadosdesemp <- tapply(dadosdesemp$one * dadosdesemp$V1032,list(dadosdesemp$estado,dadosdesemp$V2010,dadosdesemp$V2007),sum)
      dadosFT <- dados[!is.na(dados$VD4002),]
      dadosFT <- tapply(dadosFT$one * dadosFT$V1032,list(dadosFT$estado,dadosFT$V2010,dadosFT$V2007),sum)
      dadosdesemp <- ifelse(is.na(dadosdesemp), 0, dadosdesemp)
      dadosFT <- ifelse(is.na(dadosFT), 0, dadosFT)
      if (length(input$checkGroup_desemp) == 2) {
        dadosdesemp <- dadosdesemp[,,1] + dadosdesemp[,,2]
        dadosFT <- dadosFT[,,1] + dadosFT[,,2]
      } else if (input$checkGroup_desemp == "Homem") {
        dadosdesemp <- dadosdesemp[,,1]
        dadosFT <- dadosFT[,,1]
      } else {
        dadosdesemp <- dadosdesemp[,,2]
        dadosFT <- dadosFT[,,2]
      }
      dadosdesemp <- dadosdesemp/dadosFT
      rm(dadosFT)
      dadosdesemp <- ifelse(is.na(dadosdesemp), 0, dadosdesemp)
      dadosdesemp <- as.data.frame(round(dadosdesemp*100,2))
      dadosdesemp$estado <- rownames(dadosdesemp)
      dadosdesemp <- as.data.frame(dadosdesemp[,c(input$radio_desemp,"estado")])
      dadosdesemp <- merge(br,dadosdesemp,all.x = TRUE)
      
      if (input$radio_desemp == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_desemp == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_desemp == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_desemp == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadosdesemp[input$radio_desemp], 
           main = paste0("Taxa de desemprego na população referida por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$desemp_tabela <- renderTable({
    if (length(input$checkGroup_desemp) > 0){
      dadosdesemp <- dados[dados$VD4002 == "Pessoas desocupadas" & !is.na(dados$VD4002),]
      dadosdesemp <- tapply(dadosdesemp$one * dadosdesemp$V1032,list(dadosdesemp$UF,dadosdesemp$V2010,dadosdesemp$V2007),sum)
      dadosFT <- dados[!is.na(dados$VD4002),]
      dadosFT <- tapply(dadosFT$one * dadosFT$V1032,list(dadosFT$UF,dadosFT$V2010,dadosFT$V2007),sum)
      dadosdesemp <- ifelse(is.na(dadosdesemp), 0, dadosdesemp)
      dadosFT <- ifelse(is.na(dadosFT), 0, dadosFT)
      if (length(input$checkGroup_desemp) == 2) {
        dadosdesemp <- dadosdesemp[,,1] + dadosdesemp[,,2]
        dadosFT <- dadosFT[,,1] + dadosFT[,,2]
      } else if (input$checkGroup_desemp == "Homem") {
        dadosdesemp <- dadosdesemp[,,1]
        dadosFT <- dadosFT[,,1]
      } else {
        dadosdesemp <- dadosdesemp[,,2]
        dadosFT <- dadosFT[,,2]
      }
      dadosdesemp1 <- dadosdesemp/dadosFT
      rm(dadosFT)
      dadosdesemp <- round(dadosdesemp,0)
      dadosdesemp1 <- round(dadosdesemp1*100,)
      dadosdesemp1 <- as.data.frame(dadosdesemp1)
      dadosdesemp1$UF <- rownames(dadosdesemp1)
      dadosdesemp <- cbind(dadosdesemp1[,c("UF",input$radio_desemp)],dadosdesemp[,input$radio_desemp])
      rm(dadosdesemp1)
      names(dadosdesemp) <- c("UF","% da população","Desempregados")
      dadosdesemp <- dadosdesemp[order(dadosdesemp[,"% da população"], decreasing = T),]
      rownames(dadosdesemp) <- NULL
      dadosdesemp$pos <- paste0(as.character(rownames(dadosdesemp)),"º")
      dadosdesemp <- dadosdesemp[,c("pos","UF","% da população","Desempregados")]
      dadosdesemp[,4] <- as.integer(dadosdesemp[,4])
      print(dadosdesemp)
    }
  })
  output$aviso_desemp <- renderText({
    if (length(input$checkGroup_desemp) == 0) {
      print("Marque ao menos uma categoria de cada opção para que o gráfico seja desenhado...")}
  })
  
  # TAXA DE INFORMALIDADE -----
  
  dadosinformal <- dados[!is.na(dados$VD4009) & 
                           (dados$VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                              dados$VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada" |
                              (!is.na(dados$V4019) &
                                 ((dados$VD4009 == "Empregador" & dados$V4019 == "Não") |
                                    (dados$VD4009 == "Conta-própria" & dados$V4019 == "Não"))) |
                              dados$VD4009 == "Trabalhador familiar auxiliar"),]
  dadosinformal <- tapply(dadosinformal$one * dadosinformal$V1032,list(dadosinformal$V2010,dadosinformal$V2007),sum)
  dadosinformal <- as.data.frame(t(dadosinformal))
  dadosocup <- dados[dados$VD4002 == "Pessoas ocupadas" & !is.na(dados$VD4002),]
  dadosocup <- tapply(dadosocup$one * dadosocup$V1032,list(dadosocup$V2010,dadosocup$V2007),sum)
  dadosocup <- as.data.frame(t(dadosocup))
  
  output$informal <- renderText({
    print(tolower(input$radio_informal))
  })
  output$estat_informal <- renderText({
    print(paste0(round(dadosinformal[1,input$radio_informal],0)," homens e ",
                 round(dadosinformal[2,input$radio_informal],0)," mulheres"))
  })
  output$estat_informal1 <- renderText({
    print(paste0(round(dadosinformal[1,input$radio_informal]/dadosFT[1,input$radio_informal]*100,2)," % entre os homens e ",
                 round(dadosinformal[2,input$radio_informal]/dadosFT[2,input$radio_informal]*100,2)," % entre as mulheres"))
  })
  output$informal_map <- renderPlot({
    if (length(input$checkGroup_informal) > 0){
      dadosinformal <- dados[!is.na(dados$VD4009) & 
                               (dados$VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                                  dados$VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada" |
                                  (!is.na(dados$V4019) &
                                     ((dados$VD4009 == "Empregador" & dados$V4019 == "Não") |
                                        (dados$VD4009 == "Conta-própria" & dados$V4019 == "Não"))) |
                                  dados$VD4009 == "Trabalhador familiar auxiliar"),]
      dadosinformal <- tapply(dadosinformal$one * dadosinformal$V1032,
                              list(dadosinformal$estado,dadosinformal$V2010,dadosinformal$V2007),sum)
      dadosocup <- dados[dados$VD4002 == "Pessoas ocupadas" & !is.na(dados$VD4002),]
      dadosocup <- tapply(dadosocup$one * dadosocup$V1032,list(dadosocup$estado,dadosocup$V2010,dadosocup$V2007),sum)
      dadosinformal <- ifelse(is.na(dadosinformal), 0, dadosinformal)
      dadosocup <- ifelse(is.na(dadosocup), 0, dadosocup)
      if (length(input$checkGroup_informal) == 2) {
        dadosinformal <- dadosinformal[,,1] + dadosinformal[,,2]
        dadosocup <- dadosocup[,,1] + dadosocup[,,2]
      } else if (input$checkGroup_informal == "Homem") {
        dadosinformal <- dadosinformal[,,1]
        dadosocup <- dadosocup[,,1]
      } else {
        dadosinformal <- dadosinformal[,,2]
        dadosocup <- dadosocup[,,2]
      }
      dadosinformal <- dadosinformal/dadosocup
      rm(dadosocup)
      dadosinformal <- ifelse(is.na(dadosinformal), 0, dadosinformal)
      dadosinformal <- as.data.frame(round(dadosinformal*100,2))
      dadosinformal$estado <- rownames(dadosinformal)
      dadosinformal <- as.data.frame(dadosinformal[,c(input$radio_informal,"estado")])
      dadosinformal <- merge(br,dadosinformal,all.x = TRUE)
      
      if (input$radio_informal == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_informal == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_informal == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_informal == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadosinformal[input$radio_informal], 
           main = paste0("Taxa de informalidade na população referida por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$informal_tabela <- renderTable({
    if (length(input$checkGroup_informal) > 0){
      dadosinformal <- dados[!is.na(dados$VD4009) & 
                               (dados$VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" |
                                  dados$VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada" |
                                  (!is.na(dados$V4019) &
                                     ((dados$VD4009 == "Empregador" & dados$V4019 == "Não") |
                                        (dados$VD4009 == "Conta-própria" & dados$V4019 == "Não"))) |
                                  dados$VD4009 == "Trabalhador familiar auxiliar"),]
      dadosinformal <- tapply(dadosinformal$one * dadosinformal$V1032,
                              list(dadosinformal$UF,dadosinformal$V2010,dadosinformal$V2007),sum)
      dadosocup <- dados[dados$VD4002 == "Pessoas ocupadas" & !is.na(dados$VD4002),]
      dadosocup <- tapply(dadosocup$one * dadosocup$V1032,list(dadosocup$UF,dadosocup$V2010,dadosocup$V2007),sum)
      dadosinformal <- ifelse(is.na(dadosinformal), 0, dadosinformal)
      dadosocup <- ifelse(is.na(dadosocup), 0, dadosocup)
      if (length(input$checkGroup_informal) == 2) {
        dadosinformal <- dadosinformal[,,1] + dadosinformal[,,2]
        dadosocup <- dadosocup[,,1] + dadosocup[,,2]
      } else if (input$checkGroup_informal == "Homem") {
        dadosinformal <- dadosinformal[,,1]
        dadosocup <- dadosocup[,,1]
      } else {
        dadosinformal <- dadosinformal[,,2]
        dadosocup <- dadosocup[,,2]
      }
      dadosinformal1 <- dadosinformal/dadosocup
      rm(dadosocup)
      dadosinformal <- round(dadosinformal,0)
      dadosinformal1 <- round(dadosinformal1*100,)
      dadosinformal1 <- as.data.frame(dadosinformal1)
      dadosinformal1$UF <- rownames(dadosinformal1)
      dadosinformal <- cbind(dadosinformal1[,c("UF",input$radio_informal)],dadosinformal[,input$radio_informal])
      rm(dadosinformal1)
      names(dadosinformal) <- c("UF","% da população","Informais")
      dadosinformal <- dadosinformal[order(dadosinformal[,"% da população"], decreasing = T),]
      rownames(dadosinformal) <- NULL
      dadosinformal$pos <- paste0(as.character(rownames(dadosinformal)),"º")
      dadosinformal <- dadosinformal[,c("pos","UF","% da população","Informais")]
      dadosinformal[,4] <- as.integer(dadosinformal[,4])
      print(dadosinformal)
    }
  })
  output$aviso_informal <- renderText({
    if (length(input$checkGroup_informal) == 0) {
      print("Marque ao menos uma categoria de cada opção para que o gráfico seja desenhado...")}
  })
  
      
  # DESALENTO -----
  
  dadosdesal <- dados[dados$VD4005 == "Pessoas desalentadas" & !is.na(dados$VD4005),]
  dadosdesal <- tapply(dadosdesal$one * dadosdesal$V1032,list(dadosdesal$V2010,dadosdesal$V2007),sum)
  dadosdesal <- as.data.frame(t(dadosdesal))
  dadosFFT <- dados[dados$VD4001 == "Pessoas fora da força de trabalho" & !is.na(dados$VD4001),]
  dadosFFT <- tapply(dadosFFT$one * dadosFFT$V1032,list(dadosFFT$V2010,dadosFFT$V2007),sum)
  dadosFFT <- as.data.frame(t(dadosFFT))

  output$desal <- renderText({
    print(tolower(input$radio_desal))
  })
  output$estat_desal <- renderText({
    print(paste0(round(dadosdesal[1,input$radio_desal],0)," homens e ",
                 round(dadosdesal[2,input$radio_desal],0)," mulheres"))
  })
  output$estat_desal1 <- renderText({
    print(paste0(round(dadosdesal[1,input$radio_desal]/dadosFFT[1,input$radio_desal]*100,2)," % entre os homens e ",
                 round(dadosdesal[2,input$radio_desal]/dadosFFT[2,input$radio_desal]*100,2)," % entre as mulheres"))
  })
  output$desal_map <- renderPlot({
    if (length(input$checkGroup_desal) > 0){
      dadosdesal <- dados[dados$VD4005 == "Pessoas desalentadas" & !is.na(dados$VD4005),]
      dadosdesal <- tapply(dadosdesal$one * dadosdesal$V1032,list(dadosdesal$estado,dadosdesal$V2010,dadosdesal$V2007),sum)
      dadosFFT <- dados[!is.na(dados$VD4001),]
      dadosFFT <- tapply(dadosFFT$one * dadosFFT$V1032,list(dadosFFT$estado,dadosFFT$V2010,dadosFFT$V2007),sum)
      dadosdesal <- ifelse(is.na(dadosdesal), 0, dadosdesal)
      dadosFFT <- ifelse(is.na(dadosFFT), 0, dadosFFT)
      if (length(input$checkGroup_desal) == 2) {
        dadosdesal <- dadosdesal[,,1] + dadosdesal[,,2]
        dadosFFT <- dadosFFT[,,1] + dadosFFT[,,2]
      } else if (input$checkGroup_desal == "Homem") {
        dadosdesal <- dadosdesal[,,1]
        dadosFFT <- dadosFFT[,,1]
      } else {
        dadosdesal <- dadosdesal[,,2]
        dadosFFT <- dadosFFT[,,2]
      }
      dadosdesal <- dadosdesal/dadosFFT
      rm(dadosFFT)
      dadosdesal <- ifelse(is.na(dadosdesal), 0, dadosdesal)
      dadosdesal <- as.data.frame(round(dadosdesal*100,2))
      dadosdesal$estado <- rownames(dadosdesal)
      dadosdesal <- as.data.frame(dadosdesal[,c(input$radio_desal,"estado")])
      dadosdesal <- merge(br,dadosdesal,all.x = TRUE)
      
      if (input$radio_desal == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_desal == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_desal == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_desal == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadosdesal[input$radio_desal], 
           main = paste0("Taxa de desalento na população referida por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$desal_tabela <- renderTable({
    if (length(input$checkGroup_desal) > 0){
      dadosdesal <- dados[dados$VD4005 == "Pessoas desalentadas" & !is.na(dados$VD4005),]
      dadosdesal <- tapply(dadosdesal$one * dadosdesal$V1032,list(dadosdesal$UF,dadosdesal$V2010,dadosdesal$V2007),sum)
      dadosFFT <- dados[!is.na(dados$VD4001),]
      dadosFFT <- tapply(dadosFFT$one * dadosFFT$V1032,list(dadosFFT$UF,dadosFFT$V2010,dadosFFT$V2007),sum)
      dadosdesal <- ifelse(is.na(dadosdesal), 0, dadosdesal)
      dadosFFT <- ifelse(is.na(dadosFFT), 0, dadosFFT)
      if (length(input$checkGroup_desal) == 2) {
        dadosdesal <- dadosdesal[,,1] + dadosdesal[,,2]
        dadosFFT <- dadosFFT[,,1] + dadosFFT[,,2]
      } else if (input$checkGroup_desal == "Homem") {
        dadosdesal <- dadosdesal[,,1]
        dadosFFT <- dadosFFT[,,1]
      } else {
        dadosdesal <- dadosdesal[,,2]
        dadosFFT <- dadosFFT[,,2]
      }
      dadosdesal1 <- dadosdesal/dadosFFT
      rm(dadosFFT)
      dadosdesal <- round(dadosdesal,0)
      dadosdesal1 <- round(dadosdesal1*100,)
      dadosdesal1 <- as.data.frame(dadosdesal1)
      dadosdesal1$UF <- rownames(dadosdesal1)
      dadosdesal <- cbind(dadosdesal1[,c("UF",input$radio_desal)],dadosdesal[,input$radio_desal])
      rm(dadosdesal1)
      names(dadosdesal) <- c("UF","% da população","desalentados")
      dadosdesal <- dadosdesal[order(dadosdesal[,"% da população"], decreasing = T),]
      rownames(dadosdesal) <- NULL
      dadosdesal$pos <- paste0(as.character(rownames(dadosdesal)),"º")
      dadosdesal <- dadosdesal[,c("pos","UF","% da população","desalentados")]
      dadosdesal[,4] <- as.integer(dadosdesal[,4])
      print(dadosdesal)
    }
  })
  output$aviso_desal <- renderText({
    if (length(input$checkGroup_desal) == 0) {
      print("Marque ao menos uma categoria de cada opção para que o gráfico seja desenhado...")}
  })

  # RENDIMENTOS -----
  
  output$rend <- renderText({
    print(tolower(input$radio_rend))
  })
  output$estat_rend <- renderText({
    dadosrend <- dados[!is.na(dados$VD4020),]
    dadosrendn <- tapply(dadosrend$VD4020*dadosrend$V1032,list(dadosrend$V2010,dadosrend$V2007),sum)
    dadosrendn <- as.data.frame(t(dadosrendn))
    dadosrendd <- tapply(dadosrend$one*dadosrend$V1032,list(dadosrend$V2010,dadosrend$V2007),sum)
    dadosrendd <- as.data.frame(t(dadosrendd))
    dadosrend <- dadosrendn/dadosrendd
    print(paste0("R$ ",round(dadosrend[1,input$radio_rend],2)," para os homens e R$ ",
                 round(dadosrend[2,input$radio_rend],2)," para as mulheres"))
  })
  output$rend_map <- renderPlot({
    if (length(input$checkGroup_rend) > 0){
      dadosrend <- dados[!is.na(dados$VD4020),]
      dadosrendn <- tapply(dadosrend$VD4020*dadosrend$V1032,list(dadosrend$estado,dadosrend$V2010,dadosrend$V2007),sum)
      dadosrendd <- tapply(dadosrend$one*dadosrend$V1032,list(dadosrend$estado,dadosrend$V2010,dadosrend$V2007),sum)
      dadosrendn <- ifelse(is.na(dadosrendn), 0, dadosrendn)
      dadosrendd <- ifelse(is.na(dadosrendd), 0, dadosrendd)
      if (length(input$checkGroup_rend) == 2) {
        dadosrendn <- dadosrendn[,,1] + dadosrendn[,,2]
        dadosrendd <- dadosrendd[,,1] + dadosrendd[,,2]
      } else if (input$checkGroup_rend == "Homem") {
        dadosrendn <- dadosrendn[,,1]
        dadosrendd <- dadosrendd[,,1]
      } else {
        dadosrendn <- dadosrendn[,,2]
        dadosrendd <- dadosrendd[,,2]
      }
      dadosrend <- dadosrendn/dadosrendd
      rm(dadosrendn,dadosrendd)
      dadosrend <- ifelse(is.na(dadosrend), 0, dadosrend)
      dadosrend <- as.data.frame(dadosrend)
      dadosrend$estado <- rownames(dadosrend)
      dadosrend <- as.data.frame(dadosrend[,c(input$radio_rend, "estado")])
      dadosrend <- merge(br,dadosrend,all.x = TRUE)
      
      if (input$radio_rend == "Preta"){
        paleta <- c("white", corpreta)
      } else if(input$radio_rend == "Parda") {
        paleta <- c("white", corparda)
      } else if(input$radio_rend == "Branca") {
        paleta <- c("white", corbranca)
      } else if(input$radio_rend == "Amarela") {
        paleta <- c("white", coramarela)
      } else {
        paleta <- c("white", corindia)
      }
      
      plot(dadosrend[input$radio_rend], 
           main = paste0("Rendimento médio da referida população por UF"),
           pal = colorRampPalette(paleta))
    }
  })
  output$rend_tabela <- renderTable({
    if (length(input$checkGroup_rend) > 0){
      dadosrend <- dados[!is.na(dados$VD4020),]
      dadosrendn <- tapply(dadosrend$VD4020*dadosrend$V1032,list(dadosrend$UF,dadosrend$V2010,dadosrend$V2007),sum)
      dadosrendd <- tapply(dadosrend$one*dadosrend$V1032,list(dadosrend$UF,dadosrend$V2010,dadosrend$V2007),sum)
      dadosrendn <- ifelse(is.na(dadosrendn), 0, dadosrendn)
      dadosrendd <- ifelse(is.na(dadosrendd), 0, dadosrendd)
      if (length(input$checkGroup_rend) == 2) {
        dadosrendn <- dadosrendn[,,1] + dadosrendn[,,2]
        dadosrendd <- dadosrendd[,,1] + dadosrendd[,,2]
      } else if (input$checkGroup_rend == "Homem") {
        dadosrendn <- dadosrendn[,,1]
        dadosrendd <- dadosrendd[,,1]
      } else {
        dadosrendn <- dadosrendn[,,2]
        dadosrendd <- dadosrendd[,,2]
      }
      dadosrend <- dadosrendn/dadosrendd
      rm(dadosrendn,dadosrendd)
      dadosrend <- ifelse(is.na(dadosrend), 0, dadosrend)
      dadosrend <- as.data.frame(dadosrend)
      dadosrend$UF <- rownames(dadosrend)
      dadosrend <- dadosrend[,c("UF",input$radio_rend)]
      dadosrend <- dadosrend[order(dadosrend[,input$radio_rend], decreasing = T),]
      rownames(dadosrend) <- NULL
      dadosrend$pos <- paste0(as.character(rownames(dadosrend)),"º")
      dadosrend <- dadosrend[,c("pos","UF",input$radio_rend)]
      names(dadosrend) <- c("pos","UF","Rendimento médio")
      print(dadosrend)
    }
  })
  output$aviso_rend_map <- renderText({
    if (length(input$checkGroup_rend) == 0) {
      print("Para plotar o mapa apenas uma categoria de raça deve estar assinalada...")}
  })
  
  
}

shinyApp(ui, server)