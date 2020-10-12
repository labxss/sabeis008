#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(scipen=999) # avoid scientific notation

ds=read.csv(file="vw_sia_am_coorte_202009302136.csv")
sup=read.csv(file="td_diretriz_202010011724.csv")[,c("no_pasta","no_diretriz50","co_cid_hash","sg_tipo","ds_url","ds_publicacao")]
sup$ds_url <- paste0("<a href='",sup$ds_url,"'>",sup$ds_publicacao,"</a>")
sup$sg_tipo=toupper(sup$sg_tipo)
sup$ds_publicacao=NULL
colnames(sup)=c("no_coorte","no_dirertriz","co_cid","sg_tipo","ds_publicacao")

biologicos=c(604320140, 604320124, 3650104, 601010019, 604380011, 604380062, 
             604380097, 604380070, 604320159, 3650103, 601010027, 604380020,
             601010051, 604380038, 604380089, 3607111, 3650101, 3650102, 
             601010035, 601010043, 604380046, 604380054,
             604680023, 604690010, 604690029)

metricas = c(
  'registros'='qt_registros', 
  'usuários segundo Cartão Nacional de Saúde'='qt_cnspcn', 
  'usuários com idade <=18'='qt_cnspcn_menor18', 
  'usuárias do sexo feminino'='qt_cnspcn_sexo_f', 
  'APAC distintas (número de autorização de procedimento ambulatorial)'='qt_apac', 
  'estabelecimentos CNES distintos'='qt_cnes', 
  'municípios de residência distintos'='qt_municipio_residencia', 
  'quantidade aprovada'='qt_aprovada', 
  'valor aprovado'='vl_aprovado', 
  'valor parovado multiplicado pela mediana unitária do valor aprovado'='vl_aprovado_unitario_mediana'
)

coorte=c(
  'Acidente Vascular Cerebral (AVC) - Trombólise' = 'acidente_vascular',
  'Acromegalia' = 'acromegalia',
  'Adenocarcinoma de Próstata' = 'c_prostata',
  'Anemia Aplástica Adquirida' = 'anemia_aplastica_adquirida',
  'Anemia Aplástica, Mielodisplasia e Neutropenia' = 'anemia_aplastica',
  'Anemia em pacientes com Insuficiência Renal' = 'anemia_renal',
  'Anemia Hemolítica Autoimune' = 'anemia_hemolitica',
  'Anemia por Deficiência de Ferro' = 'anemia_ferro',
  'Angioedema' = 'angiodema',
  'Aplasia Pura Adquirida Crônica da série vermelha' = 'aplasia',
  'Artrite Psoríaca' = 'artrite_psoriaca',
  'Artrite Reativa' = 'artrite_reativa',
  'Artrite Reumatoide e Artrite Idiopática Juvenil' = 'artrite_reumatoide',
  'Asma' = 'asma',
  'Atrofia Muscular Espinhal' = 'atrofia',
  'Câncer de Cabeça e Pescoço' = 'c_cabeca',
  'Câncer de Cólon e Reto' = 'c_colon',
  'Câncer de Estômago' = 'c_estomago',
  'Câncer de Fígado - Adultos' = 'c_figado',
  'Câncer de Pulmão' = 'c_pulmao',
  'Carcinoma de Células Renais' = 'c_renal',
  'Carcinoma de Esôfago' = 'c_esofago',
  'Carcinoma de Mama' = 'c_mama',
  'Carcinoma Diferenciado de Tireóide' = 'c_tireoide',
  'Colangite Biliar Primária' = 'colangite',
  'Comportamento Agressivo no Autismo' = 'autismo',
  'Deficiência de Biotinidase' = 'biotinidase',
  'Deficiência de Hormônio do Crescimento - Hipopitui' = 'hipopituitarismo',
  'Degeneração Macular Relacionada com a Idade' = 'macular',
  'Dermatomiosite e Polimiosite' = 'dermatomiosite',
  'Diabete Melito Tipo 1' = 'diabetes_melito',
  'Diabetes Insípido' = 'diabetes_insipido',
  'Dislipidemia para a Prevenção de Eventos Cardiov.' = 'dislipidemia',
  'Distonias focais e Espasmo Hemifacial' = 'distonias',
  'Distúrbio Mineral Ósseo na Doença Renal Crônica' = 'osseo',
  'Doença Celíaca' = 'celiaca',
  'Doença de Alzheimer' = 'alzheimer',
  'Doença de Chagas' = 'chagas',
  'Doença de Crohn' = 'crohn',
  'Doença de Gaucher' = 'gaucher',
  'Doença de Paget' = 'paget',
  'Doença de Parkinson' = 'parkinson',
  'Doença de Wilson' = 'wilson',
  'Doença Falciforme' = 'falciforme',
  'Doença Pulmonar Obstrutiva Crônica (Retificado em' = 'dpoc',
  'Dor Crônica' = 'dor_cronica',
  'Endometriose' = 'endometriose',
  'Epilepsia' = 'epilepsia',
  'Esclerose Lateral Amiotrófica' = 'esclerose_amiotrofica',
  'Esclerose Múltipla' = 'esclerose_multipla',
  'Esclerose Sistêmica' = 'esclerose_sistemica',
  'Espasticidade' = 'espasticidade',
  'Espondilite Ancilosante' = 'espondilite',
  'Espondilose' = 'espondilose',
  'Esquizofrenia' = 'esquizofrenia',
  'Fenilcetonúria' = 'fenilcetonuria',
  'Fibrose Cística - Insuficiência Pancreática' = 'fibrose_cistica_pancreatica',
  'Fibrose Cística - Manifestações Pulmonares' = 'fibrose_cistica_pulmonar',
  'Glaucoma' = 'glaucoma',
  'Hemangioma Infantil' = 'hemangioma',
  'Hemoglobinúria Paroxística Noturna' = 'hemoglobinuria',
  'Hepatite Autoimune' = 'hepatite_autoimune',
  'Hepatite B e Coinfecções' = 'hepatite_b',
  'Hepatite C e coinfecções' = 'hepatite_c',
  'Hidradenite Supurativa' = 'hidradenite',
  'Hiperplasia Adrenal Congênita' = 'adrenal_congenita',
  'Hiperprolactinemia' = 'hiperprolactinemia',
  'Hipertensão Arterial Pulmonar' = 'hipertensao_pulmonar',
  'Hipoparatireoidismo' = 'hipopparatireoidismo',
  'Hipotireoidismo Congênito' = 'hipotireoidismo',
  'Homocistinúria Clássica' = 'homocistinuria',
  'Ictioses Hereditárias' = 'ictiose',
  'Imunodeficiência Primária' = 'imunosuficiencia',
  'Imunossupressão no Transplante Hepático em Adulto' = 'hepatico',
  'Imunossupressão no Transplante Renal (Republicado' = 'renal',
  'Incontinência Urinária não Neurogênica' = 'incontinencia',
  'Insuficiência Adrenal Primária' = 'adrenal_primaria',
  'Insuficiência Pancreática Exócrina' = 'insuficiencia_pancreatica',
  'Leimioma de útero' = 'leimioma',
  'Leucemia Linfoblástica Aguda Cromossoma Philadelph' = 'c_leucemia_linfoblastica',
  'Leucemia Mieloide Aguda - Adultos e Leucemia Mielo' = 'c_leucemia_mieloide',
  'Leucemia Mieloide Crônica - Adultos e Leucemia Mie' = 'leucemia_mieloide',
  'Linfoma Difuso de Grandes Células B - Adultos' = 'c_celula_b',
  'Linfoma Folicular' = 'c_folicular',
  'Lúpus Eritematoso Sistêmico' = 'lupus',
  'Melanoma Cutâneo' = 'c_cutaneo',
  'Miastenia Gravis' = 'miastenia',
  'Mieloma Múltiplo' = 'c_mieloma',
  'Mucopolissacaridose tipo II' = 'mucopolissacaridose2',
  'Mucopolissacaridose tipo I' = 'mucopolissacaridose1',
  'Mucopolissacaridose tipo IV A e Mucopolissacaridos' = 'mucopolissacaridose',
  'Neoplasia Maligna Epitelial de Ovário' = 'c_ovario',
  'Osteogênese Imperfeita' = 'osteogenese',
  'Osteoporose' = 'osteoporose',
  'Polineuropatia Amiloidótica Familiar' = 'polineuropatia',
  'Profilaxia da reinfecção pelo Vírus da Hepatite B' = 'hepatite_b_transp',
  'Psoríase' = 'psoriase',
  'Puberdade Precoce Central' = 'puberdade',
  'Púrpura Trombocitopênica Idiopática' = 'purpura',
  'Raquitismo e Osteomalácia' = 'raquitismo',
  'Retocolite Ulcerativa' = 'retocolite',
  'Síndrome de Guillain-Barré' = 'guillain_barre',
  'Síndrome de Ovários Policísticos' = 'ovario_policisticos',
  'Síndrome de Turner' = 'turner',
  'Síndrome Hipereosinofílica com mesilato de imatini' = 'hipereosinofilica',
  'Síndrome Nefrótica Primária em Crianças e Adolesce' = 'nefrotica',
  'Sobrecarga de Ferro' = 'sobrecarga_ferro',
  'Síndrome Respiratória Agura Grave - SRAG' = 'srag',
  'Tabagismo' = 'tabagismo',
  'Transtorno Afetivo Bipolar do tipo I' = 'bipolar',
  'Transtorno Esquizoafetivo' = 'esquizoafetivo',
  'Tromboembolismo Venoso em Gestantes com Trombofili' = 'tromboembolismo',
  'Tumor Cerebral - Adultos' = 'c_cerebral',
  'Tumor do Estroma Gastrointestinal' = 'c_gastro',
  'Uveítes Não-Infecciosas' = 'uveite'
)

apresentacao = c(
  "quantitativo" = "(n)",
  "percentual" = "(%)"
)

library(shiny)
library(reshape2)
library(DT) # datatable
library(ggplot2)
library(dplyr)
library(qcc)


library(forecast) # arima
library(tseries) # arima

library(gtools) # smart rbind

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
        "SABEIS - Sala Aberta de Situação em Saúde", windowTitle = "SABEIS"
    ),
    
    tags$h1("Medicamentos"),
    
    tabsetPanel(
      
      type = "tabs",
      tabPanel(
        "Início", 
        tags$h3("Apresentação"),
        
        tags$p("Bem vindo à plataforma SABEIS."),
        
        tags$p("Este módulo é dedicato à extração de informações básicas de medicamentos para doenças com diretriz CONITEC."),
        
        tags$h3("Instruções"),
        
        tags$p("Selecione uma doença e o período."),
        
        
      ), # tabPanel inicio
      
      tabPanel(
        "Critério",
        
        tags$h3("Critério de inclusão"),
        
        tags$p(
          "Os documentos clínicos da CONITEC - Comissão Nacional de Incorporação de Tecnologias de Saúde do SUS foram obtidos em ",
          tags$a(href="http://conitec.gov.br/index.php/protocolos-e-diretrizes", "http://conitec.gov.br/index.php/protocolos-e-diretrizes"),
          "."
        ),
        
        tags$p("Para cada documento foram coletados os diagnósticos (co_cid) referentes à Classificação Internacional de Doenças, CID-10."),
        
        
        tags$p("A coorte (no_coorte) é obtida a partir do CID primário ou secundário do registro da guia APAC - Autorização de Procedimento Ambulatorial (Alta complexidade/custo) de laudo de medicamentos (AM) dos Sistema de Informação Ambulatorial (SIA)."),
        
        DT::dataTableOutput("td_diretriz_202010011724")
      ),
      
      tabPanel(
        "Sobre",
        
        tags$h3("Fonte"),
        
        tags$p(
          "Dados baixados a partir do  ",
          tags$a(href="ftp://ftp.datasus.gov.br/dissemin/publicos/", "ftp.datasus.gov.br"),
          "em 28/08/2020."
        ),
        
        tags$h3("Citação"),
        
        tags$p('Ferre, F.; de Oliveira, G. L. A.; de Queiroz, M. J. & Gonçalves, F. (2020), 
           Sala de Situação aberta com dados administrativos para gestão de Protocolos Clínicos e Diretrizes Terapêuticas de tecnologias providas pelo SUS, 
           in SBCAS 2020.'),
        
        tags$h3("Código-fonte"),
        
        tags$p(
          "Baixe o arquivo RShiny em ",
          tags$a(href="https://github.com/labxss/sabeis007", "https://github.com/labxss/sabeis007"),
          "."
        ),
        
      ) # tabPanel sobre
      
    ),
    
    
   
    
    tags$hr(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            selectizeInput(
                inputId = 'inCoorte', 
                label = 'Escolha uma doença (digite ou liste)', 
                choices=coorte,
                selected = 'artrite_psoriaca'
            ),
            
            sliderInput(
                inputId = 'inAno',
                label = 'Período',
                min = 2008,
                max = 2020,
                sep = "",
                value=c(2008,2020)
            ),
            
            radioButtons(
                "inCampos", 
                "Quantificadores",
                metricas,
            ),

            radioButtons(
                "inMedicamentos", 
                "Mesmos medicamentos para outras doenças",
                c("Não (apenas registros da coorte selecionada)",
                  "Sim (coletar para todas as coortes)")
            ),
            
            radioButtons(
                "inTipo", 
                "Tipo",
                c(
                  "biológicos",
                  "não biológicos",
                  "biológicos e não biológicos")
            ),
            
            radioButtons(
              "inApresentacao", 
              "Apresentação:",
              apresentacao,
              inline=T
            )
            
        ), # sidebarPanel
        
        # Show a plot of the generated distribution
        mainPanel(
          tags$h3("Painel de dados agregados"),
          br(),
          htmlOutput("barplot_market_share_title"),
          plotOutput("barplot_market_share"),
          hr(),
          htmlOutput("pareto_title"),
          plotOutput("pareto"),
          tags$p("* Correspondente à soma acumulada do quantitativo ao ano no período, não ao número de casos distintos."),
          hr(),
          htmlOutput("arima_title"),
          plotOutput("arima"),
          tags$p("* Correspondente à soma acumulada do quantitativo ao ano no período, não ao número de casos distintos."),
          DT::dataTableOutput("sds_arima"),
          hr(),
          tags$h3("Previsão por medicamento"),
          htmlOutput("td_pcdt_abrev"),
          plotOutput("arima_abrev"),
          DT::dataTableOutput("sds_arima_abrev"),
          hr(),
          tags$h3("Conjunto de dados"),
          DT::dataTableOutput("sds")
          
        )
        
    ) # sidebarLayout
) # fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   output$rb_apresentacao = renderText(
     
     paste("Dados de (", input$inAno[1], "-", input$inAno[2],")",input$inCampos)
     
   )
   
   # ------------------------------------------
   reactive_sds = reactive({
     if (input$inMedicamentos == "Não (apenas registros da coorte selecionada)"){
       sds = subset(
         ds, 
         nu_ano_competencia >= input$inAno[1] & 
           nu_ano_competencia <= input$inAno[2] &
           no_coorte == input$inCoorte 
       )[,c(
         "co_evento",
         "nu_ano_competencia", 
         "sg_procedimento",
         input$inCampos
       ), drop = FALSE]
     } else {
       
       evento = unique(subset(
         ds, 
         nu_ano_competencia >= input$inAno[1] & 
           nu_ano_competencia <= input$inAno[2] &
           no_coorte == input$inCoorte 
       )[,"co_evento"])
       
       sds = subset(
         ds, 
         nu_ano_competencia >= input$inAno[1] & 
           nu_ano_competencia <= input$inAno[2] &
           co_evento %in% evento
       )[,c(
         "co_evento",
         "nu_ano_competencia", 
         "sg_procedimento",
         input$inCampos
       ), drop = FALSE]
       
     }
     
     
     if (input$inTipo == "biológicos"){
       sds = subset(
         sds, 
         co_evento %in% biologicos
       )
     }
     
     if (input$inTipo == "não biológicos"){
       sds = subset(
         sds, 
         !co_evento %in% biologicos
       )
     }
     
     colnames(sds)=c(
       "co_evento",
       "ano",
       "categoria", 
       "quantidade")
     
     sds = sds %>%
       group_by(co_evento, ano, categoria) %>%
       summarise(quantidade=sum(quantidade))
     
     # sds$categoria=as.character(sds$categoria)
     # 
     # top10=as.character(sds_categoria[order(-sds_categoria$quantidade),"categoria"][1:10])
     # 
     # sds[which(!sds$categoria %in% top10),"categoria"] = " outros"
     # 
     # sds = sds %>%
     #   group_by(ano, categoria) %>%
     #   summarise(quantidade=sum(quantidade))
     
     sds$categoria=as.factor(sds$categoria)  
     
     
     sdsano = sds %>%
       group_by(ano) %>%
       summarise(soma_ano=sum(quantidade))
     
     
     sds = merge(sds, sdsano, by=c("ano"))
     
     sds$porcento=round(sds$quantidade/sds$soma_ano*100,1)
     
     sds$ano=as.factor(sds$ano)
     sds$categoria=as.factor(sds$categoria)  
     
     sds$quantitativo=format(round(sds$quantidade,1), big.mark=".", decimal.mark=",")
     sds$percentual=format(round(sds$porcento,1), big.mark=".", decimal.mark=",")
     
     sds$nu_ano=as.numeric(sds$ano)
     
     return(as.data.frame(sds))
     
   })
   
   # ---------------------------------------------------------------------------
   
   output$td_pcdt_abrev <- renderUI({
     
     sds = reactive_sds()
     
     # available <- as.matrix(subset(sds, co_pcdt == input$inPCDT)$sg_procedimento)
     # row.names(available) <- subset(td_pcdt_abrev, co_pcdt == input$inPCDT)$no_procedimento
     
     selectInput(
       inputId = "inABREV",
       label = "",
       choices = sort(unique(as.character(sds$categoria)))
       # , selected = unique(available)[1]
     )
     
   })
   
   # ---------------------------------------------------------------------------
   reactive_sds_arima = reactive({
     
     sdsarima = reactive_sds()
     
     sdsarima = sdsarima[sdsarima$ano != format(Sys.Date(), "%Y"), ]
     
     sdsarima =  sdsarima %>%
        group_by(ano) %>%
        summarise(quantidade=sum(quantidade))
     
#     levels(sdsarima$ano)=levels(droplevels(sdsarima$ano))

     
     sdsarima$nu_ano = as.numeric(as.character(sdsarima$ano))
          
     anomin=min(sdsarima$nu_ano)
     anomax=max(sdsarima$nu_ano)
     
     rownames(sdsarima)=seq(anomin,anomax)
     
     sdsarima$ano=NULL
      
     sdsarima=ts(sdsarima$quantidade, start = anomin, end = anomax)
     
     return(sdsarima)
     
   })
   
   # ---------------------------------------------------------------------------
   reactive_sds_arima_abrev = reactive({
     
     sds_arima_abrev = reactive_sds()
     
     sds_arima_abrev$categoria=as.character(sds_arima_abrev$categoria)
     sds_arima_abrev = subset(sds_arima_abrev, categoria == input$inABREV)
     sds_arima_abrev$categoria=NULL
     
     sds_arima_abrev = sds_arima_abrev[sds_arima_abrev$ano != format(Sys.Date(), "%Y") & ! sds_arima_abrev$ano %in% c("2008","2009"), ]
     
     sds_arima_abrev =  sds_arima_abrev %>%
       group_by(ano) %>%
       summarise(quantidade=sum(quantidade))
     
     sds_arima_abrev$nu_ano = as.numeric(as.character(sds_arima_abrev$ano))
     
     anomin=min(sds_arima_abrev$nu_ano)
     anomax=max(sds_arima_abrev$nu_ano)
     
     rownames(sds_arima_abrev)=seq(anomin,anomax)
     
     sds_arima_abrev$ano=NULL
     
     sds_arima_abrev=ts(sds_arima_abrev$quantidade, start = anomin, end = anomax)
     
     
     
     return(sds_arima_abrev)
     
   })

   # ---------------------------------------------------------------------------
   
   output$barplot_market_share = renderPlot({
        
     sds = reactive_sds()
     
     ggplot(
       sds,
       aes(
         fill = categoria,
         y = porcento,
         x = ano,
         label = sds[,print(names(which(apresentacao == input$inApresentacao)))]
       )
     ) + geom_bar(stat = "identity") +
       xlab('ano') +
       ylab('') + # input$inCampos
       geom_text(size = 3, position = position_stack(vjust = 0.5)) +
       coord_flip() + 
       theme(
         plot.title = element_text(size = 12, face = "bold"),
         legend.text = element_text(size = rel(1.3)),
         axis.title.x = element_text(size = rel(1.3)),
         axis.title.y = element_text(size = rel(1.3)),
         axis.text.x = element_text(size = rel(1.3)),
         axis.text.y = element_text(size = rel(1.3))
       )

     
    })  
   
   # ---------------------------------------------------------------------------
   
   output$barplot_market_share_title = renderText({
     
     tudo=""
     
     if (input$inMedicamentos == "Sim (coletar para todas as coortes)"){
       tudo="e demais doenças."
     }
     
     paste(
       "<b>Difusão do percentual de uso no SUS (maket share) de medicamentos ",
       input$inTipo,
       "para",
       tolower(print(names(which(coorte == input$inCoorte)))),
       tudo,
       "</b><br>",
       print(names(which(metricas == input$inCampos))), 
       input$inApresentacao, 
       input$inAno[1], "-", input$inAno[2],"<br><br>"
     )
   })
   
   
   # ---------------------------------------------------------------------------
   
   output$pareto = renderPlot({
     
     sds_categoria = as.data.frame( reactive_sds() %>%
                                      group_by(categoria) %>%
                                      summarise(quantidade=sum(quantidade)))
     
     fator=10^(nchar(as.character(min(sds_categoria$quantidade)[1]))-1)
     
     x=sds_categoria$quantidade/fator
     names(x)=sds_categoria$categoria
     
     if ( fator == 1 ){
       fator = ""
     } else fator = paste("( X", format(fator, big.mark="."), ")")
     
     pareto.chart(
       x,
       ylab = paste("quantidade", fator),
       ylab2 = "",
       main = "",
       las=2,
       cumperc = seq(0, 100, by = 5)  # ranges of the percentages at the right
     )
     
   })

   # ---------------------------------------------------------------------------
   
   output$pareto_title = renderText({
     
     tudo=""
     
     if (input$inMedicamentos == "Sim (coletar para todas as coortes)"){
       tudo="e demais doenças."
     }
     
     paste(
       "<b>Quantitativo total e acumulado por medicamento (distribuição de pareto) para ",
       tolower(print(names(which(coorte == input$inCoorte)))),
       tudo,
       "</b><br>",
       print(names(which(metricas == input$inCampos))),
       input$inApresentacao,
       input$inAno[1], "-", input$inAno[2],
       "<br><br>"
     )
   })
   
   # ---------------------------------------------------------------------------
   
   output$arima = renderPlot({
     
     sds=reactive_sds_arima()
     
     
     max=round(max(c(
      max(sds),
      max(as.data.frame(forecast(reactive_sds_arima(), h=5)))
     )))
     
     fator=10^(nchar(as.character(max))-1)
     max=round(max/fator,1)*fator
     
     y=round(
       seq(0, max, length.out=6)
     )

     autoplot(sds, ylab="", xlab="ano") + 
       geom_forecast(h=5) +
       scale_x_continuous(breaks = seq(min(time(sds)), max(time(sds)+5), by = 2)) +
       scale_y_continuous(breaks = y)
     
     
   })

   # ---------------------------------------------------------------------------
   
   output$arima_abrev = renderPlot({
     
     sds=reactive_sds_arima_abrev()
     
     max=round(max(c(
       max(sds),
       max(as.data.frame(forecast(sds, h=5)))
     )))
     
     fator=10^(nchar(as.character(max))-1)
     max=round(max/fator,1)*fator
     
     y=round(
       seq(0, max, length.out=6)
     )
     
     autoplot(sds, ylab="", xlab="ano") + 
       geom_forecast(h=5) +
       scale_x_continuous(breaks = seq(min(time(sds)), max(time(sds)+5), by = 2)) +
       scale_y_continuous(breaks = y)
     
     
   })
   
      
   # ---------------------------------------------------------------------------
   
   output$arima_title = renderText({
     tudo=""
     
     if (input$inMedicamentos == "Sim (coletar para todas as coortes)"){
       tudo="e demais doenças."
     }
     
     paste(
       "<b>Quantitativo ao ano e projeção para 5 anos (método arima) para ",
       tolower(print(names(which(coorte == input$inCoorte)))),
       tudo, "</b><br>",
       print(names(which(metricas == input$inCampos))),
       input$inApresentacao,
       input$inAno[1], "-", input$inAno[2]
     )
   })   
   

   # ---------------------------------------------------------------------------
   
   output$sds_arima = DT::renderDataTable({

     data = round(as.data.frame(forecast(reactive_sds_arima(), h=5)))
     colnames(data)=c("quantidade",colnames(data)[2:5])

     anomin = min(time(data))
     anomax = max(time(data))
     
     x = as.data.frame(reactive_sds_arima())
     colnames(x)="quantidade"
     
      
     data = smartbind(x, data)
     rownames(data)=seq(input$inAno[1],input$inAno[2]+4)

     colnames(data)=c("quantidade","IC80\nmin","IC80\nmax","IC95\nmin","IC95\nmax")
     
     datatable(
       
       data= data,
       
       # colnames = c( ),
       rownames= TRUE,
       extensions = 'Buttons', options = list(
         dom = 'Bfrtip',
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
         paging =FALSE
       ),
       escape = FALSE
     )
   })
   
   # ---------------------------------------------------------------------------
   
   output$sds_arima_abrev = DT::renderDataTable({
     
     data = round(as.data.frame(forecast(reactive_sds_arima_abrev(), h=5)))
     colnames(data)=c("quantidade",colnames(data)[2:5])
     
     anomin = min(as.numeric(time(reactive_sds_arima_abrev())))
     
     x = as.data.frame(reactive_sds_arima_abrev())
     colnames(x)="quantidade"
     
     data = smartbind(x, data)
     rownames(data)=seq(anomin,input$inAno[2]+4)
     
     colnames(data)=c("quantidade","IC80\nmin","IC80\nmax","IC95\nmin","IC95\nmax")
     
     datatable(
       
       data= data,
       
       # colnames = c( ),
       rownames= TRUE,
       extensions = 'Buttons', options = list(
         dom = 'Bfrtip',
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
         paging =FALSE
       ),
       escape = FALSE
     )
   })
      
   # ---------------------------------------------------------------------------
   
   
   output$sds = DT::renderDataTable({
     
     data= reactive_sds()
     data$porcento=NULL
     data$soma_ano=NULL
     data$nu_ano=NULL
     data$quantitativo=NULL
     
     
     colnames(data)=c(
       "ano","SIGTAP","sigla","quantidade","(%)"
     )
     
     
     datatable(
       
       data = data,
       
       
       # colnames = c( ),
       rownames= FALSE,
       extensions = 'Buttons', options = list(
         dom = 'Bfrtip',
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
         paging =FALSE
       ),
       escape = FALSE
     )
     
   })
   
   # ---------------------------------------------------------------------------
   
   
   output$td_diretriz_202010011724 = DT::renderDataTable({
     datatable(
       
       data= sup,
       
       # colnames = c( ),
       rownames= FALSE,
       extensions = 'Buttons', options = list(
         dom = 'Bfrtip',
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
         paging =FALSE
       ),
       escape = FALSE
     )
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
