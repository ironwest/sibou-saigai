library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)

dd <- read_rds("death_by_accident_data.rds")
gyou <- readxl::read_excel("sibou_codes.xlsx",sheet="業種")
gyoudai <- gyou |> select(code = dai_code, name = dai_name) |> distinct()

kiin <- readxl::read_excel("sibou_codes.xlsx",sheet="起因物")
kiindai <- kiin |> select(code = dai_code, name = dai_name) |> distinct()

jiko <- readxl::read_excel("sibou_codes.xlsx", sheet="事故の型")

  
choices_gyoudai <- gyoudai$code |> setNames(gyoudai$name)
choices_kibo    <- levels(dd$kibo)
choices_kiin    <- kiindai$code |> setNames(kiindai$name)
choices_jiko    <- jiko$jiko_code |> setNames(jiko$jiko_bunrui)
choices_month <- c(4:12,1:3)
choices_fy <- unique(dd$fy)

pick_gyou <- pickerInput(
  inputId = "gyou",
  label = "業種の選択", 
  choices = choices_gyoudai,
  selected = choices_gyoudai,
  options = list(`actions-box` = TRUE), 
  multiple = TRUE
)

pick_kibo <- pickerInput(
  inputId = "kibo",
  label = "事業場規模の選択",
  choices = choices_kibo,
  selected = choices_kibo,
  options = list(`actions-box` = TRUE),
  multiple = TRUE
)

pick_kiin <- pickerInput(
  inputId = "kiin",
  label = "事故の起因物の選択",
  choices = choices_kiin,
  selected = choices_kiin,
  options = list(`actions-box` = TRUE),
  multiple = TRUE
)

pick_jiko <- pickerInput(
  inputId = "jiko",
  label = "事故の種類の選択",
  choices = choices_jiko,
  selected = choices_jiko,
  options = list(`actions-box` = TRUE),
  multiple = TRUE
)

pick_month <- pickerInput(
  inputId = "month",
  label = "月の選択",
  choices = choices_month,
  selected = choices_month,
  options = list(`actions-box` = TRUE),
  multiple = TRUE
)

slide_fy <- sliderTextInput(
  inputId = "fy",
  label = "年度を選択", 
  choices = choices_fy,
  selected = choices_fy[c(1,length(choices_fy))]
)


ui <- fluidPage(
  
  # Application title-------------------
  titlePanel("労働災害データベースアプリ"),
  
  # -------------------------
  sidebarLayout(
    sidebarPanel(width = 3,
      pick_gyou,
      pick_kibo,
      pick_kiin,
      pick_jiko,
      pick_month,
      slide_fy,
      hr(),
      textInput("kw", "災害状況をKWで絞り込む"),
      textOutput("hits")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
      checkboxInput("posgraph", label = "グラフ横並び/表をすべて表示"),
      tabsetPanel(
        tabPanel("表"           , br(), DT::dataTableOutput("table")),
        tabPanel("経年:業種"    , br(), plotOutput("plot_gyou")),
        tabPanel("経年:規模"    , br(), plotOutput("plot_kibo")),
        tabPanel("経年:起因物"  , br(), plotOutput("plot_kiin")),
        tabPanel("経年:事故要因", br(), plotOutput("plot_jiko")),
        tabPanel("経年:月別"　  , br(), plotOutput("plot_month"))
      )
    )
  )
)

server <- function(input, output) {
  #read data
  d <- read_rds("death_by_accident_data.rds")
  
  #filter data
  searched <- reactive({
    res <- d
    if(input$kw==""){
      #do nothing
    }else{
      kws <- str_split_1(input$kw,"\\s")
      
      for(i in 1:length(kws)){
        res <- res |> 
          filter(str_detect(text,kws[i]))  
      }
      
    }
    return(res)
  })
  
  dat <- reactive({
    searched() |> 
      filter(gyou_dai_code %in% input$gyou) |> 
      filter(kibo          %in% input$kibo) |> 
      filter(kiin_dai_code %in% input$kiin) |> 
      filter(jiko_code     %in% input$jiko) |> 
      filter(month %in% input$month) |> 
      filter(between(fy, input$fy[1], input$fy[2]))
  })
  
  #make plots
  make_plot <- function(gdat, grpby, lglposition, grplabel, titlelabel){
    if(lglposition){
      position <- "dodge"
    }else{
      position <- "stack"
    }
    
    graph <- gdat |> 
      count(fy, {{grpby}}) |> 
      ggplot() +
      geom_col(aes(x = fy, y = n, fill = {{grpby}}), position = position) +
      labs(title = titlelabel, x = "年度", y = "件数", fill=grplabel) +
      theme_bw()  
    
    return(graph)
  }
  
  output$plot_gyou  <- renderPlot({ make_plot(dat(),gyou_dai_name, input$posgraph, "業種"      , "年度別事故件数(業種)")})
  output$plot_kibo  <- renderPlot({ make_plot(dat(),kibo         , input$posgraph, "事業場規模", "年度別事故件数(事業場規模)")})
  output$plot_kiin  <- renderPlot({ make_plot(dat(),kiin_dai_name, input$posgraph, "起因物"    , "年度別事故件数(起因物)")})
  output$plot_jiko　<- renderPlot({ make_plot(dat(),jiko_name    , input$posgraph, "事故原因"  , "年度別事故件数(事故原因)")})
  output$plot_month <- renderPlot({ make_plot(dat(),month        , input$posgraph, "月"        , "年度別事故件数(月)")})
  
  output$hits <- renderText({
    hits <- nrow(dat())
    overall <- nrow(d)
    
    return(str_glue("{hits}件該当({overall}件中)"))
  })
  #search data
  output$table <- DT::renderDataTable({
    res <- dat() |> 
      select(`年度` = fy,
             `ID`   = ID,
             `月` = month, 
             `発生時間` = time, 
             `災害状況` = text, 
             `業種コード(大分類)` = gyou_dai_code, 
             `業種(大分類)`       = gyou_dai_name, 
             `業種コード(中分類)` = gyou_tyu_code, 
             `業種(中分類)`       = gyou_tyu_name, 
             `業種コード(小分類)` = gyou_syo_code, 
             `業種(小分類)`       = gyou_syo_name, 
             `事業場規模` = kibo, 
             `起因物コード(大分類)` = kiin_dai_code, 
             `起因物(大分類)`     　= kiin_dai_name, 
             `起因物コード(中分類)` = kiin_tyu_code, 
             `起因物(中分類)`     　= kiin_tyu_name, 
             `起因物コード(小分類)` = kiin_syo_code, 
             `起因物(小分類)`     　= kiin_syo_name, 
             `事故の型` = jiko_name)
    
    if(!input$posgraph){
      res <- res |> 
        select(`年度`,`月`,`発生時間`,
               `災害状況`,`業種(大分類)`,
               `事業場規模`,`起因物(大分類)`,`事故の型`)
    }
    
    return(res)
  },
  extensions = 'Buttons', 
  options = list(
    dom = 'Bfrtip',
    buttons = c('copy',  'excel')
  ))
}


shinyApp(ui = ui, server = server)

