library(shinyjs)
library(ggplot2)
library(ggiraph)

##################
# USER INTERFACE #
##################
attractions_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML("
      @media (max-width: 1200px) {
        #tableauViz {
          height: 800px;
        }
      }
      @media (max-width: 768px) {
        #tableauViz {
          height: 600px;
        }
      }
      /* 添加以下样式以确保 Tableau 可视化居中 */
      .tableau-container {
        display: flex;
        justify-content: center;
        align-items: center;
        width: 100%;
        overflow-x: auto;
        padding: 20px;
      }
    ")),
    setUpTableauInShiny(),
    h2("Pedestrian Heatmap Around LandMarks In City Of Melbourne Over Time"),
    fluidRow(
      column(
        12,
        div(
          class = "tableau-container",
          tableauPublicViz(
            id = ns("tableauViz"),
            url = "https://public.tableau.com/views/Book2_17298960274770/Dashboard1?:language=zh-CN&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link",
            width = "80%",
            height = "1100px",
            style = "border:none;"
          )
        )
      )
    ),
    uiOutput("image_output")
  )
}

################
# SHINY SERVER #
################
attractions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 监听 URL 中的 feature_name 参数变化
    observe({
      query <- parseQueryString(session$clientData$url_search)
      feature_name <- query$feature_name

      # 打印 feature_name 参数到控制台，方便调试
      print(paste("Received Feature Name:", feature_name))

      if (!is.null(feature_name)) {
        # 设置图片路径 (假设图片存放在 www/images 文件夹下，并与 feature_name 匹配)
        image_path <- paste0("www/image_tab1_yanyang/", feature_name, ".jpg")

        # 渲染图片输出
        output$image_output <- renderUI({
          tags$img(src = image_path, width = "100%", height = "auto")
        })
      } else {
        # 如果没有接收到 feature_name 参数，显示提示文字
        output$image_output <- renderUI({
          h4("No Feature Name received or invalid Feature Name.")
        })
      }
    })
  })
}
