library(shiny)
library(DT)
library(recharts)
library(metricsgraphics)
require(rCharts)


packageCheck = unlist(packageVersion("shiny"))

if(packageCheck[1] == 0 & packageCheck[2] < 9){
  
  shinyOld = TRUE
} else {
  shinyOld = FALSE
}
shinyUI(pageWithSidebar(
  
  headerPanel('数据总览'),
  
  sidebarPanel(
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Data_Display'",
      
      h3('宝贝浏览')
      
      
    ),
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Data_statistical'",
      
      h3('宝贝数据总览表格')
      
      
    ),
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Charts'",
      
      h3('数据图表')
      
      
    ),
    
    selectInput('Mode',"请选择搜索浏览模式",list('按类目展现','按关键词搜索')),
    
    
    conditionalPanel(
      
      
      
      condition = "input.Mode == '按关键词搜索'",
      
      textInput(inputId = 'key',label = '请输入关键词来浏览宝贝',value='GT')
      
      
    ),
    
    conditionalPanel(
      
      
      
      condition = "input.Mode == '按类目展现'",
      
      selectInput("Product_Categorys", "请选择类目",list("面膜","眼部护理", "胸部护理"))
      
      
    ),
    
    
    
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Charts'",
      
      
      selectInput("Statistics_Model", "请选择内置图表数据模型",list("商家宝贝计数","商家宝贝平均价格", "价格区间的销量"))
      
      
      
    ),
    
    
    
    
    radioButtons(inputId = "sortby",
                 label = "排序选择",
                 choices = list("按销量从高到低" = "sale-desc",
                                "按价格从低到高" = "price-asc",
                                "按人气从高到低" = "renqi-desc",
                                "综合排序"   = "default")),
    
    
    
    
    h4("附加选项"),
    
    checkboxInput(inputId = "Tmall",
                  label = "天猫店铺",
                  value = FALSE),
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Data_Display'",
      
      numericInput(inputId = 'displaydatas',label = '宝贝页数',
                   min = 1,value =1 ,max = 100,step =1 )
      
      
    ),
    
    
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs != 'Data_Display'",
      
      
      sliderInput(inputId = 'displaydatasC',label='前几页的数据统计',min = 1,value=1,max=100,step = 1)
      
      
      
    ),
    
    
    
    helpText('右侧面板如果出现错误,请将页数重置为1,更新数据需要时间，请耐心等待 '),
    
    
    
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Data_Display'",
      
      actionButton("go", "更新")
      
      
    ),
    ###submitButton(text = 'Update Now')
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Data_statistical'",
      
      actionButton("go2", "更新数据表")
      
      
    ),
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Charts'",
      
      actionButton("go3", "更新图表")
      
      
    )
    
  ),
  
  
  
  
  
  
  mainPanel(
    tabsetPanel(id="theTabs",
                
                
                tabPanel('宝贝展示' , htmlOutput('mianmohtmloutput'),value="Data_Display"),
                tabPanel('数据表',DT::dataTableOutput('DT'),value="Data_statistical"),
                ### tabPanel('Statistical_charts',metricsgraphicsOutput('mjs1',width = '1040px',height = '620px'),value='Charts')
                tabPanel('图表',showOutput("myChart", "polycharts"),value='Charts')
                
    )
    
    
    
  )
  
  
  
)
)











