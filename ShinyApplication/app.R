library(shiny)
library(magrittr)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(viridis)

NineNonfarmIndTwoCrisis<-read.csv('NineNonfarmIndTwoCrisis.csv')
RaceURTwoCrisis<-read.csv('RaceURTwoCrisis.csv')
GenderURTwoCrisis<-read.csv('GenderURTwoCrisis.csv')
StatesUnemploymentTwoCrisis<-read.csv('StatesUnemploymentTwoCrisis.csv')[,c(2:7)]
StateAbbr<-read.csv('StateAbbr.csv')


ui<-fluidPage(
    fluidRow(column(12,titlePanel(h3(strong('How Covid-19 recession Impact The Job Market Differently than the Financial Crisis in 2008')))
    )
    ),
    tabsetPanel(type = 'tabs',
                tabPanel("Unemployment Rates for States",
                         fluidRow(
                             column(2,selectInput("State",'Select a State',choices = c(StateAbbr$State,'U.S.'),
                                                  multiple = F,selected = 'U.S.'))
                         ),
                         fluidRow(
                             column(12,plotOutput('StateUR08'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2008-01-01 for Financial Crisis and 2020-01-01 for Covid-19 recession')) ))
                         )),
                tabPanel("Unemployment Rates by Industry",
                         fluidRow(
                             column(8,plotlyOutput('IndustryUR08'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2008-01-01')))),
                             column(1,selectInput('Months1','Months into recession',choices = c(1:30),width = '100%')),
                             column(3,h5(strong("Top Industries Impacted by Recession")),
                                    DT::dataTableOutput('Table1',width = '100%'))
                         ),
                         fluidRow(
                             column(8,plotlyOutput('IndustryUR20'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2020-01-01')))),
                             column(1,selectInput('Months2','Months into recession',choices = c(1:19),width = '100%')),
                             column(3,h5(strong("Top Industries Impacted by Recession")),
                                    DT::dataTableOutput('Table2',width = '100%'))
                         )),
                tabPanel("Unemployment Rates by Racial Group",
                         fluidRow(
                             column(8,plotlyOutput('RaceUR08'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2008-01-01')))),
                             column(1,selectInput('Months3','Months into recession',choices = c(1:30),width = '100%')),
                             column(3,h5(strong("Racial Group Ranked by UR")),
                                    DT::dataTableOutput('Table3',width = '100%'))
                         ),
                         fluidRow(
                             column(8,plotlyOutput('RaceUR20'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2020-01-01')))),
                             column(1,selectInput('Months4','Months into recession',choices = c(1:19),width = '100%')),
                             column(3,h5(strong("Racial Group Ranked by UR")),
                                    DT::dataTableOutput('Table4',width = '100%'))
                         )),
                tabPanel("Unemployment Rates by Gender Group",
                         fluidRow(
                             column(8,plotlyOutput('GenderUR08'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2008-01-01')))),
                             column(1,selectInput('Months5','Months into recession',choices = c(1:30),width = '100%')),
                             column(3,h5(strong('Gender Group Ranked by UR')),
                                    DT::dataTableOutput('Table5',width = '100%'))
                         ),
                         fluidRow(
                             column(8,plotlyOutput('GenderUR20'),
                                    fluidRow(column(8,h6('Note: Months into Recession measured by months from 2020-01-01')))),
                             column(1,selectInput('Months6','Months into recession',choices = c(1:19),width = '100%')),
                             column(3,h5(strong('Gender Group Ranked by UR')),
                                    DT::dataTableOutput('Table6',width = '100%'))
                         ))
    ),
)


server <- function(input,output){
    
    NineNonFarm1<-reactive(
        merge(NineNonfarmIndTwoCrisis[which(NineNonfarmIndTwoCrisis$Months<=as.numeric(input$Months1) &
                                                NineNonfarmIndTwoCrisis$Crisis=='Subprime mortgage'),][,c(2,3,5)],
              NineNonfarmIndTwoCrisis[which(NineNonfarmIndTwoCrisis$Crisis=='Subprime mortgage' &
                                                NineNonfarmIndTwoCrisis$Months<=as.numeric(input$Months1)),] %>%
                  dplyr::arrange(Months,desc(UR)) %>% dplyr::slice(seq(1,(as.numeric(input$Months1)*9-8),by=9)) %>% dplyr::count(Industry),
              by='Industry',all.x=TRUE) %>% dplyr::rename(`Months at #1` = n) %>% tidyr::replace_na(list(`Months at #1` = 0))
    )
    
    NineNonFarm2<-reactive(
        merge(NineNonfarmIndTwoCrisis[which(NineNonfarmIndTwoCrisis$Months<=as.numeric(input$Months2) &
                                                NineNonfarmIndTwoCrisis$Crisis=='Covid-19'),][,c(2,3,5)],
              NineNonfarmIndTwoCrisis[which(NineNonfarmIndTwoCrisis$Crisis=='Covid-19' &
                                                NineNonfarmIndTwoCrisis$Months<=as.numeric(input$Months2)),] %>%
                  dplyr::arrange(Months,desc(UR)) %>% dplyr::slice(seq(1,(as.numeric(input$Months2)*9-8),by=9)) %>% dplyr::count(Industry),
              by='Industry',all.x=TRUE) %>% dplyr::rename(`Months at #1` = n) %>% tidyr::replace_na(list(`Months at #1` = 0))
    )
    
    Race1<-reactive(
        merge(RaceURTwoCrisis[which(RaceURTwoCrisis$Months<=as.numeric(input$Months3) &
                                        RaceURTwoCrisis$Crisis=='Subprime mortgage'),][,c(4,5,3)],
              RaceURTwoCrisis[which(RaceURTwoCrisis$Crisis=='Subprime mortgage' &
                                        RaceURTwoCrisis$Months<=as.numeric(input$Months3)),] %>%
                  dplyr::arrange(Months,desc(UR)) %>% dplyr::slice(seq(1,(as.numeric(input$Months3)*4-3),by=4))%>%dplyr::count(Race),
              by='Race',all.x=TRUE) %>% dplyr::rename(`Months at #1` = n) %>% tidyr::replace_na(list(`Months at #1` = 0))
    )
    
    Race2<-reactive(
        merge(RaceURTwoCrisis[which(RaceURTwoCrisis$Months<=as.numeric(input$Months4) &
                                        RaceURTwoCrisis$Crisis=='Covid-19'),][,c(4,5,3)],
              RaceURTwoCrisis[which(RaceURTwoCrisis$Crisis=='Covid-19' &
                                        RaceURTwoCrisis$Months<=as.numeric(input$Months4)),] %>%
                  dplyr::arrange(Months,desc(UR)) %>% dplyr::slice(seq(1,(as.numeric(input$Months4)*4-3),by=4))%>%dplyr::count(Race),
              by='Race',all.x=TRUE) %>% dplyr::rename(`Months at #1` = n) %>% tidyr::replace_na(list(`Months at #1` = 0))
    )
    
    
    Gender1<-reactive(
        merge(GenderURTwoCrisis[which(GenderURTwoCrisis$Months<=input$Months5 &
                                          GenderURTwoCrisis$Crisis=='Subprime mortgage'),][,c(4,5,3)],
              GenderURTwoCrisis[which(GenderURTwoCrisis$Crisis=='Subprime mortgage' &
                                          GenderURTwoCrisis$Months<=as.numeric(input$Months5)),] %>%
                  dplyr::arrange(Months,desc(UR)) %>% dplyr::slice(seq(1,(as.numeric(input$Months5)*2-1),by=2)) %>% dplyr::count(Gender),
              by='Gender',all.x=TRUE) %>% dplyr::rename(`Months at #1` = n) %>% tidyr::replace_na(list(`Months at #1` = 0))
    )
    
    
    Gender2<-reactive(
        merge(GenderURTwoCrisis[which(GenderURTwoCrisis$Months<=input$Months6 &
                                          GenderURTwoCrisis$Crisis=='Covid-19'),][,c(4,5,3)],
              GenderURTwoCrisis[which(GenderURTwoCrisis$Crisis=='Covid-19' &
                                          GenderURTwoCrisis$Months<=as.numeric(input$Months6)),] %>%
                  dplyr::arrange(Months,desc(UR)) %>% dplyr::slice(seq(1,(as.numeric(input$Months6)*2-1),by=2)) %>% dplyr::count(Gender),
              by='Gender',all.x=TRUE) %>% dplyr::rename(`Months at #1` = n) %>% tidyr::replace_na(list(`Months at #1` = 0))
    )
    
    
    output$StateUR08<-renderPlot(
        ggplot(StatesUnemploymentTwoCrisis[which(StatesUnemploymentTwoCrisis$State==input$State),],
               aes(x=Months, y=UR,color=Crisis,group = Crisis))+
            geom_line(size=1)+ylab('Unemployment Rate (UR)')+xlab('Months into Recession')+ggtitle(paste(input$State,"Unemployment Rate"))+
            theme(plot.title = element_text(size = 20,face = 'bold',margin = margin(10,0,10,0),
                                            hjust = 0.5))
    )
    
    output$IndustryUR08<-renderPlotly({
        ggplotly(ggplot(NineNonfarmIndTwoCrisis[which(NineNonfarmIndTwoCrisis$Crisis=='Subprime mortgage'),],
                        aes(x=Months,y=UR,fill=Industry))+geom_area(alpha=0.6,size=0.5,color='white')+xlab('Months into Recession')+ylab('Unemployment Rate (UR)')+
                     ggtitle("Unemployment Rate By Industry During Subprime Mortgage Crisis")+
                     scale_fill_viridis(discrete=T)+theme_ipsum())
    })
    
    output$Table1<-DT::renderDataTable(
        DT::datatable(NineNonFarm1()[which(NineNonFarm1()$Months==input$Months1),][,c(1,2,4)] %>% dplyr::arrange(desc(UR)),
                      options = list(dom='tr',scrollY = '250px'),class = 'cell-border stripe',
                      rownames = F)
    )
    
    output$IndustryUR20<-renderPlotly(
        ggplotly(ggplot(NineNonfarmIndTwoCrisis[which(NineNonfarmIndTwoCrisis$Crisis=='Covid-19'),],
                        aes(x=Months,y=UR,fill=Industry))+geom_area(alpha=0.6,size=0.5,color='white')+xlab('Months into Recession')+ylab('Unemployment Rate (UR)')+
                     ggtitle("Unemployment Rate By Industry During Covid-19 Crisis")+
                     xlim(0,35)+scale_fill_viridis(discrete=T)+theme_ipsum())
    )
    
    output$Table2<-DT::renderDataTable(
        DT::datatable(NineNonFarm2()[which(NineNonFarm2()$Months==input$Months2),][,c(1,2,4)] %>% dplyr::arrange(desc(UR)),
                      options = list(dom='tr',scrollY = '250px'),class = 'cell-border stripe',
                      rownames = F)
    )
    
    
    output$RaceUR08<-renderPlotly(
        ggplotly(ggplot(RaceURTwoCrisis[which(RaceURTwoCrisis$Crisis=='Subprime mortgage'),],
                        aes(x=Months,y=UR,fill=Race))+geom_area(alpha=0.6,size=0.5,color='White')+xlab('Months into Recession')+ylab('Unemployment Rate (UR)')+
                     ggtitle('Unemployment Rate By Race During Subprime Mortgage Crisis')+
                     scale_fill_viridis(discrete = T)+theme_ipsum())
    )
    
    output$Table3<-DT::renderDataTable(
        DT::datatable(Race1()[which(Race1()$Months==input$Months3),][,c(1,2,4)] %>%
                          dplyr::arrange(desc(UR)),
                      options = list(dom='tr'),class = 'cell-border stripe',rownames = F)
    )
    
    
    output$RaceUR20<-renderPlotly(
        ggplotly(ggplot(RaceURTwoCrisis[which(RaceURTwoCrisis$Crisis=='Covid-19'),],
                        aes(x=Months,y=UR,fill=Race))+geom_area(alpha=0.6,size=0.5,color='White')+xlab('Months into Recession')+ylab('Unemployment Rate (UR)')+
                     ggtitle('Unemployment Rate By Race During Covid-19 Crisis')+
                     xlim(0,35)+scale_fill_viridis(discrete = T)+theme_ipsum())
    )
    
    output$Table4<-DT::renderDataTable(
        DT::datatable(Race2()[which(Race2()$Months==input$Months4),][,c(1,2,4)] %>%
                          dplyr::arrange(desc(UR)),
                      options = list(dom='tr'),class = 'cell-border stripe',rownames = F)
    )
    
    
    output$GenderUR08<-renderPlotly(
        ggplotly(ggplot(GenderURTwoCrisis[which(GenderURTwoCrisis$Crisis=='Subprime mortgage'),],
                        aes(x=Months,y=UR,color=Gender))+geom_line(size=1)+xlab('Months into Recession')+ylab('Unemployment Rate (UR)')+
                     ggtitle('Unemployment Rate by Gender During Subprime Mortgage Crisis')+theme_ipsum())
    )
    
    output$Table5<-DT::renderDataTable(
        DT::datatable(Gender1()[which(Gender1()$Months==input$Months5),][,c(1,2,4)] %>%
                          dplyr::arrange(desc(UR)),options = list(dom='tr'),
                      class = 'cell-border stripe',rownames = F)
    )
    
    
    output$GenderUR20<-renderPlotly(
        ggplotly(ggplot(GenderURTwoCrisis[which(GenderURTwoCrisis$Crisis=='Covid-19'),],
                        aes(x=Months,y=UR,color=Gender))+xlim(0,35)+geom_line(size=1)+xlab('Months into Recession')+ylab('Unemployment Rate (UR)')+
                     ggtitle('Unemployment Rate By Gender During Covid-19 Crisis')+
                     theme_ipsum())
    )
    
    output$Table6<-DT::renderDataTable(
        DT::datatable(Gender2()[which(Gender2()$Months==input$Months6),][,c(1,2,4)] %>%
                          dplyr::arrange(desc(UR)),options = list(dom='tr'),
                      class = 'cell-border stripe',rownames = F)
    )
    
    
}

shinyApp(ui = ui, server = server)
