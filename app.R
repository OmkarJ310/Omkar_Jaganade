#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#----------------downloading ggradar--------------------
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

library(shiny)
library(shinydashboard)
require("openxlsx")
require(sqldf)
require("colorspace")
require("tibble")
require(dplyr)
require(ggplot2)
require(countrycode)
require(ggthemes)
require(ggradar)
library(ggforce)
library(ggiraphExtra)
require(plotly)
require(ggsoccer)
require(maps)
install.packages("rgdal")
install.packages("shapes")
require(rgdal)
require(shapes)
require(sp)
require(leaflet)
#----------------- Preparing the data ------------------#

fifa_analysis <- read.csv("data_output.csv")

## ------------------- Player Count Analysis --------------------##
player_country <-
    sqldf("select count(Name) as Count, Nationality from fifa_analysis group by Nationality")
player_country['ISOcode'] <- countryname(player_country$Nationality, destination = 'iso3c')
top_ten_country <- head(arrange(player_country, desc(Count)), 10)

##-------------- Age versus Potential ------------------------##
fifa_analysis$Age <- as.numeric(as.character(fifa_analysis$Age))

temp = aggregate(list(potential = fifa_analysis$Potential),
                 list(age = factor(fifa_analysis$Age)),
                 mean)
## ------------ MAP ------------##
# world <- map("world", fill=TRUE, plot=FALSE)
# world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
# world_map <- SpatialPolygonsDataFrame(world_map,
#                                       data.frame(country=names(world_map),
#                                                  stringsAsFactors=FALSE),
#                                       FALSE)
# 
# 
# 
# target <- subset(world_map, country %in% player_country$Nationality, count %in% player_country$Count)
# 
# 
# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# 
# 
# 
# pal <- colorBin("YlOrRd", domain = player_country$Count, bins = bins)
# m<- leaflet(target) %>%
#     addTiles() %>% addPolygons(weight=2, color = 'white', fillColor = pal(player_country$Count), popup = htmlEscape(player_country$Count))
# m

world_map <- map_data("world")
fifa_1_new <-
    world_map %>% mutate(region = as.character(region)) %>%
    left_join((
        fifa_analysis %>% mutate(
            Nationality = as.character(Nationality),
            Nationality = if_else(Nationality %in% "England",
                                  "UK", Nationality)
        ) %>%
            
            count(Nationality, name = "Number of Player") %>%
            rename(region = Nationality) %>%
            mutate(region = as.character(region))
    ), by = "region")
fifa_1_new<-subset(fifa_1_new, select = -c(3:6))
fifa_1_new<- na.omit(fifa_1_new)
xy <- fifa_1_new[1:2]
df<-as.data.frame(fifa_1_new$`Number of Player`)
df

SPDF<- SpatialPointsDataFrame(coords = xy, data = df)
map<- leaflet(SPDF)%>% addTiles()
map %>% addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                    color  = ~pal(Count),popup = ~htmlEscape(Count)
)

##-------------------- Best Squad Data ------------------- #
shots<-data.frame(x = c(50, 50, 50, 32, 66, 50, 32, 66, 50, 34, 68, 18, 82, 12, 90, 48, 50, 16, 84, 20, 82, 32, 68, 32, 66, 32, 68),
                  y = c(88, 68, 1, 68, 68, 54, 54, 54, 12, 14, 14, 14, 14, 30, 30, 30, 44, 44, 44, 60, 60, 44, 44, 86, 86, 30, 30),
                  Position=c('ST', 'CF', 'GK', 'LF', 'RF', 'CAM', 'LAM', 'RAM', 'CB','LCB', 'RCB', 'LB', 'RB', 'LWB', 'RWB', 'CDM', 'CM', 'LM','RM', 'LW', 'RW', 'LCM', 'RCM', 'LS', 'RS', 'LDM', 'RDM'))

squad <- sqldf(
    "select max(Overall) as  Overall, Position,Name, Crossing, Finishing, HeadingAccuracy, ShortPassing, Volleys, Dribbling, Curve, FKAccuracy, LongPassing from fifa_analysis group by Position order by Overall "
)
squad <-
    mutate(squad, Name_Position = paste(squad$Name, "-", squad$Position))

shots_squad <- full_join(squad, shots, all=TRUE)

new_squad <- squad[, c(-1)]
players_list <- unique(squad$Name_Position)

#---------------------UI---------------------------#
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "FIFA 2019 Analysis"),
    dashboardSidebar(sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Player Count Analysis", tabName = "player_count"),
        menuItem("Team of the Best", tabName = "best_team"),
        menuItem("About", tabName = "about")
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "home", mainPanel(
            img(
                src = "fifa_2019.png",
                height = 300,
                width = 350
            )
        )),
        tabItem(tabName = "player_count",
                mainPanel(
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Plot 1", plotOutput("plot1")),
                        tabPanel("Plot 2", plotOutput("plot3"))
                    )
                )),
        tabItem(
            tabName = "best_team",
            p(
                "FACT: The best team represents the top players played for each position of football field for the year 2019."
            ),
            p(
                "The dropdown below has player name - field position, on selection it displays the statistics of the player. "
            ),
            fluidRow(box(
                sidebarLayout(sidebarPanel (
                    selectInput(
                        inputId = 'pname',
                        label = 'Choose a Player:',
                        choices = players_list,
                        selected = 'T. Kroos - LCM'
                    ),
                    width = 6
                ), mainPanel()),
                plotOutput("plot2")
                
            ),
            box(
                plotlyOutput("plot4")
            ))
        ),
        tabItem(
            tabName = "about",
            p("The available player positions are:", style = "font-size:25px"),
            p("GK: Goalkeeper", style = "font-size:15px;color: blue"),
            p("CB: Center Back", style = "font-size:15px;color: blue"),
            p("RB: Right Back", style = "font-size:15px;color: blue"),
            p("LB: Left Back", style = "font-size:15px;color: blue"),
            p("DMF: Defense Midfield", style = "font-size:15px;color: blue"),
            p("CMF: Center Midfield", style = "font-size:15px;color: blue"),
            p("AMF: Attacking Midfield", style = "font-size:15px;color: blue"),
            p("RMF: Right Midfield", style = "font-size:15px;color: blue"),
            p("LMF: Left Midfield", style = "font-size:15px;color: blue"),
            p("RWF: Right Wing Forward", style = "font-size:15px;color: blue"),
            p("LWF: Left Wing Forward", style = "font-size:15px;color: blue"),
            p("SS: Second Striker", style = "font-size:15px;color: blue"),
            p("CF: Counter Forward", style = "font-size:15px;color: blue"),
            hr()
        )
    ))
)





# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot({
        ggplot(top_ten_country, aes(x = reorder(Nationality, -Count), y = Count)) +
            geom_col(fill = "red") +
            theme_few() +
            ggtitle(label = "Top 10 FIFA Playing Nations") + xlab("Country") + theme(plot.title = element_text(hjust = 0.5)) +
            geom_text(aes(label = top_ten_country$Count, vjust = -0.6))
    })
    
    output$plot2 <- renderPlot({
        new_squad %>% filter(Name_Position == input$pname) %>%
            mutate_each(funs(),-Name_Position) %>%
            ggRadar(rescale = FALSE)
    })
    
    output$plot3 <- renderPlot({
        ggplot(temp, aes(
            x = age,
            y = potential,
            group = 1
        )) + geom_line() + geom_point() + ggtitle(label = "Age versus Potential") +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    output$plot4 <- renderPlotly({
        football_field<- ggplot(shots_squad, aes(x=y, y=x, label=Name))+annotate_pitch(fill = "dark green" )+theme_pitch()+geom_point(color = "red")+geom_text(aes(label=Position), hjust=0, vjust=1.5)
        ggplotly(football_field, tooltip = "Name")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
