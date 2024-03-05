library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("UCLA Tennis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

shotdata <- read.csv("GiacomoRevelli_HenryVds_Shot.csv")
pointdata <- read.csv("GiacomoRevelli_HenryVds_Point_ForVisuals.csv")
player1 <- "Giacomo Revelli"
player2 <- "Henry Vds"
grid1 <- readPNG("serve_placement_court.png")

serve_placement <- function(shotdata, pointdata, player){
  shotdata$courtSide <- ifelse(shotdata$pointScore %in% c("15-0", "15-30", "30-40", "40-0", "40-40 (Ad)",
                                                          "0-15", "30-15", "40-30", "0-40"), "Ad", 
                               ifelse(shotdata$pointScore %in% c("15-40", "30-0", "30-30", "15-15", "40-40 (Deuce)",
                                                                 "40-15", "0-30", "0-0"), "Deuce", NA))
  
  pointdata$courtSide <- ifelse(pointdata$pointScore %in% c("15-0", "15-30", "30-40", "40-0", "40-40 (Ad)",
                                                            "0-15", "30-15", "40-30", "0-40"), "Ad", 
                                ifelse(pointdata$pointScore %in% c("15-40", "30-0", "30-30", "15-15", "40-40 (Deuce)",
                                                                   "40-15", "0-30", "0-0"), "Deuce", NA))
  
  ad_serve_win <- pointdata %>% 
    filter(serverName == player & courtSide == "Ad") %>% 
    summarise("Type" = "Percent",
              "adServeWide" = round(((length(which((firstServeZone == "Wide" | secondServeZone == "Wide") & pointWonBy == player))) /
                                       (length(which(firstServeZone == "Wide" | secondServeZone == "Wide"))))*100),
              "adServeBody" = round(((length(which((firstServeZone == "Body" | secondServeZone == "Body") & pointWonBy == player))) /
                                       (length(which(firstServeZone == "Body" | secondServeZone == "Body"))))*100),
              "adServeT" = round(((length(which((firstServeZone == "T" | secondServeZone == "T") & pointWonBy == player))) /
                                    (length(which(firstServeZone == "T" | secondServeZone == "T"))))*100))
  
  deuce_serve_win <- pointdata %>% 
    filter(serverName == player & courtSide == "Deuce") %>% 
    summarise("deuceServeWide" = round(((length(which((firstServeZone == "Wide" | secondServeZone == "Wide") & pointWonBy == player))) /
                                          (length(which(firstServeZone == "Wide" | secondServeZone == "Wide"))))*100),
              "deuceServeBody" = round(((length(which((firstServeZone == "Body" | secondServeZone == "Body") & pointWonBy == player))) /
                                          (length(which(firstServeZone == "Body" | secondServeZone == "Body"))))*100),
              "deuceServeT" = round(((length(which((firstServeZone == "T" | secondServeZone == "T") & pointWonBy == player))) /
                                       (length(which(firstServeZone == "T" | secondServeZone == "T"))))*100))
  
  ad_serve_count <- shotdata %>% 
    filter(serverName == player & courtSide == "Ad") %>% 
    summarise("Type" = "Count",
              "adServeWide" = length(which(firstServeZone == "Wide")) + length(which(secondServeZone == "Wide")),
              "adServeBody" = length(which(firstServeZone == "Body")) + length(which(secondServeZone == "Body")),
              "adServeT" = length(which(firstServeZone == "T")) + length(which(secondServeZone == "T")))
  
  deuce_serve_count <- shotdata %>% 
    filter(serverName == player & courtSide == "Deuce") %>% 
    summarise("deuceServeWide" = length(which(firstServeZone == "Wide")) + length(which(secondServeZone == "Wide")),
              "deuceServeBody" = length(which(firstServeZone == "Body")) + length(which(secondServeZone == "Body")),
              "deuceServeT" = length(which(firstServeZone == "T")) + length(which(secondServeZone == "T")))
  
  df <- rbind(cbind(ad_serve_win, deuce_serve_win), cbind(ad_serve_count, deuce_serve_count))
  
  g <- rasterGrob(grid1, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)
  
  ggplot(df) +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    annotate("rect", 
             xmin=c(0,1,2,3.05,4,5), 
             xmax=c(1,2,2.95,4,5,6), 
             ymin=c(0.1,0.1,0.1,0.1,0.1,0.1) ,
             ymax=c(10,10,10,10,10,10), 
             alpha=1, color="black", 
             fill=c(ifelse(df$adServeWide[2] >= 15, "goldenrod", ifelse(df$adServeWide[2] < 10, "lemonchiffon2", "lightgoldenrod2")),
                    ifelse(df$adServeBody[2] >= 15, "goldenrod", ifelse(df$adServeBody[2] < 10, "lemonchiffon2", "lightgoldenrod2")),
                    ifelse(df$adServeT[2] >= 15, "goldenrod", ifelse(df$adServeT[2] < 10, "lemonchiffon2", "lightgoldenrod2")),
                    ifelse(df$deuceServeT[2] >= 15, "goldenrod", ifelse(df$deuceServeT[2] < 10, "lemonchiffon2", "lightgoldenrod2")),
                    ifelse(df$deuceServeBody[2] >= 15, "goldenrod", ifelse(df$deuceServeBody[2] < 10, "lemonchiffon2", "lightgoldenrod2")),
                    ifelse(df$deuceServeWide[2] >= 15, "goldenrod", ifelse(df$deuceServeWide[2] < 10, "lemonchiffon2", "lightgoldenrod2")))) +
    
    annotate("text", x=0.5, y=0.5, label= "Wide", cex = 4) +
    annotate("text", x=1.5, y=0.5, label= "Body", cex = 4) +
    annotate("text", x=2.5, y=0.5, label= "T", cex = 4) +
    annotate("text", x=3.5, y=0.5, label= "T", cex = 4) +
    annotate("text", x=4.5, y=0.5, label= "Body", cex = 4) +
    annotate("text", x=5.5, y=0.5, label= "Wide", cex = 4) +
    
    annotate("text", x=0.5, y=6, label= paste0(df$adServeWide[1], "%"), cex = 6, fontface = 2, 
             col = ifelse(df$adServeWide[1] >= 70, "darkgreen", ifelse(df$adServeWide[1] < 50, "red", "black"))) +
    annotate("text", x=1.5, y=6, label= paste0(df$adServeBody[1], "%"), cex = 6, fontface = 2, 
             col = ifelse(df$adServeBody[1] >= 70, "darkgreen", ifelse(df$adServeBody[1] < 50, "red", "black"))) +
    annotate("text", x=2.5, y=6, label= paste0(df$adServeT[1], "%"), cex = 6, fontface = 2, 
             col = ifelse(df$adServeT[1] >= 70, "darkgreen", ifelse(df$adServeT[1] < 50, "red", "black"))) +
    annotate("text", x=3.5, y=6, label= paste0(df$deuceServeT[1], "%"), cex = 6, fontface = 2, 
             col = ifelse(df$deuceServeT[1] >= 70, "darkgreen", ifelse(df$deuceServeT[1] < 50, "red", "black"))) +
    annotate("text", x=4.5, y=6, label= paste0(df$deuceServeBody[1], "%"), cex = 6, fontface = 2, 
             col = ifelse(df$deuceServeBody[1] >= 70, "darkgreen", ifelse(df$deuceServeBody[1] < 50, "red", "black"))) +
    annotate("text", x=5.5, y=6, label= paste0(df$deuceServeWide[1], "%"), cex = 6, fontface = 2, 
             col = ifelse(df$deuceServeWide[1] >= 70, "darkgreen", ifelse(df$deuceServeWide[1] < 50, "red", "black"))) +
    
    annotate("text", x=0.5, y=5.5, label= paste0("won"), cex = 4,
             col = ifelse(df$adServeWide[1] >= 70, "darkgreen", ifelse(df$adServeWide[1] < 50, "red", "black"))) +
    annotate("text", x=1.5, y=5.5, label= paste0("won"), cex = 4,
             col = ifelse(df$adServeBody[1] >= 70, "darkgreen", ifelse(df$adServeBody[1] < 50, "red", "black"))) +
    annotate("text", x=2.5, y=5.5, label= paste0("won"), cex = 4,
             col = ifelse(df$adServeT[1] >= 70, "darkgreen", ifelse(df$adServeT[1] < 50, "red", "black"))) +
    annotate("text", x=3.5, y=5.5, label= paste0("won"), cex = 4,
             col = ifelse(df$deuceServeT[1] >= 70, "darkgreen", ifelse(df$deuceServeT[1] < 50, "red", "black"))) +
    annotate("text", x=4.5, y=5.5, label= paste0("won"), cex = 4,
             col = ifelse(df$deuceServeBody[1] >= 70, "darkgreen", ifelse(df$deuceServeBody[1] < 50, "red", "black"))) +
    annotate("text", x=5.5, y=5.5, label= paste0("won"), cex = 4,
             col = ifelse(df$deuceServeWide[1] >= 70, "darkgreen", ifelse(df$deuceServeWide[1] < 50, "red", "black"))) +
    
    annotate("text", x=0.5, y=4.75, label= paste0("(", df$adServeWide[2], " shots)"), cex = 4, 
             fontface = ifelse(df$adServeWide[2] > 15, 2, 1)) +
    annotate("text", x=1.5, y=4.75, label= paste0("(", df$adServeBody[2], " shots)"), cex = 4, 
             fontface = ifelse(df$adServeBody[2] > 15, 2, 1)) +
    annotate("text", x=2.5, y=4.75, label= paste0("(", df$adServeT[2], " shots)"), cex = 4, 
             fontface = ifelse(df$adServeT[2] > 15, 2, 1)) +
    annotate("text", x=3.5, y=4.75, label= paste0("(", df$deuceServeT[2], " shots)"), cex = 4, 
             fontface = ifelse(df$deuceServeT[2] > 15, 2, 1)) +
    annotate("text", x=4.5, y=4.75, label= paste0("(", df$deuceServeBody[2], " shots)"), cex = 4, 
             fontface = ifelse(df$deuceServeBody[2] > 15, 2, 1)) +
    annotate("text", x=5.5, y=4.75, label= paste0("(", df$deuceServeWide[2], " shots)"), cex = 4, 
             fontface = ifelse(df$deuceServeWide[2] > 15, 2, 1)) +
    
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(title = paste("Serve Placement -", player), x = "", y = "") +
    xlim(-0.73, 6.73) +
    ylim(0, 10)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$servePlacement <- reactive({
    serve_placement(shotdata, pointdata, player1)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
