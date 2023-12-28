#2023 IMLS Oak Analysis with No Map
#

library(shiny)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(ggsci)

oaks <- read.csv("data/oak-data-2023-complete.csv",header=TRUE)
oaks <- oaks %>%
  mutate(Status = case_when(X7.5.23.Status %in% c("unknown", "fungal discard",
                                                  "bacterial discard",
                                                  "bacterial/fungal discard",
                                                  "brown discard") ~"Discard",
                            X7.5.23.Status %in% c("alive", "green bud", 
                                                  "growing")~"Alive")) %>%
  mutate(X7.5.23.Status = fct_relevel(X7.5.23.Status, "unknown",
                                      "fungal discard", 
                                      "bacterial discard", 
                                      "bacterial/fungal discard", 
                                      "brown discard", "alive", 
                                      "green bud", "growing"))
oaks <- oaks %>% mutate(Status = fct_relevel(Status, "Discard", "Alive"))


group.colors <- c("unknown" = "grey52",
                  "fungal discard" = "darkgoldenrod2",
                  "bacterial discard" = "indianred2", 
                  "bacterial/fungal discard" = "palevioletred2",
                  "brown discard" = "salmon4",
                  "alive" = "darkseagreen2",
                  "green bud" = "olivedrab3",
                  "growing" = "springgreen4", 
                  "Discard" = "salmon4", 
                  "Alive" = "springgreen4")

# Define UI for application 
ui <- fluidPage(
  titlePanel("2023 Oak Data"), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput("oak_graph", "Select graph", 
                  choices = list("Number of tubes & genotypes per species" = "genotypes",
                                 "Full status by media - counts" = "media_count",
                                 "Full status by media - proportions" = "media_prop",
                                 "Dead/Alive status by media - counts" = "media_binary_count",
                                 "Dead/Alive status by media - proportions" = "media_binary_prop",
                                 "Full status by species - counts" = "species_counts", 
                                 "Full status by species - proportions" = "species_prop", 
                                 "Dead/Alive status by species - counts" = "species_binary_count", 
                                 "Dead/Alive status by species - proportions" = "species_binary_prop", 
                                 "Full status by rinsing" = "rinsing", 
                                 "Full status by antimicrobial" = "antimicrobial", 
                                 "Full status by STS" = "STS",
                                 "Full status by STS & species" = "STS_species", 
                                 "Full status by shoot stage" = "shoot_stage", 
                                 "Full status by sterilization method" = "sterilization", 
                                 "Full status by storage method" = "storage", 
                                 "Full status by number of days stored" = "storage_days"),
                  selected = "genotypes")
    ), 
    mainPanel(
      plotOutput("plot")
    )
  )
)



# Define server logic 
server <- function(input, output) {
  
  #create bar chart
  output$plot <- renderPlot({
    if (input$oak_graph == "media_count") {
      oaks %>% 
        filter(!is.na(X7.5.23.Status)) %>%
        filter(!is.na(Base.Medium)) %>%
        ggplot(.,aes(x=Base.Medium, fill=X7.5.23.Status)) + 
        scale_fill_manual(values = group.colors) +
        geom_bar() + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "genotypes") {
      ggplot(oaks,aes(x=Species, fill=Tree.ID)) + 
        geom_bar() + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        guides(fill="none") +
        labs(x="Species (color indicates genotype)") +
        theme_minimal()
    } else if (input$oak_graph == "species_counts") {
      ggplot(oaks,aes(x=Species, fill=X7.5.23.Status)) + 
        geom_bar() + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "species_prop") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        ggplot(., mapping = aes(x=Species)) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") +
        scale_fill_manual(values = group.colors) +
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes") + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "media_prop") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        filter(!is.na(Base.Medium)) %>%
        ggplot(., mapping = aes(x=Base.Medium)) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") +
        scale_fill_manual(values = group.colors) +
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes") + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "media_binary_prop") {
      oaks %>% filter(!is.na(Status)) %>%
        ggplot(., aes(x=Species)) + 
        geom_bar(aes(fill=Status), position="fill") + 
        scale_x_discrete(guide = guide_axis(angle=45)) + 
        labs(y="proportion of total tubes") + 
        theme_minimal()
    } else if (input$oak_graph == "species_binary_prop") {
      oaks %>% filter(!is.na(Status)) %>%
        ggplot(., aes(x=Species)) + 
        geom_bar(aes(fill=Status), position="fill") + 
        scale_x_discrete(guide = guide_axis(angle=45)) + 
        labs(y="proportion of total tubes") + 
        theme_minimal()
    } else if (input$oak_graph == "media_binary_count") {
      oaks %>% filter(!is.na(Status)) %>%
        ggplot(., aes(x=Species)) + 
        geom_bar(aes(fill=Status)) + 
        scale_x_discrete(guide = guide_axis(angle=45)) + 
        labs(y="proportion of total tubes") + 
        theme_minimal()
    } else if (input$oak_graph == "species_binary_count") {
      oaks %>% filter(!is.na(Status)) %>%
        ggplot(., aes(x=Species)) + 
        geom_bar(aes(fill=Status)) + 
        scale_x_discrete(guide = guide_axis(angle=45)) + 
        labs(y="proportion of total tubes") + 
        theme_minimal()
    } else if (input$oak_graph == "rinsing") {
      oaks %>% filter(!is.na(Rinsed.)) %>%
        ggplot(., aes(x=Rinsed.)) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) +
        scale_x_discrete(guide = guide_axis(angle=45)) + 
        labs(y="proportion of total tubes") +
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "antimicrobial") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        filter(!is.na(Antimicrobial)) %>%
        ggplot(., mapping = aes(x=Antimicrobial)) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes") + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "STS") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        ggplot(., mapping = aes(x=Other.addenda)) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="Addition of STS") + 
        guides(fill=guide_legend(title="Status")) +
        theme_minimal()
    } else if (input$oak_graph == "STS_species") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        ggplot(., mapping = aes(x=Other.addenda)) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        facet_grid(~ Species) +
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="Addition of STS") + 
        guides(fill=guide_legend(title="Status")) +
        theme_minimal()
    } else if (input$oak_graph == "shoot_stage") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        ggplot(., mapping = aes(x=as.factor(Shoot.Stage))) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="Shoot Stage") + 
        guides(fill=guide_legend(title="Status")) +
        theme_minimal()
    } else if (input$oak_graph == "sterilization") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        mutate(sterile = case_when(Sterilization == "1" ~ "standard",
                                   Sterilization == "2" ~ "standard + 1sec Ben",
                                   Sterilization == "3" ~ "standard + 2min Ben", 
                                   Sterilization == "4" ~ "standard + 2min Ben + rinse",
                                   Sterilization == "5" ~ "standard + Ben soak")) %>%
        filter(!is.na(Sterilization)) %>%
        ggplot(., mapping = aes(x=as.factor(sterile))) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="Sterilization Method") + 
        guides(fill=guide_legend(title="Status")) +
        theme_minimal()
    } else if (input$oak_graph == "sterilization_binary") {
      oaks %>% mutate(Status = fct_relevel(Status, "Discard", "Alive")) %>%
        mutate(sterile = case_when(Sterilization == "1" ~ "standard",
                                   Sterilization == "2" ~ "standard + 1sec Ben",
                                   Sterilization == "3" ~ "standard + 2min Ben", 
                                   Sterilization == "4" ~ "standard + 2min Ben + rinse",
                                   Sterilization == "5" ~ "standard + Ben soak")) %>%
        filter(!is.na(Sterilization)) %>%
        ggplot(., mapping = aes(x=as.factor(Sterilization))) + 
        geom_bar(aes(fill=Status), position="fill") + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="Sterilization Method") + 
        guides(fill=guide_legend(title="Status")) +
        theme_minimal()
    } else if (input$oak_graph == "storage") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        filter(!is.na(Storage)) %>%
        ggplot(., mapping = aes(x=as.factor(Storage))) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="Storage Method") + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } else if (input$oak_graph == "storage_days") {
      oaks %>% filter(!is.na(X7.5.23.Status)) %>%
        ggplot(., mapping = aes(x=as.factor(Days.stored))) + 
        geom_bar(aes(fill=X7.5.23.Status), position="fill") + 
        scale_fill_manual(values = group.colors) + 
        scale_x_discrete(guide = guide_axis(angle = 45)) + 
        labs(y="proportion of total tubes", x="# Days Stored") + 
        guides(fill=guide_legend(title="Status")) + 
        theme_minimal()
    } 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

