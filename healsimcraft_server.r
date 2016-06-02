library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested mastery coefficient
  datasetInput <- reactive({
    switch(input$spec,
           "Paladin-Holy" = 1.5,
           "Paladin-Protection" = 1,
           "Paladin-Retribution" = 2.1,
           "Priest-Discipline"= 2.5, 
           "Priest-Holy" = 1.25, 
           "Priest-Shadow" = 1.8, 
           "Druid-Restoration" = 1.25, 
           "Druid-Feral Cat" = 3.13, 
           "Druid-Feral Bear" = 1.25, 
           "Druid-Balance" = 1.875,
           "Shaman-Restoration" = 3, 
           "Shaman-Elemental" = 2, 
           "Shaman-Enhancement" = 2,
           "Monk-Mistweaver", 
           "Monk-Brewmaster", 
           "Monk-Windwalker",
           "Death Knight-Blood" = 6.25, 
           "Death Knight-Frost" = 2, 
           "Death Knight-Unholy" = 2,
           "Hunter-Beast Mastery" = 2, 
           "Hunter-Marksmanship" = 2, 
           "Hunter-Survival" = 1,
           "Mage-Arcane" = 2, 
           "Mage-Fire" = 1.5, 
           "Mage-Frost" = 2,
           "Rogue-Assassination" = 3.5, 
           "Rogue-Combat" = 2, 
           "Rogue-Subtlety" = 3,
           "Warlock-Affliction" = 3.1, 
           "Warlock-Demonology" = 1, 
           "Warlock-Destruction" = 3,
           "Warrior-Arms" = 2.2, 
           "Warrior-Fury" = 1.4, 
           "Warrior-Protection" = 2.2)
  
  })
  
  # Generate a summary of the dataset

  stamina_b <- 649
  intellect_b <- 122
  spirit_b <- 121
  agility_b <- 107
  strength_b <- 176

  basestat <- reactive({
    

    
    # Compose data frame
    data.frame(
      Name = c("Health",
               "Stamina",
               "Intellect",
               "Spirit",
               "Agility",
               "Strength"),
      Value = as.character(c(
        146663 + (input$stamina_I+stamina_b-20)*14 + 20, 
        stamina_b + input$stamina_I
        intellect_b + input$intellect_I
        spirit_b + input$spirit_I
        agility_b + input$agility_I
        strength_b + input$strength_I)), 
      stringsAsFactors=FALSE)
  }) 

  secondarystat <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Mastery",
               "Critical Strike-Spell",
               "Critical Strike-Melee",
               "Critical Strike-Ranged",
               "Haste-Spell",
               "Haste-Melee",
               "Haste-Ranged",
               "Dodge",
               "Parry",
               "Expertise",
               "Hit-Spell",
               "Hit-Melee",
               "Hit-Ranged",
               "Spellpower"),
      Value = as.character(c(        
        input$mastery_I/600
        input$criticalstrike_I/600 + (intellect_b + input$intellect_I)/2533.66
        input$criticalstrike_I/600 + (agility_b + input$agility_I)/1259.52
        input$criticalstrike_I/600 + (agility_b + input$agility_I)/1259.52
        input$haste_I/425
        input$haste_I/425
        input$haste_I/425
        input$dodge_I/885
        input$parry_I/885
        input$expertise_I/340
        input$hit_I/340
        input$hit_I/340
        input$hit_I/340
        (intellect_b + input$intellect_I)+10+input$sp_I)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the first "n" observations
    output$basestat <- renderTable({
      sliderValues()
    })

    output$secondarystat <- renderTable({
      sliderValues()
    })


})
