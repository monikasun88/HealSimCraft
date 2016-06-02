library(shiny)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested racial coefficient
  raceInput <- reactive({
    switch(input$race,
           "Tauren" = 146663*0.05,
           "Troll" = 0,
           "Blood Elf" = 0,
           "Pandaren" = 0,
           "Undead" = 0,
           "Goblin" = 425,
           "Orc" = 0,
           "Worgen" = 600,
           "Draenei" = 340,
           "Night Elf" = 1770,
           "Gnome" = 0,
           "Dwarf" = 0,
           "Human" = 0)
    
  })
  
  # Return the requested mastery coefficient
  # http://www.wowpedia.org/Mastery
  specInput <- reactive({
    switch(input$spec,
           "Paladin-Holy" = 1.5,
           "Paladin-Protection" = 1,
           "Paladin-Retribution" = 1.85,
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
           "Monk-Mistweaver" = 1, 
           "Monk-Brewmaster" = 1.6, 
           "Monk-Windwalker" = 5,
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
        146663 + (input$stamina_I-20)*14 + 20 + raceInput(), 
        input$stamina_I,
        input$intellect_I,
        input$spirit_I,
        input$agility_I,
        input$strength_I)), 
      stringsAsFactors=FALSE)
  }) 

  secondarystat <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Mastery (%)",
               "Critical Strike-Spell (%)",
               "Critical Strike-Melee (%)",
               "Critical Strike-Ranged (%)",
               "Haste-Spell (%)",
               "Haste-Melee (%)",
               "Haste-Ranged (%)",
               "Dodge (%)",
               "Parry (%)",
               "Expertise (%)",
               "Hit-Spell (%)",
               "Hit-Melee (%)",
               "Hit-Ranged (%)",
               "Spellpower"),
      Value = as.character(c(        
        input$mastery_I/600*specInput(),
        input$criticalstrike_I/600 + (input$intellect_I)/2533.66 + raceInput(),
        input$criticalstrike_I/600 + (input$agility_I)/1259.52 + raceInput(),
        input$criticalstrike_I/600 + (input$agility_I)/1259.52 + raceInput(),
        input$haste_I/425 + raceInput(),
        input$haste_I/425 + raceInput(),
        input$haste_I/425 + raceInput(),
        input$dodge_I/885 + raceInput(),
        input$parry_I/885,
        input$expertise_I/340,
        input$hit_I/340 + raceInput(),
        input$hit_I/340 + raceInput(),
        input$hit_I/340 + raceInput(),
        (input$intellect_I)+10+input$sp_I)), 
      stringsAsFactors=FALSE)
  }) 
  
  output$baseS <- renderTable({
    basestat()
  })
  
  output$secondaryS <- renderTable({
    secondarystat()
  })
  
})
