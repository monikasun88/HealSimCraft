library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Basic Character Statistics"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view. The helpText function is also used to 
  # include clarifying text. Most notably, the inclusion of a 
  # submitButton defers the rendering of output until the user 
  # explicitly clicks the button (rather than doing it immediately
  # when inputs change). This is useful if the computations required
  # to render output are inordinately time-consuming.
  sidebarPanel(
    h4("Enter stats here:"),
    helpText("Please enter only stat values from items only.",
             "Disregard Base stats."),
    
    numericInput("stamina_I", "Stamina:", 0),
    numericInput("intellect_I", "Intellect:", 0),
    numericInput("spirit_I", "Spirit:", 0),    
    numericInput("strength_I", "Strength:", 0),
    numericInput("agility_I", "Agility:", 0),
    
    numericInput("mastery_I", "Mastery:", 0),
    numericInput("criticalstrike_I", "Critical Strike:", 0),
    numericInput("sp_I", "Spell Power(weapon):", 0),
    numericInput("haste_I", "Haste:", 0),    
    numericInput("dodge_I", "Dodge:", 0),
    numericInput("parry_I", "Parry:", 0),
    numericInput("expertise_I", "Expertise:", 0),
    numericInput("hit_I", "Hit:", 0),
    
    selectInput("spec", "Choose your spec:", 
                choices = c("Paladin-Holy","Paladin-Protection","Paladin-Retribution",
    "Priest-Discipline", "Priest-Holy", "Priest-Shadow", 
    "Druid-Restoration", "Druid-Feral Cat", "Druid-Feral Bear", "Druid-Balance",
    "Shaman-Restoration", "Shaman-Elemental", "Shaman-Enhancement",
    "Monk-Mistweaver", "Monk-Brewmaster", "Monk-Windwalker",
    "Death Knight-Blood", "Death Knight-Frost", "Death Knight-Unholy",
    "Hunter-Beast Mastery", "Hunter-Marksmanship", "Hunter-Survival",
    "Mage-Arcane", "Mage-Fire", "Mage-Frost",
    "Rogue-Assassination", "Rogue-Combat", "Rogue-Subtlety",
    "Warlock-Affliction", "Warlock-Demonology", "Warlock-Destruction",
    "Warrior-Arms", "Warrior-Fury", "Warrior-Protection")),    
    
    submitButton("Update View")
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations. Note the use of the h4 function to provide
  # an additional header above each output section.
  mainPanel(
    h4("Summary"),
    mainPanel(
      tableOutput("basestat")
    )
    
    h4("Observations"),
    mainPanel(
      tableOutput("secondarystat")
    )
  )
))
