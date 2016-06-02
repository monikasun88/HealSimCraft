setInternet2(TRUE)

library(shiny)
library(reshape)

# URL of Armory API Import Character
url_name <- "http://us.battle.net/api/wow/character/Balnazzar/Monika?fields=stats"

# Download file and save
download.file(url_name, "armoryimport.txt")

# Read and reconstruct information to dataframe
armimport <- read.table("armoryimport.txt", sep = ",", header = FALSE, quote="", na.strings="", comment.char="", stringsAsFactors = FALSE)
armimport <- gsub("\"", "", armimport, fixed = TRUE)
armimport <- as.matrix(as.vector(sapply(armimport, function(x) gsub("\\{|\\}", "", x))), ncol = 1)

head_arm <- armimport[1:11]
stats_arm <- armimport[grep("stats:", armimport):length(armimport)]
stats_arm[1] <- gsub("stats:", "", stats_arm[1])

stats_arm <- colsplit(stats_arm, ":", c("class", "value"))

# Display major fields
majorfields <- subset(stats_arm, class %in% c("int", "spr", "mastery", "masteryRating", "crit", "critRating", "haste", "hasteRating", "spellHaste", "spellHasteRating", "spellPower", "mana5", "mana5Combat"))
majorfields$class <- as.character(majorfields$class)
majorfields$value <- as.numeric(as.character(majorfields$value))

raidbuff_majorfields <- data.frame(matrix(as.numeric(t(majorfields)[2,]), nrow = 1))
colnames(raidbuff_majorfields) <- majorfields[[1]]
raidbuff_majorfields$int <- 1.05 * (raidbuff_majorfields$int + 1000 + 300)
raidbuff_majorfields$masteryRating <- raidbuff_majorfields$masteryRating + 3000
raidbuff_majorfields$mastery <- raidbuff_majorfields$mastery + (3000/480)
raidbuff_majorfields$haste <- raidbuff_majorfields$haste + 10
raidbuff_majorfields$crit <- raidbuff_majorfields$crit + 5
raidbuff_majorfields$spellHaste <- raidbuff_majorfields$spellHaste + 5
raidbuff_majorfields$spellPower <- (raidbuff_majorfields$int + (majorfields[majorfields$class == "spellPower",]$value - majorfields[majorfields$class == "int",]$value)) * 1.1









