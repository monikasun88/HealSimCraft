setInternet2(TRUE)

theme_bw()

# Read in trinket logs
setwd("C:/Documents and Settings/jmsun/My Documents/Dropbox/Public")
trinketlog <- read.csv("trinketlogs.txt", sep = "\t")

# Look at lightning-imbued chalice (LIC)
LIC_logs <- subset(trinketlog, Trinket == "Lightning-Imbued Chalice")

LIC_healingdone <- list()
LIC_buffs <- list()

for (a in 1:nrow(LIC_logs)) {
  
  getData <- getWoLInfo(as.character(LIC_logs$World.of.Logs.Link[a]))
  LIC_healingdone[[a]] <- getData[[1]]
  LIC_buffs[[a]] <- getData[[2]]

}

# Filter logs by only those with the LIC buff and in English
LIC_healingdone_sub <- LIC_healingdone[unlist(lapply(LIC_healingdone, function(x) "Restoration of the Infinite" %in% x$Spell_Total))]
LIC_buffs_sub <- LIC_buffs[unlist(lapply(LIC_healingdone, function(x) "Restoration of the Infinite" %in% x$Spell_Total))]
LIC_logs_sub <- LIC_logs[unlist(lapply(LIC_healingdone, function(x) "Restoration of the Infinite" %in% x$Spell_Total)),]

RoI_healingdone <- as.numeric(unlist(lapply(LIC_healingdone_sub, function(x) subset(x,Spell_Total == "Restoration of the Infinite")$"Healing done_Total")))
RoI_healingdone_overheal <- unlist(lapply(LIC_healingdone_sub, function(x) subset(x,Spell_Total == "Restoration of the Infinite")$"Overheal_Total"))
as.numeric(sub(" %", "", RoI_healingdone_overheal))/100


healingdone <- as.numeric(unlist(lapply(LIC_healingdone_sub, function(x) x[nrow(x),]$"Healing done_Total")))
directhotticks <- as.numeric(unlist(lapply(LIC_healingdone_sub, function(x) x[nrow(x), c("Direct Heals_#")])))+
           
### Quality Control

# Plot healing done by fight length, verfiy a roughly linear increase
qplot(LIC_logs_sub$Length.1, healingdone, color = LIC_logs_sub$Fight, geom = "point", size = 4) +
  scale_color_discrete("Fight") + xlab("Fight Length") + ylab("Healing Done") + scale_size(legend = FALSE) + theme_bw()

hc_healingdone <- hclust(dist(cbind(LIC_logs_sub$Length.1, healingdone)))
plot(hc_healingdone, labels = LIC_logs_sub$Fight, xlab = "Fight", main = "Similarity of Healing Done and Fight Length By Fight")

# Plot direct heals + hots by fight length, verify a roughly linear increase
qplot(LIC_logs_sub$Length.1 , directhotticks, color = LIC_logs_sub$Fight, geom = "point", size = 4) +
  scale_color_discrete("Fight") + xlab("Fight Length") + ylab("Direct Heals + HoT Ticks") +scale_size(legend = FALSE) + theme_bw()

# Estimate SP from first value
est_sp <- NULL 
total_LIC_heal <- NULL

for (a in 1:length(LIC_healingdone_sub)) {
total_LIC_heal[a] <- as.numeric(sub(" %", "", subset(LIC_healingdone_sub[[a]], 
  Spell_Total == "Restoration of the Infinite")$"Healing done_Total"))/
  (1-(as.numeric(sub(" %", "", subset(LIC_healingdone_sub[[a]],    
     Spell_Total == "Restoration of the Infinite")$Overheal_Total))/100))

if (sub(" %", "", subset(LIC_healingdone_sub[[a]], Spell_Total == "Restoration of the Infinite")$"Hits_#")=="-") {
  one_LIC_heal <- total_LIC_heal/(as.numeric(sub(" %", "", subset(LIC_healingdone_sub[[a]], 
      Spell_Total == "Restoration of the Infinite")$"Crits_#")))
} else if (sub(" %", "", subset(LIC_healingdone_sub[[a]], Spell_Total == "Restoration of the Infinite")$"Crits_#")=="-") {
  one_LIC_heal <- total_LIC_heal/(as.numeric(sub(" %", "", subset(LIC_healingdone_sub[[a]], 
   Spell_Total == "Restoration of the Infinite")$"Hits_#")))
} else {
one_LIC_heal <- total_LIC_heal/(as.numeric(sub(" %", "", subset(LIC_healingdone_sub[[a]], 
  Spell_Total == "Restoration of the Infinite")$"Hits_#")) + as.numeric(sub(" %", "", subset(LIC_healingdone_sub[[a]], 
  Spell_Total == "Restoration of the Infinite")$"Crits_#"))*2)
}
  if (LIC_logs_sub[a,]$iLVL == 522) {
    est_sp[a] <- (one_LIC_heal-56071)/1.25
  } else {
    est_sp[a] <- (one_LIC_heal-56286)/1.25
  }
}

# Plot direct heals + hots by fight length, verify a roughly linear increase
ggplot(LIC_logs_sub) +
geom_point(aes(Length.1 , est_sp, color = Fight), size = 4) + 
  geom_text(aes(Length.1 , est_sp, label = Fight), hjust = -0.1, size = 4) +
  scale_color_discrete("Fight") + xlab("Fight Length") + ylab("Healing Done") +scale_size(legend = FALSE) +
  theme_bw()

LIC_buffs_sub_1 <- LIC_buffs_sub[LIC_logs_sub$Version == 1]
LIC_buffs_sub_2 <- LIC_buffs_sub[LIC_logs_sub$Version == 2]

total_LIC_procs_1 <- as.numeric(unlist(lapply(LIC_buffs_sub_1, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total)))
total_LIC_procs_2 <- as.numeric(unlist(lapply(LIC_buffs_sub_2, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total)))

subset(LIC_logs_sub, Version == 1)$Length.1
subset(LIC_logs_sub, Version == 2)$Length.1

fit1 <- summary(lm(total_LIC_procs_1 ~ subset(LIC_logs_sub, Version == 1)$Length.1-1))
fit2 <- summary(lm(total_LIC_procs_2 ~ subset(LIC_logs_sub, Version == 2)$Length.1-1))


ggplot() +
  geom_point(data = subset(LIC_logs_sub, Version == 1), aes(Length.1, total_LIC_procs_1), colour = "blue", size = 4) +
  geom_point(data = subset(LIC_logs_sub, Version == 2), aes(Length.1, total_LIC_procs_2), colour = "red", size = 4) + 
  stat_smooth(aes(x = Length.1, y = total_LIC_procs_1), data = subset(LIC_logs_sub, Version == 1), method="lm", formula=y ~ x-1, se=FALSE) +
  stat_smooth(aes(x = Length.1, y = total_LIC_procs_2), data = subset(LIC_logs_sub, Version == 2), colour = "red", method="lm", formula=y ~ x-1, se=FALSE) +
  xlab("Fight Length") + ylab("Procs") + ggtitle("Red = Version 2 (Buffed), Blue = Version 1") + theme_bw()
  

ggplot() +
  geom_point(data = LIC_logs_sub, aes(as.numeric(unlist(lapply(LIC_buffs, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total))), directhotticks, colour = factor(Version), size = 4))+
  xlab("Procs") + ylab("Direct Heals + HoTs") + scale_size(legend = FALSE) + scale_color_discrete(legend = FALSE) + 
  ggtitle("Red = Version 2 (Buffed), Blue = Version 1") + theme_bw()


ggplot() +
  geom_point(data = LIC_logs_sub, aes(as.numeric(unlist(lapply(LIC_buffs, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total))), directhotticks, colour = factor(Fight), size = 4))+
  xlab("Procs") + ylab("Direct Heals + HoTs") + scale_size(legend = FALSE) +  
  ggtitle("Red = Version 2 (Buffed), Blue = Version 1") + theme_bw()

ggplot() +
  geom_point(data = LIC_logs_sub, aes(as.numeric(unlist(lapply(LIC_buffs, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total))), directhotticks, colour = Length.1, size = 4))+
  xlab("Procs") + ylab("Direct Heals + HoTs") + scale_size(legend = FALSE) + scale_colour_gradientn(colours = rainbow(7), "Fight\nLength") +
  ggtitle("Red = Version 2 (Buffed), Blue = Version 1") + theme_bw()


ggplot() +
  geom_point(data = LIC_logs_sub, aes(as.numeric(unlist(lapply(LIC_buffs, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total))), directhotticks, colour = Length.1, size = 4))+
  xlab("Procs") + ylab("Direct Heals + HoTs") + scale_size(legend = FALSE) + scale_colour_gradientn(colours = rainbow(7), "Fight\nLength") +
  ggtitle("Red = Version 2 (Buffed), Blue = Version 1") + theme_bw()

sub_RoI_healingdone <- data.frame(cbind(Version=LIC_logs_sub$Version, RoI_healingdone, healingdone, iLVL = LIC_logs_sub$iLVL))
ggplot() +
  geom_boxplot(data = sub_RoI_healingdone, aes(x = factor(paste(Version,iLVL)), y = RoI_healingdone/healingdone)) +
  xlab("Version:iLVL") + ylab("LIC % Effective Healing of Total") + ggtitle("Percent of Total Healing from LIC by Item") + theme_bw()


qplot(RoI_healingdone, as.numeric(sub(" %", "", RoI_healingdone_overheal))/100)+
  xlab("Effective Healing") + ylab("Overhealing %") + ggtitle("Overhealing at Different Effective Healing Values") + theme_bw()


ggplot() +
  geom_point(data = LIC_logs_sub, aes(as.numeric(unlist(lapply(LIC_buffs, function(x) subset(x,Name_Total == "Infinite Power")$Amount_Total))), RoI_healingdone, colour = Length.1, size = 4))+
  xlab("Procs") + ylab("Effective Healing") + scale_size(legend = FALSE) + scale_colour_gradientn(colours = rainbow(7), "Fight\nLength") +
  ggtitle("Procs vs Effective Healing") + theme_bw()


# Get expression editor information on holy light
getED <-function(url_name) {
  findfs <- unlist(gregexpr("[/]", as.character(url_name)))  
  return(paste(paste(substr(as.character(url_name), 1, findfs[length(findfs)-2]-1), "xe",
        substr(as.character(url_name), findfs[length(findfs)]+1, nchar(as.character(url_name))), 
               sep = "/"),"&x=spell+%3D+\"Holy+Light\"", sep = ""))
}

# Get holy light urls
temp_url <- sapply(LIC_logs_sub$World.of.Logs.Link, getED)
download.file(temp_url[1], "test.html")




getWoLInfo <- function(url_name) {

# Two example WoL files
download.file(url_name, "test.html")
test <- file("test.html")

# Read in whole page
temp <- scan("test.html", "character", sep = "\n")

# Find beginning (temp_start) and end (temp_end) of healing done charge in HTML code and subset it (sub_temp)
temp_start <- grep("<th rowspan=\"2\" colspan=\"2\">Healing done</th>", temp)[[1]]-3
temp_end <- grep("</table>",temp)[(grep("</table>", temp) > temp_start)][1]
sub_temp <- temp[temp_start:temp_end]

# Row number of file read-in where row begins in Healing done table
temp_num_col <- grep("<tr", sub_temp) 
temp_num_col_end <- grep("</tr", sub_temp) 

# Column number of file read-in where column begins in Healing done table (only sub columns/disregard first row)
# This is a little more complicated and requires finding out how many sub-columns are in each large column in the
# first row of the data

# Finds the number of sub columns in each column
sub_temp_rows <- sub_temp[grep("<tr>", sub_temp)[1]:grep("</tr>", sub_temp)[1]]

# rows with one column
# length(grep("<tr>", sub_temp)[1]:grep("</tr>", sub_temp)[1]) - length(grep("colspan", sub_temp_rows)) - 2

# rows with multiple columns
sub_temp_cols <- sum(as.numeric(substr(sub_temp_rows[grep("colspan", sub_temp_rows)], regexpr("colspan", sub_temp_rows[grep("colspan", sub_temp_rows)])[1:5]+9, 
  regexpr("colspan", sub_temp_rows[grep("colspan", sub_temp_rows)])[1:5]+9))) + (length(grep("<tr>", sub_temp)[1]:grep("</tr>", sub_temp)[1]) - length(grep("colspan", sub_temp_rows)) - 2)

# number of sub-columns for each major column
sub_temp_cols2 <- rep(1, length(sub_temp_rows)-2)
sub_temp_cols2[grep("colspan", sub_temp_rows)-1] <- as.numeric(substr(sub_temp_rows[grep("colspan", sub_temp_rows)], regexpr("colspan", sub_temp_rows[grep("colspan", sub_temp_rows)])[1:5]+9, 
  regexpr("colspan", sub_temp_rows[grep("colspan", sub_temp_rows)])[1:5]+9))

# Manual Way
# sub_temp_rows <- 16
# sub_temp_cols <- 12
# sub_temp_cols2 <- c(1, 2, 3, 3, 3, 3, 1, 1)


# Read each line
contents <- list()
for (a in 1:length(temp_num_col)) {
  row_sub <- sub_temp[temp_num_col[a]:temp_num_col_end[a]]
  beg <- regexpr(">[[:print:]]|'> ", row_sub[2:length(row_sub)-1])
  theend <- regexpr("[[:graph:]]<", row_sub[2:length(row_sub)-1])

  # Spell Name
  if (a > 2) {
    beg_spellname <- regexpr("'> ", row_sub[2:length(row_sub)-1])
    theend_spellname <- regexpr("</span>", row_sub[2:length(row_sub)-1])
    contents[[a]] <- substr(row_sub[beg != -1], beg[beg != -1]+1, theend[theend != -1])
    contents[[a]][1] <-  substr(row_sub[2], beg_spellname[2]+3, theend_spellname[2]-1)
  } else{
    contents[[a]] <- substr(row_sub[beg != -1], beg[beg != -1]+1, theend[theend != -1])
  }
}

# Make new combined column header
newFirstRow <- paste(rep(contents[[1]], sub_temp_cols2), c("Total", "Total", "%", contents[[2]], "Total", "Total"), sep = "_")

newlines <- do.call("rbind", lapply(tail(contents, length(contents)-2), longatedash))
head_newlines <- do.call("rbind", lapply(tail(contents, length(contents)-2), function(x) head(x, 3)))
tail_newlines <- do.call("rbind", lapply(tail(contents, length(contents)-2), function(x) tail(x, 2)))

healingdone_Final <- data.frame(cbind(head_newlines, newlines, tail_newlines), stringsAsFactors = FALSE)
names(healingdone_Final) <- newFirstRow

###  Pull out data for buffs from Buffs cast tab ###

# Find beginning (buffs_cast_start) and end (buffs_cast_end) of healing done charge in HTML code and subset it (sub_buffs_cast)
buffs_start <- grep("<div id='tab-auras-cast'>", temp) + 4
buffs_end <- grep("</table>",temp)[(grep("</table>", temp) > buffs_start)][1]
sub_buffs<- temp[buffs_start:buffs_end]

# Row number of file read-in where row begins in Healing done table
buffs_num_col <- grep("<tr", sub_buffs) 
buffs_num_col_end <- grep("</tr", sub_buffs) 

# Read each line
buffs_contents <- list()
for (a in 1:length(buffs_num_col)) {
  row_sub <- sub_buffs[buffs_num_col[a]:buffs_num_col_end[a]]
  beg <- regexpr(">[[:print:]]|'> ", row_sub[2:length(row_sub)-1])
  theend <- regexpr("[[:graph:]]<", row_sub[2:length(row_sub)-1])

  # Spell Name
  if (a > 1) {
    beg_spellname <- regexpr("'> ", row_sub[2:length(row_sub)-1])
    theend_spellname <- regexpr("</span>", row_sub[2:length(row_sub)-1])
    buffs_contents[[a]] <- substr(row_sub[beg != -1], beg[beg != -1]+1, theend[theend != -1])
    buffs_contents[[a]][1] <-  substr(row_sub[2], beg_spellname[2]+3, theend_spellname[2]-1)
  } else{
    buffs_contents[[a]] <- substr(row_sub[beg != -1], beg[beg != -1]+1, theend[theend != -1])
  }
}

buffsFirstCol <- paste(rep(buffs_contents[[1]], c(1, 1, 3)), c("Total", "Total", "Seconds", "Percent", "Extra"), sep = "_")
buffs_Final <- data.frame(do.call("rbind", tail(buffs_contents,length(buffs_contents)-1)), stringsAsFactors = FALSE)
names(buffs_Final) <- buffsFirstCol

### Pull out data for power gains from Buffs cast tab ###

# Find beginning (pg_start) and end (pg_end) of power gains in HTML code and subset it (sub_pg_cast)
pg_start <- grep("Power gains", temp)[grep("Power gains", temp) > grep("<div id='tab-auras-cast'>", temp)][1]
pg_end <- grep("</table>",temp)[(grep("</table>", temp) > pg_start)][1]
sub_pg<- temp[pg_start:pg_end]

# Row number of file read-in where row begins in Healing done table
pg_num_col <- grep("<tr", sub_pg) 
pg_num_col_end <- grep("</tr", sub_pg) 

# Read each line
pg_contents <- list()
for (a in 1:length(pg_num_col)) {
  row_sub <- sub_pg[pg_num_col[a]:pg_num_col_end[a]]
  beg <- regexpr(">[[:print:]]|'> ", row_sub[2:length(row_sub)-1])
  theend <- regexpr("[[:graph:]]<", row_sub[2:length(row_sub)-1])

  # Spell Name
  if (a > 1) {
    beg_spellname <- regexpr("'> ", row_sub[2:length(row_sub)-1])
    theend_spellname <- regexpr("</span>", row_sub[2:length(row_sub)-1])
    pg_contents[[a]] <- substr(row_sub[beg != -1], beg[beg != -1]+1, theend[theend != -1])
    pg_contents[[a]][1] <-  substr(row_sub[2], beg_spellname[2]+3, theend_spellname[2]-1)
  } else{
    pg_contents[[a]] <- substr(row_sub[beg != -1], beg[beg != -1]+1, theend[theend != -1])
  }
}

pg_Final <- data.frame(do.call("rbind", tail(pg_contents,length(pg_contents)-1)), stringsAsFactors = FALSE)
names(pg_Final) <- pg_contents[[1]]

return(list(healingdone_Final, buffs_Final, pg_Final))

# Functions
longatedash <- function(x) {
  datapoints = x[4:(length(x)-2)]
  if (sum(datapoints == "-") > 0) {
    wheredash = grep("-", datapoints)
    newline = datapoints
    for (b in (length(wheredash)):1) {
      newline = append(newline, c("-", "-"), after = wheredash[b])
    }
  } else {newline <- datapoints}
  return(newline)
}

}
