library(ggplot2)


# Define Constants
srz_ppm <- 3
lic_ppm <- 3
ibhs <- 1.13
hlg <- 1

haste_rating <- 3506
haste_per <- 1.05*1.1*(haste_rating/425.19/100+1)
spellpower <- 23853

rppm <- ppm * haste * tla / 60

# lic

lic_rppm <- NULL

for (a in seq(1000,10000,100)) {
  for (b in seq(0.1,5,0.2)) {
    lic_rppm_temp <- c(a, b, 1.05*1.1*(a/425.19/100+1)*lic_ppm*b/60)
    lic_rppm <- rbind(lic_rppm, lic_rppm_temp)
  }
}

lic_rppm <- data.frame(lic_rppm)
names(lic_rppm) <- c("HasteRating", "TimeSinceAttack", "ChanceToProc")

ggplot(lic_rppm, aes(HasteRating, TimeSinceAttack, fill = ChanceToProc)) + geom_raster() + 
  scale_fill_gradientn(colours = rainbow(7))

# Time since last proc

TimeSinceLastSuccessfulProc <- 72
AverageProcInterval <- 45

F <- max(1, 1+((TimeSinceLastSuccessfulProc/AverageProcInterval)-1.5)*3)

lic_factor <- NULL

for (a in seq(1,60,1)) {
  for (b in seq(1,60,1)) {
    lic_factor_temp <- c(a, b, max(1,1+((a/b)-1.5*3)))
    lic_factor <- rbind(lic_factor , lic_factor_temp )
  }
}

lic_factor <- data.frame(lic_factor)
names(lic_factor) <- c("TimeSinceLastSuccessfulProc", "AverageProcInterval", "Factor")

ggplot(lic_factor, aes(TimeSinceLastSuccessfulProc, AverageProcInterval, fill = Factor)) + geom_raster() + 
  scale_fill_gradientn(colours = rainbow(7))