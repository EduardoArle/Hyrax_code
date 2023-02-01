#load necessary packages
library(raster)

#list wds
wd_hyrax <- setwd('/Volumes/INTENSO/Hyrax data')

#load suitability maps of both species
hyrax <-  raster('hyrax.tif')
sandfly <- raster('andfly.tif')

######### CLASSIFY EACH MAP INTO LOW, MEDIUM, OR HIGH SUITABILITY BY TERTILES #######

#inform the thresholds (th) of each model (external info from MaxEnt)
th_hyrax <- 0.53
th_sandfly <- 0.28

# Use threshold MAX SENS + SPEC to divide the data in two portions

#get all values above the thresold
hyrax_top <- hyrax[hyrax[] > th_hyrax]
sandfly_top <- sandfly[sandfly[] > th_sandfly]

#values >= the median of the values above the th considered HIGH Habitat Suitability (HS)
th_medium_high_hyrax <- median(hyrax_top)
th_medium_high_sandfly <- median(sandfly_top)

#values above the th but below the aforementioned median considered MEDIUM HS


#get all values below the thresold
hyrax_bottom <- hyrax[hyrax[] < th_hyrax]
sandfly_bottom <- sandfly[sandfly[] < th_sandfly]

#values <= the median of the values below the th considered LOW HS
th_low_medium_hyrax <- median(hyrax_bottom)
th_low_medium_sandfly <- median(sandfly_bottom)

#values below the th but above the aforementioned median also considered MEDIUM HS

# Now we have created two thresholds for each species to divide the data into three !

### HYRAX ###
#duplicate the raster object
hyrax_3_classes <- hyrax

#reassess values above the medium-to-high th to 3
hyrax_3_classes[which(hyrax_3_classes[] >= th_medium_high_hyrax)] <- 3

#reassess values between the low-to-medium th and the medium-to-high th to 2
hyrax_3_classes[which(hyrax_3_classes[] <= th_medium_high_hyrax &
                      hyrax_3_classes[] >= th_low_medium_hyrax)] <- 2

#reassess values below the low-to-medium th to 1
hyrax_3_classes[which(hyrax_3_classes[] <= th_low_medium_hyrax)] <- 1


### SANDFLY ###
#duplicate the raster object
sandfly_3_classes <- sandfly

#reassess values above the medium-to-high th to 30
sandfly_3_classes[which(sandfly_3_classes[] >= th_medium_high_sandfly)] <- 30

#reassess values between the low-to-medium th and the medium-to-high th to 2
sandfly_3_classes[which(sandfly_3_classes[] <= th_medium_high_sandfly &
                          sandfly_3_classes[] >= th_low_medium_sandfly)] <- 20

#reassess values below the low-to-medium th to 1
sandfly_3_classes[which(sandfly_3_classes[] <= th_low_medium_sandfly)] <- 10

# Make both rasters have the same extent and resolution

#crop hyrax by sandfly
hyrax_3_classes_b <- crop(hyrax_3_classes, extent(sandfly_3_classes), snap="out")

#resample hyrax to sandfly resolution using method 'ngb' because data is categorical
hyrax_3_classes_c <- resample(hyrax_3_classes_b, sandfly_3_classes, methods = 'ngb')

#round values to get back to 1s, 2s, and 3s
hyrax_3_classes_d <- hyrax_3_classes_c
hyrax_3_classes_d[] <- round(hyrax_3_classes_d[])

# Sum both HS maps
sum_HS <- sum(sandfly_3_classes, hyrax_3_classes_d)

# Plot raster by selected colours

#create break points for the 9 colours
breakpoints <- c(0, 11, 12, 13,
                 21, 22, 23,
                 31, 32, 33, 34)

#create colours in a red to blue double scale
colors <- c('#ffffff','#d8e1fe','#82a3ff',
            '#f9ed9b','#43dd52','#0d928b',
            '#f9dc0a','#0e8813','#053707')
            
plot(sum_HS, breaks = breakpoints, col = colors)


'#82a3ff''#0d928b''#053707'
'#d8e1fe''#43dd52''#0e8813'
'#ffffff''#f9ed9b''#f9dc0a'

#### plot legend


plot(sum_HS, breaks = breakpoints, col = colors, box = F, legend = F, bty = 'n',
     axes = F)

#make lines creating a table (cols)
a <- c(35.58,36)
b <- c(32.1,32.1)
lines(a,b)

a <- c(35.58,36)
b <- c(31.98,31.98)
lines(a,b)

a <- c(35.58,36)
b <- c(31.86,31.86)
lines(a,b)

a <- c(35.58,36)
b <- c(31.74,31.74)
lines(a,b)

a <- c(35.58,35.58)
b <- c(32.1,31.74)
lines(a,b)

a <- c(35.72,35.72)
b <- c(32.1,31.74)
lines(a,b)

a <- c(35.86,35.86)
b <- c(32.1,31.74)
lines(a,b)

a <- c(36,36)
b <- c(32.1,31.74)
lines(a,b)

cex.pt <- 7
  
#plot squares
points(35.65, 32.04, pch = 15, col = '#82a3ff', cex = cex.pt)
points(35.65, 31.92, pch = 15, col = '#d8e1fe', cex = cex.pt)
points(35.65, 31.80, pch = 15, col = '#ffffff', cex = cex.pt)

points(35.79, 32.04, pch = 15, col = '#0d928b', cex = cex.pt)
points(35.79, 31.92, pch = 15, col = '#43dd52', cex = cex.pt)
points(35.79, 31.80, pch = 15, col = '#f9ed9b', cex = cex.pt)

points(35.93, 32.04, pch = 15, col = '#053707', cex = cex.pt)
points(35.93, 31.92, pch = 15, col = '#0e8813', cex = cex.pt)
points(35.93, 31.80, pch = 15, col = '#f9dc0a', cex = cex.pt)

#legend
text(35.52, 32.04, 'high', cex = 1)
text(35.52, 31.92, 'medium', cex = 1)
text(35.52, 31.80, 'low', cex = 1)

text(35.93, 31.70, 'high', cex = 1)
text(35.79, 31.70, 'medium', cex = 1)
text(35.65, 31.70, 'low', cex = 1)

text(35.44, 31.92, 'Hirax', cex = 1.5, srt = 90)
text(35.79, 31.60, 'Sandfly', cex = 1.5)













####################################################                     
plot(sum_HS, breaks = breakpoints, col = colors)

hyrax_no_low <- hyrax
hyrax_no_low[] <- ifelse(hyrax_no_low[] < th_low_medium_hyrax, 0, hyrax_no_low[])

plot(hyrax_no_low)
                     
'#82a3ff''#0d928b''#053707'
'#d8e1fe''#43dd52''#0e8813'
'#ffffff''#f9ed9b''#f9dc0a'


############################################################################################

#SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP

############################################################################################

plot(hyrax )
plot(sandfly)


test_11 <- sum_HS
test_11[] <- ifelse(test_11[] == 11 , 11, 0)
plot(test_11)

test_12 <- sum_HS
test_12[] <- ifelse(test_12[] == 12 , 12, 0)
plot(test_12)



library(raster)
data(volcano)
volcanoR <- raster(volcano)

#making colors below 100 red and above 180 blue in this example

breakpoints <- c( 94 ,   100,     120,     140,     160,    180,195)
colors <-  c("red","white", "white", "white", "white", "blue")
plot(volcanoR,breaks=breakpoints,col=colors)


colors <- c('#ffffff','#d8e1fe','#82a3ff',
                     '#ffffff','#ffffff','#ffffff',
                     '#ffffff','#ffffff','#ffffff')
                     
plot(sum_HS, breaks = breakpoints, col = colors)


colors <- c('#ffffff','#ffffff','#ffffff',
                     '#f9ed9b','#ffffff','#ffffff',
                     '#f9dc0a','#ffffff','#ffffff')
                     

plot(sum_HS, breaks = breakpoints, col = colors)

colors <- c('#ffffff','#ffffff','#ffffff',
                     '#ffffff','#43dd52','#0d928b',
                     '#ffffff','#0e8813','#053707')
                     

plot(sum_HS, breaks = breakpoints, col = colors)

colors <- c('#ffffff','#ffffff','#d8e1fe','#82a3ff',
                     '#ffffff','#43dd52','#0d928b',
                     '#ffffff','#0e8813','#053707','#053707')
                     
plot(sum_HS, breaks = breakpoints, col = colors)

#create colours in a red to blue double scale
colors <- c('#ffffff','#ffffff','#ffffff','#ffffff',
                     '#f9ed9b','#43dd52','#0d928b',
                     '#f9dc0a','#0e8813','#053707','#053707')
                     