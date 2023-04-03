library(countrycode) # to switch between country names and codes
library(png) # to read png images
library(emojifont) # to insert logos and emojis
library(extrafont) # to embed extra fonts
library(sysfonts) # to check available fonts and download fonts from google
library(showtext) # to use the extra fonts

## Function to place images by center points on the two axes rather than corners by Stack Overflow user 'Marc in the box', retrieved from: https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r
addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}


## add custom fonts
font_add_google('Quattrocento') #get the fonts 
font_add_google('Quattrocento Sans')
font_families() #check that the fonts are installed and available

showtext_auto() #this is to turn on the custom fonts availability
showtext_opts(dpi = 96) #set the resolution: 96 is default

## color settings
background.color = rgb(248, 244, 255, max=255) # color for the background: magnolia

dark.color = rgb(24, 24, 38, max=255) # dark color: almost black

red.1 = rgb(237, 41, 57, max=255) # default red
blue.1 = rgb(57, 41, 237, max=255) # default red

green.1 = rgb(0, 173, 67, max=255) # default green
green.dark = rgb(0, 66, 26, max=255) # dark green
green.light = rgb(15, 255, 108, max=255) # light green

blue.twitter = rgb (29, 161, 242, max=255) # twitter blue

my_theme_2 <- function(){ 
  font <- "Quattrocento"   #assign font family up front
  theme_bw() %+replace%    #replace elements we want to change
    theme(
      plot.margin = unit(c(1,1,1,1), "lines"),
      
      #grid elements
      panel.border = element_blank(),
      panel.background = element_blank(),
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),   
      axis.ticks = element_blank(),          #strip axis ticks
      
      #axis.ticks.length.x =unit(.10, "cm"),
      #axis.ticks.y = element_blank(),
      
      #plot.background = element_rect(fill='white', colour='black'),
      
      #text elements
      plot.title = element_text(             #title
        color = 'black',
        family = font,            #set font family
        size = 26,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        color = 'black',
        family = font,            #font family
        face = 'italic',
        size = 16,
        hjust = 0,                #left align
        vjust = 1),               #raise slightly
      
      plot.caption = element_text(           #caption
        color = 'darkgrey',
        family = font,            #font family
        size = 12,  
        hjust = 1, 
        vjust = -2),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 16,
        face = 'italic',
        vjust = 0.5),               #font size
      
      axis.text.x = element_text(              #axis text
        family = font,            #axis famuly
        size = 12,
        #face = 'italic',
        vjust=1),                #font size
      
      axis.text.y = element_text(              #axis text
        family = font,            #axis famuly
        size = 12,
        vjust = 0.5,
        hjust = 1)                #font size
      
    )
}

my_theme_3 <- function(){ 
  font <- "Quattrocento"   #assign font family up front
  theme_bw() %+replace%    #replace elements we want to change
    theme(
      plot.margin = unit(c(1,1,1,1), "lines"),
      
      #grid elements
      panel.border = element_blank(),
      panel.background = element_blank(),
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),   
      axis.ticks = element_blank(),          #strip axis ticks
      
      #axis.ticks.length.x =unit(.10, "cm"),
      #axis.ticks.y = element_blank(),
      
      #plot.background = element_rect(fill='white', colour='black'),
      
      #text elements
      plot.title = element_text(             #title
        color = 'black',
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        color = 'black',
        family = font,            #font family
        face = 'italic',
        size = 18,
        hjust = 0,                #left align
        vjust = 1),               #raise slightly
      
      plot.caption = element_text(           #caption
        color = 'darkgrey',
        family = font,            #font family
        size = 18,  
        hjust = 1, 
        vjust = -2),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 16*3,
        face = 'italic',
        vjust = 0.5),               #font size
      
      axis.text.x = element_text(              #axis text
        family = font,            #axis famuly
        size = 14*3,
        #face = 'italic',
        vjust=1),                #font size
      
      axis.text.y = element_text(              #axis text
        family = font,            #axis famuly
        size = 14*3,
        vjust = 0.5,
        hjust = 1)                #font size
      
    )
}
