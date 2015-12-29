---
title: "Happy New Year using a Heatmap !?"
author: "Riddhiman Roy"
date: "December 29th 2015"
output:
  html_document:
    keep_md: yes
    self_contained: yes
layout: post_nocomments
---

Just because we all like numbers doesn't mean we can't have some fun. The following example uses [Plotly](https://plot.ly/r/).

Here's to wishing to everyone a very **Happy New Year !**


{% highlight r %}
# install.packages("jpeg") 

library(jpeg)
library(plotly)

# Download a jpeg file from imgur
URL <- "http://i.imgur.com/FWsFq6r.jpg"
file <- tempfile()
download.file(URL, file, mode = "wb")

# Read in JPEG file
j <- readJPEG(file)
j <- j[,,1]

# Create an empty matrix
img.mat <-  mat.or.vec(nrow(j), ncol(j))

# Identify elements where there is data
idx <- j > 0

# Add some glitter like effect
img.mat[idx] <-  sample(x = seq(0,1,by = 0.1), size = sum(idx), replace = T)

# Add some glitter to background
idx <-  j == 0
img.mat[idx] <-  sample(seq(0.7,0.9,0.01), size = sum(idx), replace = T)

# Invert the matrix or else it prints upside down
img.mat[nrow(img.mat):1,] <- img.mat[1:nrow(img.mat),]

# Plot !!!
x.axisSettings <- list(
  title = "Learn from yesterday, live for today, hope for tomorrow. The important thing is not to stop questioning. -Albert Einstein",
  titlefont = list(
    family = 'Arial, sans-serif',
    size = 12,
    color = 'black'
  ),
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  ticks = ""
)

y.axisSettings <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  ticks = ""
)


bordercolor = "#ffa64d"
borderwidth = 20

trianglecolor = "#009933"

nCol = ncol(img.mat)
nRow = nrow(img.mat)

plot_ly(z = img.mat, colorscale = "Hot", type = "heatmap", showscale = F, hoverinfo = "x+y") %>%
  layout(xaxis = x.axisSettings,
         yaxis = y.axisSettings,

         # Add a border
         shapes = list(

           # left border
           list(type = 'rect', fillcolor = bordercolor, line = list(color = bordercolor),
                x0 = 0, x1 = borderwidth,
                y0 = 0, y1 = nRow),

           # Right border
           list(type = 'rect', fillcolor = bordercolor, line = list(color = bordercolor),
                x0 = nCol - borderwidth, x1 = nCol,
                y0 = 0, y1 = nRow),

           # Top border
           list(type = 'rect', fillcolor = bordercolor, line = list(color = bordercolor),
                x0 = 0, x1 = nCol,
                y0 = nRow, y1 = nRow - borderwidth),

           # Bottom border
           list(type = 'rect', fillcolor = bordercolor, line = list(color = bordercolor),
                x0 = 0, x1 = nCol,
                y0 = 0, y1 = borderwidth)))
{% endhighlight %}
{::nomarkdown}<embed src = "/assets/figures/HappyNewYear/HappyNewYear.html" height = 600px width = 800px />{:/}
