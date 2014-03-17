library(Cairo)
library(scales)


load('design_pal.rdata')
design.cols <- design_pal$primaries

wgsproj <- "+init=epsg:4326"
nztmproj <- "+init=epsg:2193"


rename.levels <- function (x, orig, new) 
  {
    if (!is.factor(x)) 
        stop("x must be a factor")
    if (length(orig) != length(new)) 
        stop("Number of new labels must equal number of old labels.")
    for (i in 1:length(orig)) {
        levels(x)[levels(x) == orig[i]] <- new[i]
    }
    return(x)
  }

## Function to draw a series of ticks from an origin
## direction dir can be 'L': left, 'R': right, 'H': horizontal (centre), 'T': top, 'B': bottom, 'V': vertical (centre)
draw.ticks <- function(xy_fixed, at, dir, labs, len=unit(2,'mm'), lab.fontsize=6,
                       col='black', col.lab=col, spacing=unit(0.5, 'mm'), draw.axis=F,
                       tick.lwd=1, lims=unit.c(min(at),max(at)), ...) {
    tick_at <- rep(at, each=2)
    if (dir %in% c('R', 'T'))
        tick_fixed <- rep(unit.c(xy_fixed, xy_fixed + len), length(at))  else
    if (dir %in% c('L', 'B'))
        tick_fixed <- rep(unit.c(xy_fixed - len, xy_fixed), length(at))  else
    tick_fixed <- rep(unit.c(xy_fixed - 0.5*len, xy_fixed + 0.5*len), length(at))
    if (dir %in% c('L', 'R', 'H')) {
        x <- tick_fixed; y <- tick_at
        axis_x <- rep(xy_fixed, 2); axis_y <- lims
    }  else {
        x <- tick_at; y <- tick_fixed
        axis_x <- lims; axis_y <- rep(xy_fixed, 2)
    }
    ids <- rep(1:length(at), each=2)
    grid.polyline(x, y, id=ids, gp=gpar(col=col, lineend='square', lwd=tick.lwd))
    if (draw.axis == T) grid.lines(x=axis_x, y=axis_y, gp=gpar(col=col, lineend='square'))
    gplab <- gpar(fontsize = lab.fontsize, col=col.lab)
    if (dir == 'L') 
        grid.text(labs, x=xy_fixed - len - spacing, y=at, hjust=1, gp=gplab) else
    if (dir == 'R')
        grid.text(labs, x=xy_fixed + len + spacing, y=at, hjust=0, gp=gplab) else
    if (dir == 'B') 
        grid.text(labs, y=xy_fixed - len - spacing, x=at, vjust=1, gp=gplab) else
    if (dir == 'T')
        grid.text(labs, y=xy_fixed + len + spacing, x=at, vjust=0, gp=gplab)
    }

## Function to darken or lighten a colour
darken <- function (col, c = 0.3) 
{
    rgbs <- col2rgb(col, alpha=T)
    r <- rgbs["red", ]/255
    g <- rgbs["green", ]/255
    b <- rgbs["blue", ]/255
    rd <- r * (1 - c)
    gd <- g * (1 - c)
    bd <- b * (1 - c)
    return(rgb(rd, gd, bd, rgbs["alpha",]/255))
}
lighten <- function (col, c = 0.5) 
{
    rgbs <- col2rgb(col, alpha=T)
    r <- rgbs["red", ]/255
    g <- rgbs["green", ]/255
    b <- rgbs["blue", ]/255
    rd <- r + c * (1 - r)
    gd <- g + c * (1 - g)
    bd <- b + c * (1 - b)
    return(rgb(rd, gd, bd, rgbs["alpha",]/255))
}

## Capitalise first letter
upper1st <- function (x) 
  {
      x1 <- strsplit(x, "")
      x <- sapply(x1, function(x) {
          x[1] <- toupper(x[1])
          return(paste(x, collapse = ""))
      })
      return(x)
  }



###  Formatting to follow designer's style
font_family = 'Helvetica'
CairoFonts(
    regular="Helvetica:style=Book",
    bold="Helvetica:style=Medium",
    italic="Helvetica:style=BookItalic",
    bolditalic="Helvetica:style=MediumItalic"
    )

ptsize <- ggplot2:::.pt ## to convert fontsize in points to ggplot size (see ggplot source in geom-text.r)
## Graph title
font_graph_title <- 'Helvetica-Medium'
size_graph_title <- 10
leading_graph_title <- 13
lheight_title <- leading_graph_title/size_graph_title  ## to achieve leading of 13 pts (to be confirmed)
gpar_title <- function(...) gpar(fontsize=size_graph_title, fontfamily=font_graph_title, fontface='plain',
                                 lineheight=lheight_title, ...)
## Graph normal text
font_graph_text <- 'Helvetica-Book'
size_graph_text <- 7
size_graph_text_gg <- size_graph_text / ptsize
leading_graph_text <- 8.5
lheight_text <- leading_graph_text/size_graph_text
element_graph_text <- function(face='plain', colour='black', hjust=0, vjust=0.5, angle=0)
    element_text(family=font_graph_text, size=size_graph_text,
                 lineheight=lheight_text*.8, face=face, colour=colour,
                 hjust=hjust, vjust=vjust, angle=angle)
gpar_text <- function(...) gpar(fontsize=size_graph_text, fontfamily=font_graph_text,
                                fontface='plain', lineheight=lheight_text, ...)
gtext <- function(...) geom_text(..., size=size_graph_text_gg, family=font_graph_text,
                                 lineheight=lheight_text)

## Graph small text (not from the designer)
font_graph_smalltext <- 'Helvetica-Book'
size_graph_smalltext <- 6
leading_graph_smalltext <- 7.5
lheight_smalltext <- leading_graph_smalltext/size_graph_smalltext
element_graph_smalltext <- function(face='plain', colour='black', hjust=0, vjust=0.5,
                                    angle=0)
    element_text(family=font_graph_smalltext, size=size_graph_smalltext,
                 lineheight=lheight_smalltext, face=face, colour=colour,
                 hjust=hjust, vjust=vjust, angle=angle)
gpar_smalltext <- function(...) gpar(fontsize=size_graph_smalltext, fontfamily=font_graph_smalltext,
                                     fontface='plain', lineheight=lheight_smalltext, ...)
gsmalltext <- function(...) geom_text(..., size=size_graph_smalltext / ptsize, family=font_graph_smalltext,
                                      lineheight=lheight_smalltext)

## Graph source
font_graph_source <- 'Helvetica-Book'
size_graph_source <- 6
leading_graph_source <- 7.5
lheight_source <- leading_graph_source/size_graph_source
gpar_source <- function(...) gpar(fontsize=size_graph_source, fontfamily=font_graph_source, fontface='plain',
                                  lineheight=lheight_source, ...)



###  Pre-formatted output
## Takes a plotting function to make both PDF and PNG, a file name without extension,
## a format (one of 'fullpage', 'halfpage', 'quarterpage'),
## and a margin in inches (vector of length 2 for horizontal and vertical, or single value for both
plot2files <- function(plot_cmd, filename, format=NULL, margin=c(.5,.5),
                       width=NULL, height=NULL, png_dpi=600, pointsize=12, ...) {
    if (length(margin) == 1)   margin <- rep(margin, 2)
    a4 <- c(width=8.3, height=11.7)
    a4_ratio <- a4[['height']] / a4[['width']]

    if (!is.null(width) & !is.null(height)) {
        W <- width; H <- height
    } else {
        if (!(format %in% c('fullpage', 'halfpage', 'quarterpage')))
            stop("Format unrecognised - currently, only 'fullpage', 'halfpage', 'quarterpage'")
        if (format == 'fullpage') {
            W <- a4[['width']]  - 2*margin[1]
            H <- a4[['height']] - 2*margin[2]
        } else if (format == 'halfpage') {
            W <- a4[['width']]    - 2*margin[1]
            H <- a4[['height']]/2 - 2*margin[2]
        } else { ## quarter page
            W <- a4[['width']]/2  - 2*margin[1]
            H <- a4[['height']]/2 - 2*margin[2]
        }
    }

    ## Make PDF
    Cairo(W, H, file=sprintf('%s.pdf', filename),  type="pdf", onefile=T,
          pointsize=pointsize, bg = "transparent", canvas = "white", units = "in")
    plot_cmd
    dev.off() 
    ## Convert PDF to PNG
    system(sprintf('pdftoppm -png -r %i %s.pdf pdfconvert', png_dpi, filename))
    file.rename('pdfconvert-1.png', sprintf('%s.png', filename))
}

myreplace <- function (what, lookup, verbose = T, warn = T) 
{
    w <- what
    lkup <- matrix(lookup, ncol = 2, byrow = T)
    c <- w %in% lkup[, 1]
    if (warn) {
        cond <- !(lkup[, 1] %in% what)
        if (sum(cond)) 
            warning(sum(cond), " value(s) in lookup vector not in original data")
    }
    w[c] <- lkup[match(w[c], lkup[, 1]), 2]
    s <- sum(w != what, na.rm = T)
    if (verbose) 
        cat("\nReplaced ", s, " values out of ", length(w), " (", 
            round(100 * s/length(w), 2), "%).\n", sep = "")
    return(w)
}


clean.string <- function(x) {
    ## remove heading and trailing spaces, multiple spaces, multiple tabs, and consecutive tabs or spaces
    return(gsub('[\t ]{2,}', ' ', gsub('\t{2,}', '\t', gsub(' {2,}', ' ', gsub('^ *| *$', '', x)))))
}
