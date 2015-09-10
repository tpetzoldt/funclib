#-->Von Lars<--

rm(list=ls())  


#======================================================================
# Function type library
#   function examples for biologists
#   version alpha-1, Mar 06/2001
#   
#  R-version 1.2.0 (compiled on Linux Suse 7.0)
#
#  Thomas Petzoldt & Susanne Worischka
#  petzoldt@rcs.urz.tu-dresden.de
#
# NO WARRANTY! May be distributed under GPL 2.0
#======================================================================

#----------------------------------------------------------------------
# Global control parameters
#----------------------------------------------------------------------

lang <- "de"            # language selection: de or en 
creategraphics <- FALSE  # TRUE: create new graphics files
nographics     <- FALSE # TRUE: textonly version for correcting text
if (lang=="de") {
  pagetitle <-   "Funktionstypenbibliothek"
} else {
  pagetitle <-   "Function Type Library"
}


#======================================================================
# subroutines
#======================================================================



#----------------------------------------------------------------------
# function prototype
#----------------------------------------------------------------------
aFunction <- list(
  id = "id",
  name.de = "name_de",
  name.en = "name_en",
  formula = expression(paste(y == a * x)),
  formula.pc = "y = a*x",
  nVariants = 2,
  calc = function(x, i) {a[i]*x},
  leg  = function(i)    {a[i]},
  a    = c(2, 1),
  b    = c(-0.5, -1),
  c    = c(2, 0),
  d    = c(-0.5, -1),
  comment.de = c("Beispiel-Definition"),
  comment.en = c("example definition")
)

#----------------------------------------
# HTML-Header, footer, ... 
#----------------------------------------
# Attention: use \" for quote and \n for newline

header <- paste("<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">",
                "<html>",
                "<head>",
                "<title>",
                pagetitle,
                "</title>",
                "<body bgcolor=\"#33CCFF\">\n", sep="\n")

footer <- "</body>\n</html>"


#----------------------------------------------------------------------
# write HTML output for function descriptions
#----------------------------------------------------------------------
writecontent <- function(htmlfile, imgfile, name, formula.pc, comment) {

        if (lang=="de") {
          ctitle <- "Anwendungsbeispiele"
        } else {
          ctitle <- "Examples and Remarks"
        }
        write(file = htmlfile, paste("<H2>",name, "</H2>", sep=""),  append=TRUE)
        if (nographics == FALSE) {
          write(file = htmlfile,  paste("<img src=\"", imgfile, "\">", sep=""),  append=TRUE)
        }
        write(file =htmlfile, paste("<p>", formula.pc,"</p>"), append = TRUE)
        write(file = htmlfile,  paste("<H3>", ctitle, "</H3>", sep=""),  append=TRUE)
	if (is.null(comment))
          write(file = htmlfile,  "<p>comment is missing!</p>",  append = TRUE)
        else {
          write(file = htmlfile,  "<ul>",  append = TRUE)
          for (i in 1:length(comment)) {
             write(file = htmlfile,  paste("<li>",  comment[i],  "</li>"), append = TRUE)
          }
          write(file = htmlfile,  "</ul>",  append = TRUE)
        }

	write(file = htmlfile,  "<HR width=\"100%\">\n",  append=TRUE)        
}

#----------------------------------------------------------------------
# Core-function for plotting and generation of HTML output
#----------------------------------------------------------------------
plotit <- function (aFunction,  x,  miny=-Inf,  maxy=+Inf, legpos=0) {

	attach(aFunction)
	print(id)
        
        # ---- set file name for graphics
        imgfile <- paste(id, ".png", sep="")
        
        if (lang=="de") {
           name <- name.de
           comment <- comment.de
        } else {
           name <- name.en
           comment <- comment.en
        }
        
  
        # ---- create menue entry

        funcfile <- paste(id, ".html", sep="")
        write (file=menufile,  paste("<li><a href=\"", funcfile, "\" target=\"content\">",  name, "</a></li>", sep=""),  append=TRUE)


        # ---- Output framed version
        
        write(file = funcfile,  header)        
        writecontent(funcfile, imgfile, name, formula.pc, comment)
        write(file = funcfile, footer, append = TRUE)

        # ---- Output unframed, long version
        writecontent(htmlfile, imgfile, name, formula.pc, comment)
	
	# ----------------------------------------
        # ----    Calculation and Plotting    ----
        # ----------------------------------------

        if (creategraphics) {
            png(filename=imgfile, width=600, height=400)

            # Create empty array
            y<-array(0,c(length(x), nVariants))

            # Create data
            for (i in 1:nVariants) {
              y[,i] <- calc(x,  i)
            }

            # Create legend entries
            col    <- c("darkgreen", "orange", "mediumblue", "red", "violet")

            leg<-array(0, nVariants)
            for (i in 1:nVariants) {
              leg[i]  <- leg(i)
            }

            ymax <- if(maxy==+Inf) max(y) else maxy
            ymin <- if(miny==-Inf) min(y) else miny

            # Create empty plot
            plot (x, y[,1], ylim=c(ymin, ymax), 
                  type="n", xlab="x", ylab="y")

            # zero axis cross
            lines(c(min(x), max(x)), c(0, 0), col="gray")
            lines(c(0, 0), c(ymin, ymax), col="gray")

            # plot functions
            for (i in 1:nVariants) {
              y1 <- ifelse(y[,i] > ymax,  NA,  y[,i])
              y2 <- ifelse(y1 < ymin, NA, y1)
              lines(x, y2, col=col[i], lwd=2)
            }

            # plot title and legend
            title(main=formula)
            # title(main=paste(name,"\n", formula))
            if (legpos == 0) {
            	legend(x = min(x)+(max(x)*0.05), y = ymax, legend=leg, col=col, lwd=2) 
	    } else {
            	legend(x = legpos[1], y = legpos[2], legend=leg, col=col, lwd=2) 
            }
	    
	    # plot formulaname in PC-useable form
 	    	 

            # close graphics file
            dev.off ()
         }
        detach(aFunction)

	#readline("Press RETURN to continue")
} # end plotit



#======================================================================
# Function objects
#======================================================================


expgrow <- list(
  id = "expgrow",
  name.de = "Exponentialfunktion (Exponent&gt;0) ",
  name.en = "Exponential function (exponent&gt;0) ",
  formula = expression(paste(y == a * e^{b*x})),
  formula.pc = "y = a * exp(b * x)", 
  nVariants = 3,
  calc = function(x, i) {a[i] * exp(b[i] * x)},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i])},
  a    = c(2, 1, 1),
  b    = c(0.2, 0.2, 0.1),
  comment.de = c("Organismenzahl als Funktion der Zeit, unlimitiertes Populationswachstum ",
                 "Temperaturabhängigkeit physiologischer und biochemischer Prozesse z.B.Respirationsrate, Sauerstoffzehrung beim biologischen. Abbau"),
  comment.en = c("number of organisms as a function of time; unlimited population growth",
                 "oxygen consumption as a function of temperature",
                 "valid only for a physiological temperature range",
                 "respiration rate as a function of temperature")
)

expdecay <- list(
  id = "expdecay",
  name.de = "Exponentialfunktion (Exponent&lt;0)",
  name.en = "Exponential function (exponent&lt;0)",
  formula = expression(paste(y == a * e^{b*x})),
  formula.pc = "y = a * exp(b * x)", 
  nVariants = 3,
  calc = function(x, i) {a[i] * exp(b[i] * x)},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i])},
  a    = c(8, 5, 5),
  b    = c(-2, -0.2, -2),
  comment.de = c("Zerfallsgesetz, Abbau 1.Ordnung",
		 "Sauerstoffgehalt des Wassers als Funktion der Temperatur",
		 "Elimination von Fremdstoffen aus Organismen (Konzentration als Funktion der Zeit, 1-Kompartiment-Modell)",
		 "Lambert-Beersches-Gesetz (Lichtextinktion als Funktion der Schichtdicke)"),
  comment.en = c("elimination of foreign substances from organisms, concentration as a function of time (one compartment model)")
)

expdecay2 <- list(
  id = "expdecay2",
  name.de = "Exponentialfunktion (Zweikompartimentmodell)",
  name.en = "Exponential function (two compartment model)",
  formula = expression(paste(y == a * e^{b*x} + c * e^{d*x})),
  formula.pc = "y = a * exp(b * x) + c * exp(d*x)",
  nVariants = 2,
  calc = function(x, i) {a[i] * exp(b[i] * x)},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i], ",  c =", c[i], ",  d =", d[i])},
  a    = c(2, 1),
  b    = c(-0.5, -1),
  c    = c(2, 0),
  d    = c(-0.5, -1),
  comment.de = c("Elimination von Fremdstoffen aus Organismen, Konzentration als Funktion der Zeit, 2-Kompartiment-Modell"),
  comment.en = c("elimination of foreign substances from organisms, concentration as a function of time (two compartment model)"),
)

### 2. Potenzunktionen ###

power <- list(
  id = "power",
  name.de = "Potenzfunktion (Exponent&gt;1)",
  name.en = " Power function (exponent&gt;1)",
  formula = expression(paste(y == a * x^b)),
  formula.pc = "y = a * x^b",
  nVariants = 2,
  calc = function(x, i) {a[i] * x^b[i]},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i])},
  a    = c(0.2, 10 ),
  b    = c(4, 2 ),
  comment.de = c("steigt schneller an als Exponentialfunktion, wird nicht f&uuml;r Populationswachstum verwendet",
                  "viele temperaturabhängige physiologische Prozesse z.B. Respirationsrate als Funktion der Temperatur, Sauerstoffzehrung als Funktion der Temperatur"),
  comment.en = c("rises faster than exponential function, not used for population growth",
                  "various temperature-dependent pysiological processes",
                  "respiration rate as a function of temperature",
                  "oxygen consumption as a function of temperature")
)

power2 <- list(
  id = "power2",
  name.de = "Modifizierte Potenzfunktion",
  name.en = "Modified power function",
  formula = expression(paste(y == a * x^b + c)),
  formula.pc = "y = a * x^b +c",
  nVariants = 3,
  calc = function(x, i) {a[i] * x^b[i] + c[i]},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i], ",  c =",  c[i])},
  a    = c(10, 10, 2),
  b    = c(2, 2, 2 ),
  c    = c(0, 100, 100),
  comment.de = c("steigt schneller an als Exponentialfunktion, wird nicht f&uuml;r Populationswachstum verwendet",
                  "viele temperaturabhängige physiologische Prozesse",
                  "Respirationsrate als Funktion der Temperatur",
                  "Sauerstoffzehrung als Funktion der Temperatur"),

  comment.en = c("see power function",
                  "respiration rate as a function of temperature",
                  "oxygen consumption as a function of temperature")
)

root <- list(
  id = "root",
  name.de = "Wurzelfunktion (Potenzfunktion mit 0&lt;Exponent&lt;1)",
  name.en = "Square root function (power function with 0&lt;exponent&lt;1)",
  formula = expression(paste(y == a * x^b)),
  formula.pc = "y = a * x^b",
  nVariants = 2,
  calc = function(x, i) {a[i] * x^b[i]},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i])},
  a    = c(500, 500 ),
  b    = c(0.5, 0.2 ),
  comment.de = c("zwar abflachend, aber nicht gegen einen Grenzwert strebend"),
  comment.en = c("slope decreasing but without a finite saturation")
)

powerinv <- list(
  id = "powerinv",
  name.de = "Inverse Potenzfunktion (b&lt;0)",
  name.en = "Inverse power function (b&lt;0)",
  formula = expression(paste(y == a * x^b)),
  formula.pc = "y = a * x^b",
  nVariants = 2,
  calc = function(x, i) {a[i] * x^b[i]},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i])},
  a    = c(500, 500 ),
  b    = c(-0.2, -1 ),
  comment.de = c("flexible Möglichkeit zur Beschreibung inverser Abhängigkeiten",
		 "geht asymptotisch gegen Null",
                  "Grazingrate von Daphnien als Funktion der Körpermasse",
                  "Spezifischer P-Gehalt des Phytoplanktons als Funktion der Phytoplanktonbiomasse"),
  comment.en = c("asymptotic decrease to zero",
                  "grazing rate as a function of body mass",
                  "specific posphorus content of phytoplankton as a function of phytoplankton biomass")
)

### 3. "Monod-Hyperbel" und Michaelis-Menten-Kinetik ###

monod <- list(
  id = "monod",
  name.de = "Monod-Hyperbel, S&auml;ttigungsfunktion",
  name.en = "Monod's hyperbola, saturation function",
  formula = expression(paste(y == a ~~ frac(x, {b+x}))),
  formula.pc = "y = a * x * (b + x)^-1",
  nVariants = 3,
  calc = function(x, i) {a[i] * x/(b[i] + x)},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i])},
  a    = c(10, 10, 5),
  b    = c(1, 5, 0.1),
  comment.de =c("Universell verwendbare und sehr gebräuchliche Funktion zur Beschreibung von Funktionen und Prozessen",
		 "Photosyntheserate als Funktion des Lichtangebotes", 
                 "Wachstumsrate als Funktion des limitierenden Nährstoffes",
                 "Ingestionsrate von Zooplankton als Funktion der Futterkonzentration",
                 "Mortalitätsrate als Funktion der Populationsdichte",
                 "Michaelis-Menten-Kinetik von Enzymreaktionen: a = µmax (maximale Geschwindigkeit), b = ks (Halbsättigungskonstante)"),

  comment.en =c("photosyntesis rate as a function of light supply",
                 "growth rate as a function of the limiting nutrient",
                 "ingestion rate in zooplankton as a function of food concentration",
                 "mortality rate as a function of population density",
                 "Monod's or Michaelis-Menten's kinetics of enzymatic reactions: a = µmax (maximum speed), d = ks (half saturation constant)")
)


monodinhib <- list(
  id = "monodinhib",
  name.de = "Monod-Hyperbel mit Hemmung",
  name.en = "Monod's hyperbola with inhibition",
  formula = expression(paste(y == a ~~ c ~~ frac(x, (b+x)*(c+x)))),
  formula.pc = "y = a * c * x * (b+x)^-1 * (c+x)^-1",
  nVariants = 2,
  calc = function(x, i) {a[i] * c[i] * x / ((b[i] + x) * (c + x))},
  leg  = function(i)  {paste("a =" ,a[i], ",  b =", b[i], ",  c =", c[i])},
  a    = c(2,2),
  b    = c(0.5,1),
  c    = c(8,8),
  comment.de = c("Wachstumsrate als Funktion des Lichtes, mit Lichthemmung",
                  "beachte dazu die Halbs&auml;ttigungskonstanten: b für Wachstum, (c-b) für Hemmung"),
  comment.en = c("growth rate as a function of light supply, with light inhibition",
                  "note the half saturation constants: b for growth, (c-b) for inhibition")
)

### umgekehrte Monodhyperbel, fallend ###

monodinv <- list(
  id = "monodinv",
  name.de = "Umgekehrte Monod-Hyperbel",
  name.en = "Inverted Monod's hyperbola",
  formula = expression(paste(y == a - c ~~ frac(x, {b - x}))),
  formula.pc = "y = a - c * x * (b-x)^-1",
  nVariants = 2,
  calc = function(x, i) {a[i] - c[i] * x / (b[i] - x)},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i])},
  a    = c(8, 8),
  b    = c(10, 10),
  c    = c(0.1, 1),
  comment.de = c("Beschreibung von Prozessen, die sich von einen Maximalwert beginnend auf Null oder einen kleinen Wert einpegeln",
		 "Filtrationsrate nichtselektierender Zooplankter als Funktion der Futterkonzentration"),
  comment.en = c("filtration rate of non-selective zooplankton as a function of food concentration")
)

### 4. Exponentielle Sättigung ###

expsaturation <- list(
  id = "expsaturation",
  name.de = "Exponentielle Sättigungsfunktion",
  name.en = "Exponential saturation function",
  formula = expression(paste(y == a ~~ (1 - e^{b * x}))),
  formula.pc = "y = a * (1 - exp(b * x))",
  nVariants = 3,
  calc = function(x, i) {a[i] * (1 - exp(b[i] * x))},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i])},
  a    = c(8, 8, 2),
  b    = c(-0.5, -2, -0.5),
  comment.de = c("Weniger gebräuchliche Alternative zur Monod-Hyperbel",
		  "Photosyntheserate als Funktion des Lichtangebotes", 
                  "Wachstumsrate als Funktion der Konzentration des limitierenden Nährstoffes",
                  "Ingestionsrate von Zooplankton als Funktion der Futterkonzentration",
                  "Mortalitätsrate als Funktion der Populationsdichte"),
  comment.en = c("photosynthesis rate as a function of light supply",
                  "growth rate as a function of the concentration of the limiting nutrient",
                  "ingestion rate of zooplankton as a function of food concentration",
                  "mortality rate  as a function of population density")
)

### 5. Sigmoide ###

sigmoid <- list(
  id = "sigmoid",
  name.de = "Sigmoide",
  name.en = "Sigmoid curve",
  formula = expression(paste(y == frac(a, {1 + b * x^c}))),
  formula.pc = "y = a * (1 + b * x^c)^-1",
  nVariants = 4,
  calc = function(x, i) {a[i] / (1 + b[i] * (x^c[i]))},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i])},
  a    = c(8, 8, 8, 5),
  b    = c(1, 1, 5, 2),
  c    = c(-5, -3, -3, 2),
  comment.de = c("Beschreibung von Prozessen, die zunächst mit einer exponentiellen Charakteristik beginnen und danach eine Sättigung erreichen",
		 "Organismenzahl als Funktion der Zeit, limitiertes Populationswachstum",
		  "Phytoplanktonbiomasse als Funktion der Konzentration des limitierenden N&auml;hrstoffes",
                  "in neuronalen Netzwerken",
                  "4. Graph (c=2) ist entartet"),
  comment.en = c("phytoplankton biomass as a function of the concentration of the limiting nutrient",
                  "number of organisms as a function of time; limited population growth",
                  "4th graph (c=2) is degenerated")
)

### 6. Exponentielle Sigmoide ###

sigmoidexp <- list(
  id = "sigmoidexp",
  name.de = "Exponentielle Sigmoide und logistische Funktion",
  name.en = "Exponential sigmoid curve and logistic function",
  formula = expression(paste(y == frac(a, {1 + b ~~ e^{c*x}}))),
  formula.pc = "y = a * (1 + b * exp(c * x))^-1",
  nVariants = 4,
  calc = function(x, i) {a[i] / (1 + b[i] * (exp(c[i]*x)))},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i])},
  a    = c(8, 8, 8, 2),
  b    = c(10, 10, 50, 2),
  c    = c(-4, -1, -2, -2),
  comment.de = c("Phytoplanktonbiomasse als Funktion des limitierenden N&auml;hrstoffes",
                  "Organismenzahl als Funktion der Zeit, limitiertes Populationswachstum",
                  "Sonderfall logistische Funktion: a = K (Kapazit&auml;t), b = K/X0-1 (X0 = Startbiomasse), c = -µ (negative Wachstumsrate)"),
  comment.en = c("phytoplankton biomass as a function of the concentration of the limiting nutrient",
                  "number of organisms as a function of time; limited population growth",
                  "special case logistic function: a = K (capacity), b = K/X0-1 (X0 = start biomass), c = -µ (negative growth rate)")
)              

logist2 <- list(
  id = "logist2",
  name.de = "Erweiterte logistische Funktion",
  name.en = "Extended logistic function",
  formula = expression(paste(y == a + frac(K, {1 + T ~~ e^{-mu*(x-M)^{1/T}}}))),
  formula.pc = "y = a + K * (1 + T * exp(-µ * (x - M)^(T^-1)))^-1",
  nVariants = 4,
  calc = function(x, i) {a[i] + K[i] / (1 + T[i] * exp(-mu[i]*(x - M[i]))^(1/T[i]))},
  leg  = function(i)  {paste("a =", a[i], ",  K =", K[i], ",  µ =", mu[i], ",  M =", M[i], ",  T =", T[i])},
  a    = c(0, 1, 1, 1),
  K    = c(1, 2, 3, 4),
  mu    = c(1, 2, 3, 4),
  M    = c(1, 2, 3, 4),
  T    = c(1, 1, 1, 4),
  comment.de = c("sigmoides Wachstum, siehe auch logistische Funktion",
                  "sehr flexibel anpassbar!"),
  comment.en = c("sigmoid growth, se also logistic function",
                  "very flexible!")
)

### 7. Gompertz-Funktion###
gompertz <- list(
  id = "gompertz",
  name.de = "Gompertz-Funktion",
  name.en = "Gompertz-Makeham's growth function",
  formula = expression(paste(y == a + c ~~ e^{-e^{-b*(x-d)}})),
  formula.pc = "y = a + c * exp(-exp(-b * (x-d)))",
  nVariants = 4,
  calc = function(x, i) {a[i] + c[i] * exp(-exp(-b[i] * (x-d[i])))},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i], ",  d =", d[i])},
  a    = c(0, 0, 0, .2),
  b    = c(1, 1, 1, 6),
  c    = c(1, 2, 2, 2),
  d    = c(2, 2, 1, 1),
  comment.de = c("somatisches Wachstum von Makroorganismen, Biomasse als Funktion der Zeit"),
  comment.en = c("limited growth of macro organisms, biomass as a function of time")
)


### 8. von Bertalanffy-Gleichung###
berta <- list(
  id = "berta",
  name.de = "von Bertalanffy-Funktion",
  name.en = "von Bertalanffy-equation",
  formula = expression(paste(y == (a^b - (a^b - c) * e^{-bd(x-x[0])})^{1/b})),
  formula.pc = "y = (a^b - (a^b - c) * exp(-b * d * (x - xo)))^(b^-1)",
  nVariants = 2,
  calc = function(x, i) {(a[i]^b[i] - (a[i]^b[i] - c[i]) * exp(-b[i]*d[i]*x))^(1/b[i])},
  leg  = function(i)  {paste("a =",a[i],", b =",b[i],", c =",c[i]," ,d =",d[i] )},
  a       = c(7,7),
  b 	  = c(0.07,0.14),
  c       = c(0.01,0.01),
  d	  = c(10,10),
  comment.de = c("somatische Wachstumskurven"),
  comment.en = c("  ")
)


### 9. Inverse ###

invers <- list(
  id = "invers",
  name.de = "Inverse",
  name.en = "Inverse function",
  formula = expression(paste(y == frac(a, {b+x}))),
  formula.pc = "y = a * (b + x)^-1",
  nVariants = 5,
  calc = function(x, i) {a[i] / (b[i] + x)},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i])},
  a    = c(2, 1, 1, 1, -1),
  b    = c(0, 1, 0, -1, -5),
  comment.de = c("Einfache Möglichkeit zur Beschreibung umgekehrt proportionaler Abhängigkeiten",
		 "Ei-Entwicklungsdauer poikilothermer Tiere als Funktion der Temperatur"),
  comment.en = c ("duration of egg development in poikilothermic animals as a function of temperature")
 )

hypinvers <- list(
  id = "hypinvers",
  name.de = "Inverse Hyperbel",
  name.en = "Inverse hyperbola",
  formula = expression(paste(y == a ~~ frac(1/x, {1/b + 1/x}))),
##formula = expression(paste(y == a ~~ frac(frac(1,x), {frac(1,b)+frac(1,x)}))),##
  formula.pc = "y = a * x^-1 * (b^-1 + x^-1)",
  nVariants = 2,
  calc = function(x, i) {a[i] * 1/x / (1/b[i] + 1/x)},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i])},
  a    = c(8, 8),
  b    = c(2, 1),
  comment.de = c("&Auml;ußerst wichtige Funktion!",
                  "Ingestionsrate von Zooplankton als Funktion der Zooplanktondichte",
                  "Wachstumsrate als Funktion der Populationsdichte",
                  "Yield-Koeffizient nichtselektiver Filtrierer als Funktion der Futterkonzentration ",
                  "Phosphorfreisetzung aus dem Sediment als Funktion der Sauerstoffkonzentration",
		  "Ingestionsrate von Zooplankton als Funktion der Zooplanktondichte"),
  comment.en = c("very important function!",
                  "ingestion rate of zooplankton as a function of zooplankton density",
                  "growth rate as a function of population density",
                  "yield coefficient of non- selective filtrators as a function of food concentration",
                  "phosphorus release from the sediment as a function of oxygen concentration")
)

### 10. Maximafunktion ###

maxima <- list(
  id = "maxima",
  name.de = "Maximafunktion",
  name.en = "Maximum function",
  formula = expression(paste(y == a ~~ x ~~ e^{b*x})),
  formula.pc = "y = a * exp(b * x)",
  nVariants = 4,
  calc = function(x, i) {a[i] * x * exp(b[i] * x)},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i])},
  a    = c(10, 10, 20, 20),
  b    = c(-0.5, -1, -1, -2),
  comment.de = c("Einfache Möglichkeit zur Beschreibung von Beziehungen mit Optimum",
		  "Wachstumsrate als Funktion der Substratmenge; Substrathemmung",
                  "Wachstumsrate als Funktion des Lichtes; Lichthemmung"),
  comment.en = c("growth rate as a function of substrate supply; substrate inhibition",
                  "growth rate as a function of light supply; light inhibition") 
)

### 11. Andere Maximafunktionen ###

maximaln <- list(
  id = "maximaln",
  name.de = "Maximafunktion II (nat&uuml;rlicher Logarithmus)",
  name.en = "Maximum function II (natural logarithm)",
  formula = expression(paste(y == e^{a - b ~~ log(x) - c * (log(x))^2}, "   (log = ln)")),
  formula.pc = "y = exp(a - b * ln(x) - c * (ln(x))^2)",
  nVariants = 4,
  calc = function(x, i) {exp(a[i] - b[i] * log(x) - c[i] * (log(x))^2)},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], "  c =", c[i])},
  a    = c(2, 2, 1, 1),
  b    = c(0.2, 0.1, 0.1, 0.1),
  c    = c(0.2, 0.2, 0.2, 0.5),
  comment.de = c("Beachte: Es wird nur der rechte, fallende Ast der Kurve verwendet!",
		 "Ei-Entwicklungsdauer poikilothermer Tiere als Funktion der Temperatur"),
  comment.en = c("duration of egg development in poikilothermic animals as a function of temperature",
                  "note: only the right, declining branch of the curve is used!")
)

maximaabs <- list(
  id = "maximaabs",
  name.de = "Maximafunktion III (Betrag)",
  name.en = "Maximum function III (absolute)",
  formula = expression(paste(y == a ~~ e^{-c * group("|",log(x/b),"|")}, "   (log = ln)")),
  formula.pc = "y = a * exp(-c * ABS(ln(x * b^-1))) ",
  nVariants = 3,
  calc = function(x, i) {a[i] * exp(-c[i] * abs(log(x / b[i])))},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i])},
  a    = c(8, 8, 4),
  b    = c(5, 5, 2),
  c    = c(1, 2, 0.5),
  comment.de = c("selten benutzte Funktion",
		 "Ingestionsrate von Daphnien als Funktion der Temperatur"),
  comment.en = c("ingestion rate as a function of temperature")
)

### 12. Optimumfunktion ###
optimum <- list(
  id = "optimum",
  name.de = "Optimumfunktion",
  name.en = "Optimum function",
  formula = expression(paste(y == d ~~ e^{a * x - b * x^{c * x}})),
  formula.pc = "y = d * exp(a * x - b * x^(c - x))",
  nVariants = 3,
  calc = function(x, i) {d[i] * exp(a[i] * x) - (b[i] * x^(c[i] * x))},
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i], ",  d =",  d[i])},
  a    = c(0.4, 0.3, 0.4),
  b    = c(0.1, 0.4, 1e-5),
  c    = c(0.5, 0.5, 2),
  d    = c(2, 2, 2),
  comment.de = c("Wachstumsrate als Funktion der Temperatur"),
  comment.en = c("growth rate as a function of temperature") 
)

###13. Gaußsche Normalverteilung (Glockenkurve)###

gauss <- list(
  id = "gauss",
  name.de = "Gaußsche Normalverteilung (Glockenkurve)",
  name.en = "Gauss probability distribution (bell-shaped optimum curve)",
  formula = expression(paste(y == frac(1, (sigma * sqrt({2*pi}))) ~~ e^- frac({(x-mu)^2}, {2 * sigma^2}))),
  formula.pc = "y = (sigma * sqrt(2 * pi))^-1 * exp(-(x - µ)^2 * (2 * sigma^2)^-1)",
  nVariants = 3,
  calc = function(x, i) {1 / (si[i] * sqrt(2*pi)) * exp(-((x - mu[i])^2) / (2 * si[i]^2))},
  leg  = function(i)  {paste("µ =" ,mu[i], ",  sigma =", si[i])},
  mu    = c(3, 5, 5 ),
  si    = c(1, 2, 1),
  comment.de = c("statistische Normalverteilung von Meßwerten",
                  "µ = Mittelwert der Grundgesamtheit",
                  "sigma = Standardabweichung des Mittelwertes",
                  "beachte: in dieser Form betr&auml;gt das Integral der Funktion (Fl&auml;che unter der Kurve) stets Eins!",
                  "kann auch als Optimumkurve usw. verwendet werden, dann oft Multiplikation mit einem Faktor"),
  comment.en = c("statistical normal distribution of data",
                  "µ = mean of the population",
                  "sigma = standard deviation of the mean",
                  "note: in this formula, the integral of the function (area beneath the curve) is 1!",
                  "may also be used as an optimum curve, then often multiplication by some factor")
)

### 14. Polynome ###

polynom <- list(
  id = "polynom",
  name.de = "Polynom",
  name.en = "Polynomial function",
  formula = expression(paste(y == a[0] + a[1] * x + a[2] * x^2 + a[3] * x^3 + a[4] * x^4, "...")),
  formula.pc = "y = a0 + a1 * x + a2 * x^2 + a3 * x^3 + a4 * x^4 ...",
  nVariants = 2,
  calc = function(x, i) {a[i] + b[i]*x + c[i]*x^2 + d[i]*x^3 + e[i]*x^4},
  leg  = function(i)  {paste("a[0] =", a[i], ",  a[1] =", b[i], ",  a[2] =", c[i], ",  a[3] =",  d[i], ",  a[4] =", e[i])},
  a    = c(40, 5),
  b    = c(-40.3, 2),
  c    = c(15.33, 0.1),
  d    = c(-2.1, 0.02),
  e    = c(0.1, 0.002),
  comment.de = c("sehr vielgestaltige Graphen m&ouml;glich, Ordnung kann in der Praxis meist bis auf etwa 8 erh&ouml;ht werden",
                 "bei hohen Ordnungen sind nuerische Probleme m&ouml;glich",
                  "Stauinhalts-Kurven für Gew&auml;sser, Volumen als Funktion der H&ouml;he &uuml;ber NN",
                  "Dichte des Wassers als Funktion der Temperatur"),
  comment.en = c("many different graphs possible; order can be increased up to approx. 8",
                  "high orders may lead into numerical problems",
                  "volume of water bodies as a function of altitude",
                  "density of water as a function of temperature") 
)

quadratic <- list(
  id =      "quadratic",
  name.de = "Quadratische Funktion (Sonderfall des Polynoms)",
  name.en = "Square function (special case of polynomial function)",
  formula = expression(paste(y == a[0] + a[1] * x + a[2] * x^2 )),
  formula.pc = "y = a0 + a1 * x + a2 * x^2",
  nVariants = 2,
  calc = function(x, i) {a[i] + b[i]*x + c[i]*x^2 },
  leg  = function(i)  {paste("a =", a[i], ",  b =", b[i], ",  c =", c[i])},
  a    = c(0, 10),
  b    = c(2, 2),
  c    = c(2, 0.1),
  comment.de = c("Phytoplankton-Kompensationspunkt für Licht als Funktion des Angebot an freiem CO2"),
  comment.en = c("light compensation point in phytoplankton as a function of the supply of free carbon dioxide") 
)

harmonic <- list(
  id = "harmonic",
  name.de = "Harmonische Funktionen",
  name.en = "Harmonic functions",
  formula = expression(paste(y == a[0] + a[1]* sin(x/T[1] + b[1]) + a[2] * sin({2*x/T[2] + b[2]}), "...")),
  formula.pc = "y = a0 + a1 * sin(x * T1^-1 + b1) + a2 * sin(2*x * T2^-1 + b2)",
  nVariants = 2,
  calc = function(x, i) {a0[i] + a1[i] * sin(x/T1[i] + b1[i]) + a2[i] * sin(2 * x/T2[i] + b2[i])},
  leg  = function(i)  {paste("a0 =" ,a0[i], ", a1 =" ,a1[i], ",  b1 =", b1[i], ", T1 =", T1[i], ", a2 =" ,a2[i], ", b2 =",  b2[i], ", T2 =", T2[i])},
  a0    = c(2, 1),
  a1    = c(1, 1),
  b1    = c(2, -2),
  T1    = c(1, 3),
  a2    = c(1, 0),
  b2    = c(1, 0),
  T2    = c(1, 1),
  comment.de = c("beliebige zeitperiodische Vorg&auml;nge:",
                  "Temperatur als Funktion der Zeit (Jahreszyklus)", 
                  "Licht als Funktion der Zeit (Jahreszyklus)",
                  "Vertikalwanderung des Zooplanktons als Funktion von Zeit oder Licht (Tageszyklus)"),

  comment.en = c("any periodical process, e.g.:",
                  "temperature as a function of time",
                  "light as a function of time",
                  "diel vertical migration of zooplankton as a function of time or light")
)

step <- list(
  id = "step",
  name.de = "Schwellen-Kurve",
  name.en = "Step function",
  formula = expression(paste(y == frac(1-b-a[min],1+(frac(x,xc))^z)+a[min])),
  formula.pc = "y = (1 - b - amin) * (1 + (x * xc^-1)^z)^-1 + amin",
  nVariants = 3,
  calc = function(x, i) {(1-b[i]-amin[i])/(1+(x/xc[i])^z[i])+amin[i]},
  leg  = function(i)  {paste("amin =", amin[i], ",  b =", b[i], ",  xc =", xc[i], ",  z =",  z[i])},
  amin = c(0.1,0.1,0.1),
  b    = c(0.2,0.2,0.2),
  xc    = c(250,250,250),
  z    = c(60,15,7),
  comment.de = c("Steilheit der Reaktion von Populationen auf &Auml;nderungen der Dichte von Frassfeinden",
               "<b>Beispiel:</b> Vertikalmigration von Daphnien (tags&uuml;ber in die Tiefe): Anteil NICHT wandernder Daphnien als Funktion der Fischdichte",
               "amin = Anteil der auch bei maximaler Fischdichte nicht nach unten wandernden Daphnien",
               "b = Anteil der immer unten bleibenden Daphnien",
               "xc = kritische Fischdichte f&uuml;r Induktion der Vertikalwanderung",
               "z = Steilheit der Reaktion",
               "N&auml;heres zu diesem Beispiel siehe <b>Ramos-Jiliberto, R. &amp; Gonzalez-Olivares, E. (2000)</b>: Relating behavior to population dynamics: a predator-prey metaphysiological model emphasizing zooplakton diel vertical migration as an inducible response. Ecological modelling 127:221-233.",
               "Allgemeinere Form siehe <b>Getz, W. M. (1996)</b>: A hypothesis regarding the abruptness of density dependence and the groth rate of populations. Ecology 77: 2014-2026."),
  comment.en = c("abruptness of the response of populations to changes in predator density",
               "<b>example:</b> diel vertical migration (DVM) in Daphnia (down during daylight): proportion of non-migrating animals as a function of fish density",
               "amin = proportion of animals not migrating despite of maximal fish density",
               "b = proportion of animals staying down all the time",
               "xc = critical fish density for the inductio of DVM",
               "z = abruptness of the response",
               "for more details see <b>Ramos-Jiliberto, R. &amp; Gonzalez-Olivares, E. (2000)</b>: Relating behavior to population dynamics: a predator-prey metaphysiological model emphasizing zooplakton diel vertical migration as an inducible response. Ecological modelling 127:221-233.",
               "and <b>Getz, W. M. (1996)</b>: A hypothesis regarding the abruptness of density dependence and the groth rate of populations. Ecology 77: 2014-2026."),
)



########################################
# Hauptprogramm ########################
########################################

htmlfile <- "funclib.html"
menufile <- "menu.html"

# HTML- Header schreiben
write(file = htmlfile, header)


write(file = menufile, header)
write(file = menufile, "<h3>Funktionstypenbibliothek</h3>", append=TRUE)

write(file = menufile, "<ul>", append=TRUE)



plotit(expgrow, seq(0,10, length=500), miny=0)

plotit(expdecay, seq(0,10, length=500), miny=0, maxy=10)

plotit(expdecay2, seq(0,10, length=500))

plotit(power, seq(0, 10, length=500))

plotit(power2, seq(0, 10, length=500))

plotit(root, seq(0, 10, length=500))

plotit(powerinv, seq(0, 10, length=500), miny=0, maxy=1500)

plotit(monod, seq(0, 10, length=500),miny=0, maxy=12)

plotit(monodinhib, seq(0, 10, length=500), maxy=2)

plotit(monodinv, seq(0, 10, length=500), miny=0, maxy=12)

plotit(expsaturation, seq(0, 10, length=500), miny=0, maxy=12)

plotit(sigmoid, seq(0, 10, length=500), miny=0, maxy=12)

plotit(sigmoidexp, seq(-2, 10, length=500), miny=0, maxy=12)

plotit(logist2, seq(-2, 10, length=500), miny=0, maxy=6)

plotit(gompertz, seq(-2, 10, length=500), miny=0, maxy=4)

plotit(berta, seq(0, 10, length=500), miny=0, maxy=10)

plotit(invers, seq(0, 10, length=500), miny=-1, maxy=5, legpos=c(6.5, 5))

plotit(hypinvers, seq(0, 10, length=500),miny=0,maxy=10)

plotit(maxima, seq(0, 10, length=500), miny=0, maxy=12)

plotit(maximaln, seq(0, 10, length=500), miny=0, maxy=12)

plotit(maximaabs, seq(0, 10, length=500), maxy=12)

plotit(optimum, seq(0, 10, length=500), miny=0, maxy=14)

plotit(gauss, seq(0, 10, length=500), miny=0, maxy=1)

plotit(polynom, seq(0, 10, length=500), miny=0, maxy=80)

plotit(quadratic, seq(0, 10, length=500), miny=0)

plotit(harmonic, seq(0, 20, length=500), miny=0, maxy=5)

plotit(step, seq(0, 500, length=500), miny=0, maxy=1.09)


write(file = menufile, "<hr>", append = TRUE)
if (lang=="de") {
write(file = menufile, "<p><a href=\"funclib.html\" target=\"TOP\">ohne Frames</a></p>", append = TRUE)
write(file = htmlfile, "<p><a href=\"func_frame.html\" target=\"TOP\">mit Frames</a></p>", append = TRUE)
} else {
write(file = menufile, "<p><a href=\"funclib.html\" target=\"TOP\">without frames</a></p>", append = TRUE)
write(file = htmlfile, "<p><a href=\"func_frame.html\" target=\"TOP\">with frames</a></p>", append = TRUE)
}
write(file = htmlfile, footer, append = TRUE)
write(file = menufile, "</ul>", append = TRUE)
write(file = menufile, footer, append = TRUE)





