library(igraph)
library(readxl)
library(networkD3)
library(htmlwidgets)

nwdata <- read_xlsx("nettverksdata.xlsx")

#Lag en gruppe basert på kolonne 3 i dataene
make.group <- function(x) {
  group <- subset(nwdata, nwdata[,3] == x)
  group <- unique(unlist(group[,1]))
  return(group)
}

#networkD3
d3 <- data.frame(from = nwdata[,1], to = nwdata[,2])

#Enkelt plot
nwplot <- simpleNetwork(d3, height="200px", width="200px",        
                   Source = 1,                 # kolonnenummer til source
                   Target = 2,                 # kolonnenummer til target
                   linkDistance = 10,          # distanse mellom noder. Øk verdien for å lage mer rom mellom nodene
                   charge = -900,              # numerisk verdi som indikerer enten nodenes frastøtning (negativ verdi) eller tiltrekning (positiv verdi)
                   fontSize = 14,              # størrelse på nodenavnene
                   fontFamily = "serif",       # font til nodenavn
                   linkColour = "#666",        # farge på kanter, må være felles for hele grafen
                   nodeColour = "#69b3a2",     # farge på noder, være felles for hele grafen
                   opacity = 0.9,              # Nodens transparensgrad. 0 = gjennomsiktig, 1 = ingen transparens opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Ja eller nei til zoome-mulighet
)

saveWidget(nwplot, file="nwplot_enkel.html")

#Avansert - med fargekoding på grupper

fra <- data.frame(nwdata[,1])
til <- data.frame(nwdata[,2])
fra <- as.character(unlist(fra))
til <- as.character(unlist(til))

nodefactors <- c(fra, til)
nodefactors <- as.factor(sort(unique(nodefactors)))

noder <- data.frame(name=nodefactors, group=1, size=15)

#Grupper i datasettet kan defineres som en vil. I dette prosjektet het de
#Kontakter (standardgruppe), Investorer, Hongkong, Nær familie, Sverige.
inv <- make.group("inv")
hk <- make.group("hk")
familie <- make.group("fam")
sverige <- make.group("Sverige")
inv.color <- match(inv, levels(nodefactors))
hk.color <- match(hk, levels(nodefactors))
familie.color <- match(familie, levels(nodefactors))
sverige.color <- match(sverige, levels(nodefactors))
noder[,2] <- "Kontakter"
noder[inv.color,2] <- "Investorer"
noder[hk.color,2] <- "Hongkong"
noder[familie.color,2] <- "Nær familie"
noder[sverige.color,2] <- "Sverige"

source <- match(fra, levels(nodefactors)) - 1
target <- match(til, levels(nodefactors)) - 1

links <- data.frame(Source = source, Target = target, Value = 3)

nwplot2 <- forceNetwork(Links = links, 
                  Nodes = noder,
                  Source= "Source", 
                  Target = "Target",
                  Group = "group",
                  NodeID = "name",
                  charge = -600, 
                  linkDistance = 100,
                  zoom = T, 
                  opacity = 0.9,
                  fontSize=14, 
                  opacityNoHover = 1,
                  legend = TRUE)

saveWidget(nwplot2, file="nwplot2.html")
