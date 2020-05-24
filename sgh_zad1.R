# http://michal.ramsza.org/lectures/2_r_programming/proj1/project_1.html?fbclid=IwAR0jp8_LMkcMEncwVigsPAq47b4vko-J2Mzas4mLJS5DtfsL3w0ZZB4XPgo

setwd("C:/Users/Asia/Documents/zad_R")

f = "graph_medium.dat"
h = c("delete","node", "connections")
graph = read.delim(file = f, header = FALSE, sep = "{", col.names = h, colClasses = NA, stringsAsFactors=FALSE)

drops <- c("delete")
graph = graph[ , !(names(graph) %in% drops)]

graph
#graph$node = as.numeric(gsub("[^[:digit:]]","", graph$node ))
graph$node = strsplit(graph$node, ", ")
graph$connections[] = gsub("[^[:digit:]+,]","", graph$connections[] )
graph$connections = strsplit(graph$connections, ",") # tu powstaje list of 10

#character to number
graph$node = lapply(graph$node, as.numeric)
graph$connections = lapply(graph$connections, as.numeric)


#### lista z wskaznikami na listy
P=0
for(i in 1:length(graph$node)){
  P[i] = list(c(node = graph$node[i], connections = graph$connections[i]))
  i = i + 1
}

############################# check
gMedium = P
info <- data.frame( wierzcholek = 1:length(gMedium),
                    "liczba_powiazan" = unlist( lapply( X = gMedium,
                                                        FUN = function( s){
                                                          length(s$connections)
                                                        }
                    )))

head( info)
############################## CHECK
png( file = "./fig1.png")
plot( info, type = "h", lwd = 4, col = "orange", axes = FALSE)
axis( side = 1, at = c( 1, seq(10, 100, 10)))
axis( side = 2, at = sort( unique( info$liczba_powiazan)))
dev.off()


