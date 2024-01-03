## P1

a: Sí
b: Sí
c: Sí
d: NO

## P2

a

## P3

Figura B: menos tolerancia a error de clasificacion

## P4

e

## P5

c: lift

## P6

a
d

## P7

e (kmean and dbscan do at least)

## P8

a

## P9
data <- read.csv("gini.csv", sep = " ")
data[, 1] <- as.factor(data[, 1])
data[, 2] <- as.factor(data[, 2])
data[, 3] <- as.factor(data[, 3])
data[, 4] <- as.factor(data[, 4])
data

# Genero
table(data[, 1])
table(data[, 1], data[, 4])

genero_m <- 1 - ((6/10)**2 + (4/10)**2) 
genero_f <- 1 - ((4/10)**2 + (6/10)**2) 
gini_genero <- 0.5*genero_m + 0.5*genero_f
gini_genero  # 0.480

# Tipo de Auto
table(data[, 2])
table(data[, 2], data[, 4])

auto_depo <- 1 - ((8/8)**2 + (0/8)**2)
auto_fami <- 1 - ((1/4)**2 + (3/4)**2)
auto_lujo <- 1 - ((1/8)**2 + (7/8)**2)

gini_auto <- (8/20)*auto_depo + (4/20)*auto_fami + (8/20)*auto_lujo
gini_auto  # 0.1625  (0.163)

# Talla de camisa
table(data[, 3])
table(data[, 3], data[, 4])

camisa_extra <- 1 - ((2/4)**2 + (2/4)**2)
camisa_larg <- 1 - ((2/4)**2 + (2/4)**2)
camisa_medi <- 1 - ((3/7)**2 + (4/7)**2)
camisa_smal <- 1 - ((3/5)**2 + (2/5)**2)

gini_camisa <- (4/20)*camisa_extra + (4/20)*camisa_larg + (7/20)*camisa_medi + (5/20)*camisa_smal
gini_camisa  # 0.4914286 (0.491)

# id
data$id <- 1:nrow(data)
data

table(data[, 5])
table(data[, 5], data[, 4])

obs <- 1 - ((1/1)**2 + (0/1)**2)
gini_id <- 0  # 0.000

## Menor gini no nulo: Tipo de Auto


## P10
library(tree)

data <- read.csv("gini.csv", sep = " ")
# data[21,] <- c("M", "Lujo", "Large", "C1")

data[, 1] <- as.factor(data[, 1])
data[, 2] <- as.factor(data[, 2])
data[, 3] <- as.factor(data[, 3])
data[, 4] <- as.factor(data[, 4])

data

model <- tree(clase ~ ., data)
model
plot(model)
text(model, pretty = 0)

# After new artificail row, de C1 hay 0.5238

library(dplyr)

data |>
  filter(tipo_de_auto == "Deportivo")

# Answer:
# 345, 42.03
# V5
# 0.32, 1

## P11

# De https://stats.stackexchange.com/questions/162465/in-a-random-forest-is-larger-incmse-better-or-worse
# RPTA: a 

## P12

# a)
2/8  # 0.250
2/4  # 0.500
1    # 1.000

## P13
library(arules)

datos <- read.transactions(
  file = "./bookdata.tsv.gz", 
  format = "single", sep = "\t", header = TRUE,
  cols = c("userid", "title"), rm.duplicates = TRUE
)

summary(datos)

inspect(datos[1:5])
summary(datos[1:5])

# Total de ids (duplicates ya fueron removed)
numids <- 51286+  10804+  5760+  3850+  2700+  2044+  1609+  1241+  1075+   901+   755+   643+   555+   460+   464+   393+   342+   332+   268+   258+   237+   222+   195+   179+   182+   170+   156+     
  154+   129+   114+   120+   103+   128+   101+    88+    94+    98+    82+    71+    80+    60+    81+    70+    65+    73+    77+    79+    54+    54+    47+    43+    50+    52+    43+ 
  38+    47+    43+    39+    46+    44+    36+    30+    31+    24+    46+    25+    27+    35+    29+    28+    32+    22+    26+    24+    25+    19+    22+    26+    27+    21+    29+ 
  13+    29+    21+    20+    13+    15+    10+    21+    26+    14+    16+    23+    13+    15+    19+    14+    20+    21+    12+    12+    14+    17+    16+    15+    12+    13+    18+ 
  11+    11+    15+    16+    14+    10+    12+    14+     5+     9+    17+     5+    10+    11+    11+     9+    17+    16+    14+    12+     6+    10+    10+     8+     8+     9+    10+ 
  11+     8+     5+     7+    12+     9+     6+     6+     7+     7+     9+     7+     6+     6+     5+     8+    10+     7+    11+     6+     9+     7+     3+     8+     7+     8+     6+ 
  7+     9+     4+     9+     6+     6+     1+    10+     6+     6+     6+     2+     7+     8+     3+     9+     5+     5+     4+     9+     6+     8+     3+     2+     9+     5+     5+ 
  6+     7+     4+     7+     1+     6+     8+     4+     2+     7+     4+     9+     6+     4+     2+     4+     4+     4+    10+     2+     4+     1+     3+     3+     4+     3+     5+ 
  1+     5+     5+     8+     2+     4+     5+     3+     7+     3+     2+     2+     6+     6+     5+     3+     8+     5+     5+     6+     4+     6+     2+     4+     5+     5+     1+ 
  2+     1+     1+     4+     1+     2+     1+     4+     2+     2+     2+     3+     1+     3+     4+     2+     4+     3+     4+     1+     1+     2+     4+     5+     2+     5+     4+     
  3+     6+     4+     1+     1+     2+     1+     3+     5+     2+     2+     2+     3+     2+     4+     1+     1+     4+     1+     1+     2+     1+     4+     1+     3+     1+     1+ 
  1+     2+     1+     3+     4+     2+     1+     2+     3+     1+     2+     3+     3+     1+     1+     3+     4+     1+     2+     2+     1+     1+     6+     1+     2+     2+     2+     
  2+     2+     1+     1+     1+     1+     1+     2+     2+     3+     2+     2+     1+     1+     1+     2+     1+     1+     1+     1+     1+     1+     1+     1+     3+     1+     1+ 
  2+     1+     2+     1+     2+     1+     2+     2+     1+     1+     2+     2+     1+     4+     2+     2+     1+     3+     1+     1+     2+     4+     1+     2+     1+     2+     1+ 
  4+     1+     1+     3+     1+     1+     3+     1+     1+     1+     3+     1+     4+     1+     1+     2+     2+     1+     1+     1+     1+     3+     1+     1+     2+     2+     4+ 
  1+     1+     1+     1+     1+     2+     1+     1+     2+     2+     1+     1+     2+     1+     2+     1+     2+     1+     1+     1+     1+     2+     4+     1+     1+     1+     1+ 
  1+     2+     1+     1+     1+     1+     1+     1+     1+     1+     2+     2+     1+     2+     1+     1+     1+     2+     2+     1+     1+     1+     2+     1+     1+     1+     1+ 
  2+     1+     1+     1+     1+     2+     1+     2+     1+     2+     2+     2+     1+     1+     1+     1+     3+     2+     1+     2+     1+     1+     1+     1+     2+     1+     1+ 
  1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     2+     1+     1+ 
  2+     1+     2+     2+     1+     1+     1+     1+     1+     2+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+ 
  1+     2+     1+     1+     1+     1+     1+     1+     1+     1+     1+     3+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+ 
  1+     1+     1+     1+     1+     1+     2+     2+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+ 
  1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     2+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+ 
  1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+ 
  1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1+     1


# Total de transaction ids 
numids  # 92108
# Total de libros
length(unique(unlist(as(datos, "list"))))  # 220447


summary(datos)

# A lo menos
1  # 51286 clientes

# A lo más
10253  # 1 cliente

sizes <- sort(table(size(datos)))  # 1 libro

quantile(sizes, 0.9) # A lo más 29 libros


itemFrequencyPlot(datos, topN = 20, type = 'absolute')


freq <- sort(itemFrequency(datos))
tail(freq)

length(freq[freq > 0.9 / 100])
freq[freq > 0.9 / 100]

0.027163764 * 92108  # 2502
0.014059582 * 92108  # 1295

## P14

bookbaskets_use <- apriori(
  datos, parameter = list(
    support = 0.002, confidence = 0.75, minlen = 2)
)

summary(bookbaskets_use)


lift_final <- sort(bookbaskets_use, by = 'lift', decreasing = FALSE)
inspect(lift_final)
88.9210  # menor lift (88.92)


conf_final <- sort(bookbaskets_use, by = 'confidence', decreasing = FALSE)
inspect(conf_final)
0.7518797  # menor confidence (0.75)


lift_final <- sort(bookbaskets_use, by = 'lift', decreasing = TRUE)
inspect(lift_final[1])
191.5195  # mayor lift (191,52)


soporte <- sort(bookbaskets_use, by = 'support', decreasing = TRUE)
inspect(soporte[1:1])
0.002583923  # mayor soporte (0.0026)


bookbaskets_use
inspect(bookbaskets_use)
