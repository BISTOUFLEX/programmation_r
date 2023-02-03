##EXERCICE 1 :

install.packages("readxl")

# Afficher le numéro de lignes de la Dataframe Pokemon
l <- nrow(pokemon) 
cat("le nombres de lignes de la Dataframe Pokemon est", l, "\n")

# Afficher le numéro de colonnes de la Dataframe Pokemon
c <- ncol(pokemon) 
cat("le nombres de colonnes de la Dataframe Pokemon est", c, "\n")

#Affichez le nom des colonne
n <- colnames(pokemon)
cat("le nom des colonnes de la Dataframe Pokemon est", n, "\n")


# Affichez le type des colonnes
sapply(pokemon, class)

# Convertir les colonnes en variables qualitatives (factor)
pokemon$generation <- as.factor(pokemon$generation)
pokemon$is_legendary <- as.factor(pokemon$is_legendary)
pokemon$type <- as.factor(pokemon$type)

# Déterminer le nombre de niveaux dans chaque colonne factor et les afficher
n_generation <- nlevels(pokemon$generation)
n_is_legendary <- nlevels(pokemon$is_legendary)
n_type <- nlevels(pokemon$type)

cat("Le nombre de niveaux dans la colonne generation est", n_generation, "\n")
cat("Le nombre de niveaux dans la colonne is_legendary est", n_is_legendary, "\n")
cat("Le nombre de niveaux dans la colonne type est", n_type, "\n")


# Afficher un résumé des données
summary(pokemon)

##EXERCICE 2 :

##Question A :

# Calcul de la moyenne de weight_kg
mean(pokemon$weight_kg,na.rm = T)

##Question B :

# Calcul de la médiane de weight_kg
median(pokemon$weight_kg,na.rm = T)

##Question C :

# Calcul des quartiles de height_m
quantile(pokemon$height_m,na.rm = T, c(0.25, 0.5, 0.75))

##Question D :

# Calcul des déciles de height_m
quantile(pokemon$height_m,na.rm = T ,seq(0, 1, by = 0.1))

##Question E :

# Calcul de la variance de weight_kg
var(pokemon$weight_kg,na.rm = T)

# Calcul de l'écart-type de weight_kg
sd(pokemon$weight_kg,na.rm = T)

##Question F :

# Compter les effectifs des modalités de la variable generation
gen_table <- table(pokemon$generation)

# Trier la table par effectifs décroissants
gen_table_sorted <- sort(gen_table, decreasing = TRUE)
gen_table_sorted

# Compter les effectifs des modalités de la variable is_legendary
legend_table <- table(pokemon$is_legendary)

# Trier la table par effectifs décroissants
legend_table_sorted <- sort(legend_table, decreasing = TRUE)
legend_table_sorted

# Compter les effectifs des modalités de la variable type
type_table <- table(pokemon$type)
  
# Trier la table par effectifs décroissants
type_table_sorted <- sort(type_table, decreasing = TRUE)
type_table_sorted

##EXERCICE 3 :


