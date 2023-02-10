##EXERCICE 1 :

install.packages("readxl")

# Afficher le numéro de lignes de la Dataframe Pokemon
l <- nrow(pokemon) 
cat("le nombres de lignes de la Dataframe Pokemon est", l, "\n")

# Afficher le numéro de colonnes de la Dataframe Pokemon
c <- ncol(pokemon) 
cat("le nombres de colonnes de la Dataframe Pokemon est", c, "\n")

#Affichez le nom des colonnes
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

#Sélectionne la colonne nom et is_legendary :
requete_1 <- pokemon[c("nom", "is_legendary")]
dim(requete_1)
print(requete_1)

#Sélectionne les 50 premières lignes et les deux premières colonnes :
requete_2 <- pokemon[1:50, c(1,2)]
dim(requete_2)
print(requete_2,n=50)

#Sélectionne les 50 premières lignes et les deux premières colonnes :
requete_3 <- pokemon[1:10]
dim(requete_3)
requete_3
#Sélectionnez toutes les colonnes sauf la dernière :
requete_4 <- pokemon[1:ncol(pokemon)-1]
dim(requete_4)
print(requete_4,n=nrow(requete_4))


#Triez le dataset par ordre alphabétique et afficher le nom du pokemon de la première ligne :
requete_5 <- data.frame(sort(pokemon$nom, decreasing = F))
requete_5[1,]
dim(requete_5)

#Triez le dataset par weight_kg en ordre décroissant, et afficher le nom du pokemon de la première ligne :
requete_6 <- pokemon[order(-pokemon$weight_kg),]
requete_6[1,]$nom

#Triez le dataset par attack en ordre décroissant puis par speed en ordre croissant, et afficher le nom des pokemons des 10 premières lignes :
requete_7 <- pokemon[order(-pokemon$attack,pokemon$speed),]
requete_7[1:10,]$nom
dim(requete_7)

##Exercice 4 : Tris et Filtres

#a.Filtrez sur les pokemons qui ont 150 ou plus d’attack puis trier le résultat par ordre décroissant d’attack:
pokemon_filtered <- pokemon[pokemon$attack >= 150,c("nom","attack")]
Exo_2_requete_1 <- pokemon_filtered[order(-pokemon_filtered$attack),]
dim(Exo_2_requete_1)
Exo_2_requete_1

#b. Filtrez sur les pokemons de type dragon,ghost,psychic et dark:
Exo_2_requete_2 <- pokemon[pokemon$type %in% c("dragon", "ghost", "psychic", "dark"),c("nom","type")]
dim(Exo_2_requete_2)
print(Exo_2_requete_2,n=nrow(Exo_2_requete_2))

#c. Filtrez sur les pokemons de type fire avec plus de 100 d’attack, puis trier le résultat par ordre décroissant d’attack :
pokemon_fire_attack_100 <- pokemon[pokemon$type == "fire" & pokemon$attack > 100,c("nom","type","attack")]
Exo_2_requete_3 <- pokemon_fire_attack_100[order(-pokemon_fire_attack_100$attack), ]
dim(Exo_2_requete_3)
print(Exo_2_requete_3,n=nrow(Exo_2_requete_3))

#d. Filtrez sur les pokemons qui ont entre 100 et 150 de speed. Les trier par speed décroissant :
pokemon_speed_100_150 <- pokemon[pokemon$speed >= 100 & pokemon$speed <= 150,c("nom","speed") ]
Exo_2_requete_4 <- pokemon_speed_100_150[order(-pokemon_speed_100_150$speed) , ]
dim(Exo_2_requete_4)
print(Exo_2_requete_4,n=nrow(Exo_2_requete_4))

#e. Filtrez sur les pokémons qui ont des valeurs manquantes sur la variable height_m :
Exo_2_requete_5 <- pokemon[is.na(pokemon$height_m),c("nom","height_m")  ]
dim(Exo_2_requete_5)
print(Exo_2_requete_5,n=nrow(Exo_2_requete_5))

#f.Filtrez sur les pokemons qui ont des valeurs renseignées à la fois pour la variable weight_kg et la variable height.
Exo_2_requete_6 <- pokemon[!is.na(pokemon$height_m) & !is.na(pokemon$weight_kg),c("nom","height_m","weight_kg")  ]
dim(Exo_2_requete_6)
print(Exo_2_requete_6,n=nrow(Exo_2_requete_6))

#g. Filtrez sur les pokemons pesant plus de 250 kg et affichez le résultat pour vérifier.
Exo_2_requete_7 <- pokemon[pokemon$weight_kg > 250 & !is.na(pokemon$weight_kg),c("nom","weight_kg") ]
dim(Exo_2_requete_7)
print(Exo_2_requete_7,n=nrow(Exo_2_requete_7))

