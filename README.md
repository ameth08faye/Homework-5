# Homework-5
##4)
library(tidyverse)
glimpse(cereales)
View(cereales)
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")



# Création de variable facteur avec recodage
# Créons d'abord le dataframe avec la colonne cereales_id
donnees_cereales <- data.frame(cereales_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,127))

# Ajoutons la colonne groupe_cereales
donnees_cereales$groupe_cereales <- factor(ifelse(
  donnees_cereales$cereales_id %in% c(1, 2, 5:10, 12:19, 20:26),
  "Local",
  ifelse(donnees_cereales$cereales_id %in% c(3, 4), "Importé",NA)
))

# Affichons les premières lignes du dataframe pour vérification
head(donnees_cereales)

# Affichons les premières lignes du dataframe pour vérification
head(cereales)

# Convertissons la colonne groupe_cereales en caractère
donnees_cereales$groupe_cereales <- as.character(donnees_cereales$groupe_cereales)

# Mettonsà jour les valeurs
donnees_cereales$groupe_cereales[donnees_cereales$groupe_cereales == "Local"] <- "1"
donnees_cereales$groupe_cereales[donnees_cereales$groupe_cereales == "Importé"] <- "2"

# Reconvertissons la colonne en facteur avec les bons niveaux
donnees_cereales$groupe_cereales <- factor(donnees_cereales$groupe_cereales, levels = c("1", "2"),
                                           labels = c("Local", "Importé"))

# Affichez les premières lignes du dataframe pour vérification
head(donnees_cereales)

unique(donnees_cereales$groupe_cereales)

##Convertissons la variable Taille_cons en facteur
#Labelisons la variable Taille_cons 0=aucune ,1=faible ,2=moyen , 3=élevé 7=très élevé:recoder des variables
table(cereales$Taille_cons)
# Définition des niveaux de la variable Taille_cons
niveaux <- c("aucune", "faible", "moyen", "élevé", "très élevé")
# Conversion de la variable Taille_cons en facteur avec les niveaux spécifiés
cereales$Taille_cons <- factor(cereales$Taille_cons, levels = c(0, 1, 2, 3, 7), labels = niveaux)


##5) Changeons de type de la Qtty_cons initialement en num en type char variable'
# Conversion de la variable Qtty_cons en type caractère
cereales$Qtty_cons <- as.character(cereales$Qtty_cons)

##6)Découpage en classe de la classe Qtty_cons

library(dplyr)
cereales <- cereales %>%
  mutate (Quantite_consommée_recodée = case_when (
    Qtty_cons < 60 ~ "Faible quantité consommée", 
    Qtty_cons >= 60 & Qtty_cons < 120 ~ "Moyenne quantité consommée",
    Qtty_cons >= 170 ~ "Forte quantité consommée",
    TRUE ~ NA_character_
  ))

unique(cereales$Quantite_consommée_recodée)

# 7) Fusionnons la base "cereales" et celle "Table_de_conversion_phase_2"
# Supposons que votre dataframe s'appelle "cereales" et que vous avez déjà chargé "table_conv"
# Utilisez directement left_join() pour effectuer la jointure
cereales <- left_join(cereales, Table_de_conversion_phase_2,
                      by = c("cereales__id" = "produitID",
                             "Unite_cons" = "uniteID",
                             "Taille_cons" = "tailleID"))

# Affichez les premières lignes du dataframe pour vérification
head(cereales)

