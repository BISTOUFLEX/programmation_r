# Fonction qui calcule le salaire net à partir du salaire brut
brutToNet1 = function(salaire_brut){
  # Vérification du type de la variable entrée (doit être numérique)
  if (is.numeric(salaire_brut)){
    # Calcul du salaire net
    salaire_net = 0.78 * salaire_brut
    # Retourne le salaire net
    return(salaire_net)
  } else {
    # Affiche une erreur si la variable entrée n'est pas numérique
    stop("ERROR : type not expected")
  }
}

brutToNet1(1400)

# Fonction qui calcule le salaire net en fonction du contrat
brutToNet2 = function(salaire_brut,contrat){
  # Calcul du salaire net après imposition
  salaire_net_impo = salaire_brut * 0.925
  # Vérification du contrat pour déterminer le pourcentage de salaire net
  if(contrat == "cadre"){
    salaire_net = salaire_net_impo * 0.75
  }
  else if (contrat == "non cadre"){
    salaire_net = salaire_net_impo * 0.78
  }
  else {
    # Affiche une erreur si le contrat n'est pas reconnu
    print("ERROR : contract unknown")
  }
  # Retourne le salaire net
  return(salaire_net) 
}

brutToNet2(1900, "cadre")

# Fonction qui calcule le salaire net en fonction du contrat, du taux de prélèvement et du temps de travail
brutToNet3 = function(salaire_brut, contrat, prelevement = 7.5, temps_travail = 100){
  # Vérification que le taux de prélèvement et le temps de travail sont compris entre 0 et 100%
  if (prelevement < 0 || prelevement > 100 || temps_travail < 0 || temps_travail > 100) {
    stop("ERROR : rate and time must be in range(0,100)")
  }
  # Conversion du taux de prélèvement et du temps de travail en pourcentage
  prelevement = prelevement/100
  temps_travail = temps_travail/100
  # Calcul du salaire net après imposition
  salaire_net_impo = salaire_brut * (1 - prelevement)
  # Vérification du contrat pour déterminer le pourcentage de salaire net
  if(contrat == "cadre"){
    salaire_net = salaire_net_impo * 0.75
  }
  else if (contrat == "non cadre"){
    salaire_net = salaire_net_impo * 0.78
  }
  else {
    stop("ERROR : contrat non reconnu")
  }
  return(paste("Le salaire net est égal à", salaire_net, "et le salaire brut est de", salaire_brut))
}


brutToNet3(1000, "cadre",7.5,88)  
