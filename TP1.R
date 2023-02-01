brut_vers_net = function(salaire_brut){
  if (is.numeric(salaire_brut)){
    salaire_net = 0.78*salaire_brut
    return(salaire_net)
    }else{
    print("ERROR")
    }
  }


brut_vers_net(1300)

brut_vers_net2 = function(salaire_brut,contrat){
  salaire_net_impo = salaire_brut*0.925
  if(contrat=="cadre"){
    salaire_net=salaire_net_impo*0.75
  }
  else if (contrat=="non cadre"){
    salaire_net=salaire_net_impo*0.78
  }
  else {
    print("ERROR : contract unknown")
  }
  return(salaire_net) 
  }

brut_vers_net2(1900,"cadre")