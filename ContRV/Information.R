# Used as guards
Information <- T

Information <- function(dist_name = "") {
    if (dist_name == "") {
        cat("===================================================================
    Pachetul ContRV permite lucrul cu variabile continue aleatoare.
    Apelati functia 'Information(nume_distributie)' pentru a afla mai multe informatii despre o distributie anume:
    Diferite distributii:
      - Uniforma
      - Normala
      - Exponentiala
      - Chi squared
      - Gamma")
    }
    else if (dist_name == "Uniforma") {
        cat("===================================================================
    Distributia uniforma reprezinta alegerea unei valori intr-un interval [a, b] 
    in care toate valorile au aceeasi probabilitate")
    }
    else if (dist_name == "Normala") {
        cat("===================================================================
    Distributia normala este cea mai intalnita in statistica, si in natura.
    Este caracterizata de 2 parametrii: 
      - miu: Reprezinta media valorilor, ce este si mediana distributiei
      - sigma: Reprezinta deviatia standard
      
    Alte informatii: 
      - PDF: (e^(-1/2 * ((x - miu) / sigma)^2)) / (sigma * sqrt(2 * PI))
      - CDF: 1/2 * (1 + erf((x - miu) / (sigma * sqrt(2))))
      - media: miu
      - mediana: miu
      - dispersia: sigma^2
      
      - 68.2% din valori intr-o distributie normala se afla la o deviatie standard de medie
      - 95.4% din valori intr-o distributie normala se afla la 2 deviatii standard de medie
      - 99.6% din valori intr-o distributie normala se afla la 3 deviatii standard de medie
      
      - Aceasta distributie poate modela inaltimile oamenilor intr-un grup, sau aria frunzelor unui copac")
    }
    else if (dist_name == "Exponentiala") {
        cat("===================================================================
    Distributia exponentiala modeleaza un proces in care evenimentele au loc continuu si independent la o rata constanta.
    Este caracterizata de 1 parametru: 
      - lambda: Cu cat este mai mare lambda, cu atat masa probabilitatii se afla mai mult spre inceput.
      
    Alte informatii: 
      - PDF: lambda * e^(-lambda * x)
      - CDF: 1 - e^(lambda * x)
      - media: 1 / lambda
      - mediana: ln(2) / lambda
      - Varianta: 1 / lambda^2
      
      - Aceasta distributie modeleaza timpul dintre evenimente distribuite Poisson
      - Este distributiaa analoaga distributiei geometrice pe cazul discret")
    }
    else if (dist_name == "Chi squared") {
        
    }
    else if (dist_name == "Gamma") {
        
    }
}