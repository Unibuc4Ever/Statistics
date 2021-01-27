# Used as guards
Information <- T

Information <- function(dist_name = "") {
    if (dist_name == "") {
        cat("Pachetul ContRV permite lucrul cu variabile continue aleatoare.
    Apelati functia 'Information(nume_distributie)' pentru a afla mai multe informatii despre o distributie anume:
    Diferite distributii:
      - Uniforma
      - Normala
      - Exponentiala
      - Chi squared
      - Gamma")
    }
    else if (dist_name == "Uniforma") {
        cat("Distributia uniforma reprezinta alegerea unei valori intr-un interval [a, b] 
    in care toate valorile au aceeasi probabilitate")
    }
    else if (dist_name == "Normala") {

    }
    else if (dist_name == "Exponentiala") {
        
    }
    else if (dist_name == "Chi squared") {
        
    }
    else if (dist_name == "Gamma") {
        
    }
}