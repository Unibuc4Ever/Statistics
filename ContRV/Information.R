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
      - Pareto
")
    }
    else if (dist_name == "Uniforma") {
        cat("===================================================================
    Distributia uniforma reprezinta alegerea unei valori intr-un interval [a, b] 
    in care toate valorile au aceeasi probabilitate
    
    Caracterizata prin 2 argumente:
      - a: Capatul stanga
      - b: Capatul dreapta
    
    Alte informatii:
      - Suport: [a, b]
      - PDF: 1 / (b - a)
      - CDF: (x - a) / (b - a)
      - media: (b - a) / 2
      - mediana: (b - a) / 2
      - dispersia: (b - a)^2 / 12
")
    }
    else if (dist_name == "Normala") {
        cat("===================================================================
    Distributia normala este cea mai intalnita in statistica, si in natura.
    
    Este caracterizata de 2 parametrii: 
      - miu: Reprezinta media valorilor, ce este si mediana distributiei
      - sigma: Reprezinta deviatia standard
      
    Alte informatii: 
      - Suport: (-Inf, Inf)
      - PDF: (e^(-1/2 * ((x - miu) / sigma)^2)) / (sigma * sqrt(2 * PI))
      - CDF: 1/2 * (1 + erf((x - miu) / (sigma * sqrt(2))))
      - media: miu
      - mediana: miu
      - dispersia: sigma^2
      
      - 68.2% din valori intr-o distributie normala se afla la o deviatie standard de medie
      - 95.4% din valori intr-o distributie normala se afla la 2 deviatii standard de medie
      - 99.6% din valori intr-o distributie normala se afla la 3 deviatii standard de medie
      
      - Aceasta distributie poate modela inaltimile oamenilor intr-un grup, sau aria frunzelor unui copac
")
    }
    else if (dist_name == "Exponentiala") {
        cat("===================================================================
    Distributia exponentiala modeleaza un proces in care evenimentele au loc continuu si independent la o rata constanta.
    
    Este caracterizata de 1 parametru: 
      - lambda: Cu cat este mai mare lambda, cu atat masa probabilitatii se afla mai mult spre inceput.
      
    Alte informatii: 
      - Suport: [0, Inf)
      - PDF: lambda * e^(-lambda * x)
      - CDF: 1 - e^(lambda * x)
      - media: 1 / lambda
      - mediana: ln(2) / lambda
      - Varianta: 1 / lambda^2
      
      - Aceasta distributie modeleaza timpul dintre evenimente distribuite Poisson
      - Este distributiaa analoaga distributiei geometrice pe cazul discret
")
    }
    else if (dist_name == "Chi squared") {
        cat("===================================================================
    Distributia Chi squared modeleaza suma patratelor a K variabile aleatoare standard normale, i.e. ~N(0, 1)
    
    Este caracterizata de 1 parametru: 
      - K: Gradul de libertate sau numarul de variabile standard normale
      
    Alte informatii: 
      - Suport: [0, Inf), sau (0, Inf) daca K = 1
      - PDF: x^(k/2 - 1) * e^(-x/2) / (2^(k/2) * Gamma(k/2))
      - CDF: emc * (k/2, x/2) / Gamma(k/2)
      - media: K
      - mediana: aproximativ  K * (1 - 2/9k)^3
      - Varianta: 2 * K
      
      - Este un caz particular al distributiei Gamma
")
    }
    else if (dist_name == "Pareto") {
        cat("===================================================================
    Distributia Pareto este folosita in ingineria civila, economie, si sociologie.
    Original a fost aplicata sa modeleze distributia aveerii intr-o societate

    Este caracterizata de 1 parametru: 
      - alpha: Cu cat este mai mare alpha, cu atat masa probabilitatii se afla mai mult spre inceput.
      - x_m: Punctul de start al suportului distributiei

    Alte informatii: 
      - Suport: [x_m, Inf)
      - PDF: alpha * x_m^alpha / x^(alpha + 1)
      - CDF: 1 - (x_m / x) ^ alpha
      - media: Inf  pt. [alpha <= 1]  sau  alpha * x_m / (alpha - 1)  pt. [alpha > 1]
      - mediana: x_m * 2^(alpha / 2)
      - Varianta: Inf pt. [alpha <= 2]  sau  x_m^2 * alpha / (alpha - 1)^2 * (alpha - 2)  pt. [alpha > 2]
      
      - De aici avem principiul Pareto: Iti ia 20% din timp sa faci 80% dintr-un task, si 80% din timp pentru ultimii 20%
")
    }
    else {
        cat("Distributia cautata nu este catalogata. Incercati una dintre urmatoarele:
        'Uniforma'
        'Normala'
        'Exponentiala'
        'Chi' squared
        'Pareto'
")
    }
}