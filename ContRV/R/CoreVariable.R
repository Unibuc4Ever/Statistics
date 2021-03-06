# This module gives the S4 class definition of the
# CoreVariable type, around which is built most of 
# The project.
# Note that this module only gives the raw definition
# of the class, while constructors and operator overloading
# are defined in other modules.

# Class storing the cdf and pmf functions.
CoreVariable <- setClass (
  # Name of the class.
  "CoreVariable",
  
  # Data stored in the class.
  # Using the type `ANY` because I don't know
  # the real type of the functions.
  slots = c (
    cdf = "ANY",
    pdf = "ANY"
  ),
  
  # If we want to set a prototype.
  #     prototype = list (
  #         cdf = ...,
  #         pdf = ...
  #     )
  
  # Checks if the object is valid.
  validity = function(object) {
   if (abs(object@cdf(Inf) - 1) > 1e-1) {
     print("Total density:")
     print(object@cdf(Inf))
     return("Density function doesn't add to 1!")
   }
   return(T)
  }
)
