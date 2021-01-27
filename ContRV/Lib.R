# Used as guard
LibFile <- T

# TO REMOVE
rm(list = ls())

# This module is the "API" of the package, and
# it's only role is to link the required submodules.
# It's just an interface linking together the
# different components of the project.


# Raw definition of the CoreVariable class.
if (!exists('CoreVarFile'))
  source('CoreVariable.R')
    
# CDF and PMF constructors of the CoreVariable.
if (!exists('ConstructorsFile'))
  source('Constructors.R')

# Overload of various functions function.
if (!exists('FunctionOverloadFile'))
  source('FunctionOverload.R')

# Overload of the print function.
if (!exists('OperatorOverloadFile'))
  source('OperatorOverload.R')

# Include main distributions.
if (!exists('MainDistributionsFile'))
  source('MainDistributions.R')

# Useful functions.
if (!exists('UtilitiesFile'))
  source('Utilities.R')

# 
if (!exists('MomentUtilsFile'))
  source('MomentUtils.R')
