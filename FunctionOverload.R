# Used as guards
FunctionOverloadFile <- T

setMethod(f="print",
    signature=c("CoreVariable"),
    definition=function(x)
    {
        print("Wassaaaap?")
        print("Value of the CoreVar:")
        print("PMF: ", x@pmf)
        print("CDF: ", x@cdf)
    }
)