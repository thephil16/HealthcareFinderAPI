require(R6)
require(httr)
require(xml2)

healthcareFinderRequest <- R6Class("HealthcareFinderRequest",
                                   public = list(
                                     initialize = function(query, urlSuffix){
                                       # validate input
                                       private$requestXML <- query
                                       private$url <- paste("https://api.finder.healthcare.gov", "v3.0", urlSuffix, sep = "/")
                                       private$header <- add_headers()
                                     },
                                     
                                     getQueryXML = function(){
                                       return(private$requestXML)
                                     }
                                     
                                     getHeader = function(){
                                       return(private$header)
                                     }
                                     
                                     getURL = function(){
                                       return(private$url)
                                     }
                                   ),
                                   
                                   private = list(
                                     requestXML = "",
                                     url = "",
                                     header = ""
                                   )
)

zipcodeValidation <- R6Class("ZipcodeValidation")

IFP_PlanFinder <- R6Class("IFPPlanFinder")

IFP_PlanDetails <- R6Class("IFPPlanDetails")

healthcareFinderResponse <- R6Class("HealthcareFinderResponse")