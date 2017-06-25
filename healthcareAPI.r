require(R6)
require(httr)
require(xml2)

healthcareFinderRequest <- R6Class("HealthcareFinderRequest",
                                   public = list(
                                     initialize = function(query, urlSuffix){
                                       # validate input
                                       private$requestXML <- query
                                       private$url <- paste("https://api.finder.healthcare.gov", "v3.0", urlSuffix, sep = "/")
                                       private$header <- add_headers(Host = "api.finder.healthcare.gov",
                                                                     Connection = "close",
                                                                     'Content-Length' = nchar(private$requestXML)
                                       )
                                     },
                                     
                                     getQueryXML = function(){
                                       return(private$requestXML)
                                     },
                                     
                                     getHeader = function(){
                                       return(private$header)
                                     },
                                     
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

zipcodeValidation <- R6Class("ZipcodeValidation",
                             inherit = healthcareFinderRequest,
                             public = list(
                               initialize = function(zipcode){
                                 private$zip <- zipcode
                                 query <- private$updateQuery()
                                 super$initialize(query, "getCountiesForZip")
                               }
                             ),
                             
                             private = list(
                               zip = "",
                               updateQuery = function(){
                                 #TODO: read in xml and update zipcode query
                                 "query"
                               }
                             )
)

IFP_PlanFinder <- R6Class("IFPPlanFinder",
                          inherit = healthcareFinderRequest,
                          public = list(
                            initialize = function(enrollees, zipcode, effDate){
                              query <- private$updateQuery()
                              super$initialize(query, "getIFPPlanQuotes")
                            }
                          ),
                          
                          private = list(
                            updateQuery = function(){
                              #TODO: read in xml and update plan query
                              "query"
                            }
                          )
)
IFP_PlanDetails <- R6Class("IFPPlanDetails",
                           inherit = healthcareFinderRequest,
                           public = list(
                             initialize = function(enrollees, zipcode, effDate, planID){
                               query <- private$updateQuery()
                               super$initialize(query, "getIFPPlanBenefits")
                             }
                           ),
                           
                           private = list(
                             updateQuery = function(){
                               #TODO: read in xml and update plan query
                               "query"
                             }
                           )
)
healthcareFinderResponse <- R6Class("HealthcareFinderResponse")