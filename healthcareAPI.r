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
                                     },
                                     
                                     nextPage = function(){
                                       #TODO: implement updating page# of query
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
                                 self$setZip(zipcode)
                                 query <- private$updateQuery(private$zip)
                                 super$initialize(query, "getCountiesForZip")
                               },
                               getResponseClass = function(){
                                 return("ZipcodeValidationResponse")
                               },
                               setZip = function(zipcode){
                                 zip <- as.character(zipcode)
                                 if(!grepl("^[[:digit:]]{5}$", zip)){
                                   stop("Only a 5 digit zipcode can be used!")
                                 } else {
                                   private$zip <- zip
                                 }
                               },
                               getZip = function(){
                                 return(private$zip)
                               }
                             ),
                             
                             private = list(
                               zip = "",
                               updateQuery = function(zipcode){
                                 xml <- read_xml("./XML_Templates/ZipRequest_Template.xml")
                                 zipNode <- xml_find_first(xml, "//p:ZipCode")
                                 xml_text(zipNode) <- zipcode
                                 return(xml)
                               }
                             )
)

IFP_PlanQuote <- R6Class("IFPPlanQuote",
                          inherit = healthcareFinderRequest,
                          public = list(
                            initialize = function(enrollees, zipcode, effDate){
                              query <- private$updateQuery()
                              super$initialize(query, "getIFPPlanQuotes")
                            },
                            getResponseClass = function(){
                              return("IFPPlanQuoteResponse")
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
                             },
                             getResponseClass = function(){
                               return("IFPPlanDetailsResponse")
                             }
                           ),
                           
                           private = list(
                             updateQuery = function(){
                               #TODO: read in xml and update plan query
                               "query"
                             }
                           )
)
SMG_PlanQuote <- R6Class("SMGPlanQuote",
                         inherit = healthcareFinderRequest,
                         public = list(
                           initialize = function(enrollees, zipcode, effDate){
                             query <- private$updateQuery()
                             super$initialize(query, "getSMGPlanQuotes")
                           },
                           getResponseClass = function(){
                             return("SMGPlanQuoteResponse")
                           }
                         ),
                         
                         private = list(
                           updateQuery = function(){
                             #TODO: read in xml and update plan query
                             "query"
                           }
                         )
)
SMG_PlanDetails <- R6Class("SMGPlanDetails",
                           inherit = healthcareFinderRequest,
                           public = list(
                             initialize = function(enrollees, zipcode, effDate, planID){
                               query <- private$updateQuery()
                               super$initialize(query, "getSMGPlanBenefits")
                             },
                             getResponseClass = function(){
                               return("SMGPlanDetailsResponse")
                             }
                           ),
                           
                           private = list(
                             updateQuery = function(){
                               #TODO: read in xml and update plan query
                               "query"
                             }
                           )
)
HealthcareAPIRequest <- function(request){
  #TODO: verify that is HealthcareFinderRequest
  
  #TODO: submit request and confirm successful response
  
  #TODO: process request appropriately (zip vs. IFP vs. SMG, etc...)
}

processAPIResponse <- function(xmlResponse){
  #TODO: verify is xml
}

zipcodeValidation$new(3333333)