require(R6)
require(httr)
require(xml2)
require(data.table)

healthcareFinderRequest <- R6Class("HealthcareFinderRequest",
                                   public = list(
                                     initialize = function(query, urlSuffix){
                                       # validate input
                                       private$requestXML <- query
                                       private$url <- paste("https://api.finder.healthcare.gov", "v3.0", urlSuffix, sep = "/")
                                       private$header <- add_headers(Host = "api.finder.healthcare.gov",
                                                                     Connection = "close",
                                                                     'Content-Length' = nchar(unserialize(xml_serialize(private$requestXML, connection = NULL))[1])
                                       )
                                     },
                                     
                                     getQueryXML = function(){
                                       unserialize(xml_serialize(private$requestXML, connection = NULL))[1]
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
  #TODO: verify more robustly
  if(!any(class(request) == "HealthcareFinderRequest")){
    stop("Must use a HealthcareFinderRequest object for this function!")
  }
  
  requestURL <- request$getURL()
  requestHeader <- request$getHeader()
  requestBody <- request$getQueryXML()
  
  response <- POST(url = requestURL,
                   config = list(requestHeader),
                   body = requestBody,
                   content_type_xml())
  
  status <- status_code(response)
  if(status != 200){
    stop(sprintf("Query failed with status %s!", status))
  }
  
  #TODO: process request appropriately (zip vs. IFP vs. SMG, etc...) along with getting additional pages
  class(response) <- append(class(response), request$getResponseClass())
  processAPIResponse(response)
}

processAPIResponse <- function(xmlResponse){
  #TODO: verify is xml
  UseMethod("processAPIResponse", xmlResponse)
}

processAPIResponse.default <- function(xmlResponse){
  stop("Unknown type!")
}

processAPIResponse.ZipcodeValidationResponse <- function(xmlResponse){
  xmlContent <- read_xml(xmlResponse)
  
  countyList <- xml_find_all(xmlContent, "//ns2:County")
  finalDT <- NULL
  
  for(i in countyList){
    childNodes <- xml_children(i)
    varNames <- xml_name(childNodes)
    varValues <- xml_text(childNodes)
    names(varValues) <- varNames
    tempDT <- setDT(as.list(varValues))
    if(is.null(finalDT)){
      finalDT <- tempDT
    } else {
      finalDT <- funion(finalDT, tempDT, all = TRUE)
    }
  }
  
  return(finalDT)
}
