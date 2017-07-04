require(R6)
require(httr)
require(xml2)
require(data.table)
require(lubridate)

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
                                       pageNode <- xml_find_first(private$requestXML, "//p:PaginationInformation/PageNumber")
                                       
                                       curPage <- xml_integer(pageNode)
                                       nextPage <- curPage + 1
                                       
                                       xml_integer(pageNode) <- nextPage
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
                            initialize = function(enrollees, zipcode, county, effDate){
                              query <- private$updateQuery()
                              super$initialize(query, "getIFPPlanQuotes")
                            },
                            
                            getResponseClass = function(){
                              return("IFPPlanQuoteResponse")
                            },
                            
                            setEnrollees = function(enrollees){
                              is.list(enrollees)
                            },
                            
                            setLocation = function(zip, county){
                              
                            },
                            
                            setEffDate= function(effDate){
                              
                            }
                          ),
                          
                          private = list(
                            enrollees = list(),
                            zip = "",
                            county = "",
                            effDate = "",
                            
                            updateQuery = function(){
                              #TODO: read in xml and update plan query
                            }
                          )
)
IFP_PlanDetails <- R6Class("IFPPlanDetails",
                           inherit = healthcareFinderRequest,
                           public = list(
                             initialize = function(enrollees, zipcode, county, effDate, planID){
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
                           initialize = function(enrollees, zipcode, county, effDate){
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
                             initialize = function(enrollees, zipcode, county, effDate, planID){
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
enrollee <- R6Class("Enrollee",
                    public = list(
                      initialize = function(birthDate, gender, tobaccoLastUsed, relation, householdIndicator){
                        private$setDOB(birthDate)
                        private$setGender(gender)
                        private$setTobaccoUse(tobaccoLastUsed)
                        private$setRelation(relation)
                        private$setHouseholdIndicator(householdIndicator)
                      },
                      
                      getDOB = function(){
                        private$DOB
                      },
                      getGender = function(){
                        private$gender
                      },
                      getTobaccoUse = function(){
                        private$tobaccoUse
                      },
                      getRelation = function(){
                        private$relation
                      },
                      getHouseholdIndicator = function(){
                        private$householdIndicator
                      }
                    ),
                    private = list(
                      DOB = "",
                      gender = "",
                      tobaccoUse = "",
                      relation = "",
                      householdIndicator = "",
                      
                      setDOB = function(birthDate){
                        if(!is.POSIXct(birthDate)){
                          stop("Improper date format")
                        }
                        private$DOB <- birthDate
                      },
                      setGender = function(gender){
                        # needs to be either 'Male' or 'Female'
                        if(!grepl("?i[mf]$|(?:(?:fe)?male)", gender)){
                          stop("Improper gender format")
                        }
                        output <- toupper(strtrim(gender, 1))
                        private$gender <- output
                      },
                      setTobaccoUse = function(tobaccoLastUsed){
                        if(is.null(tobaccoLastUsed)){
                          return()
                        } else if(is.integer()) {
                          output <- min(tobaccoLastUsed, 6)
                          private$tobaccoUse <- output
                          return()
                        } else {
                          stop("Incorrect input for tobacco use!")
                        }
                      },
                      setRelation = function(relationship){
                        
                      },
                      setHouseholdRelation = function(householdStatus){
                        if(!is.logical(householdStatus)){
                          stop("Household relation must be a logical value!")
                        } else {
                          private$householdIndicator <- householdStatus
                        }
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
  return(response)
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
