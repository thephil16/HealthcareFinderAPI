require(R6)
require(httr)
require(xml2)
require(data.table)
require(lubridate)

enrollee <- R6Class("Enrollee",
                    public = list(
                      initialize = function(birthDate, gender, tobaccoLastUsed, relation, householdIndicator){
                        private$setDOB(birthDate)
                        private$setGender(gender)
                        private$setTobaccoUse(tobaccoLastUsed)
                        private$setRelation(relation)
                        private$setHouseholdStatus(householdIndicator)
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
                          stop("Improperly formatted input for birth date!")
                        }
                        private$DOB <- birthDate
                      },
                      setGender = function(gender){
                        # needs to be either 'Male' or 'Female'
                        if(grepl("(?i)^[m]$|(?:^male$)", gender)){
                          output <- "Male"
                        } else if(grepl("(?i)^[f]$|(?:^female$)", gender)) {
                          output <- "Female"
                        } else {
                          stop("Improperly formatted input for gender!\n
                               Gender should be either 'Male' or 'Female'")
                        }
                        private$gender <- output
                      },
                      setTobaccoUse = function(tobaccoLastUsed){
                        if(is.null(tobaccoLastUsed)){
                          private$tobaccoUse <- NULL
                        } else if(is.integer()) {
                          output <- min(tobaccoLastUsed, 6)
                          private$tobaccoUse <- output
                        } else {
                          stop("Improperly formatted input for tobacco use!\n
                               Months since last used tobacco should be between 1 and 6.")
                        }
                      },
                      setRelation = function(relationship){
                        if(!is.character(relationship)){
                          stop("Improperly formatted input for relationship!")
                        }
                        
                        private$relation <- toupper(relationship)
                      },
                      setHouseholdStatus = function(householdStatus){
                        if(!is.logical(householdStatus)){
                          stop("Household relation must be a logical value!")
                        } else {
                          # May need to convert to character here. Or could do so when writing xml
                          # householdStatus <- tolower(as.character(householdStatus))
                          private$householdIndicator <- householdStatus
                        }
                      }
                    )
)

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
                                 private$setZip(zipcode)
                                 query <- private$updateQuery()
                                 super$initialize(query, "getCountiesForZip")
                               },
                               getResponseClass = function(){
                                 return("ZipcodeValidationResponse")
                               },
                               getZip = function(){
                                 return(private$zip)
                               }
                             ),
                             
                             private = list(
                               zip = "",
                               setZip = function(zipcode){
                                 zip <- as.character(zipcode)
                                 if(!grepl("^[[:digit:]]{5}$", zip)){
                                   stop("Only a 5 digit zipcode can be used!")
                                 } else {
                                   private$zip <- zip
                                 }
                               },
                               updateQuery = function(){
                                 xml <- read_xml("./XML_Templates/ZipRequest_Template.xml")
                                 zipNode <- xml_find_first(xml, "//p:ZipCode")
                                 xml_text(zipNode) <- private$zip
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
                            getEnrollees = function(){
                              private$enrollees
                            },
                            getLocation = function(){
                              return()
                            },
                            getEffDate = function(){
                              private$effDate
                            }
                          ),
                          
                          private = list(
                            enrollees = list(),
                            zip = "",
                            fipsCode = "",
                            county = "",
                            state = "",
                            effDate = "",
                            setEnrollees = function(enrollees){
                              if(!is.list(enrollees)){
                                stop("Enrollees must be a list of 'Enrollee' objects!")
                              }
                              
                              selfCount <- 0
                              spouseCount <- 0
                              dependentCount <- 0
                              for(i in enrollees){
                                if(!any(class(i) == "Enrollee")){
                                  stop("All enrollees must be in 'Enrollee' class!")
                                }
                                if(i$getRelation() == "SELF"){
                                  selfCount <- selfCount + 1
                                } else if(i$getRelation() == "SPOUSE" | i$getRelation() == "LIFE_PARTNER"){
                                  spouseCount <- spouseCount + 1
                                } else {
                                  dependentCount <- dependentCount + 1
                                }
                              }
                              
                              if(selfCount != 1 | spouseCount > 1 | dependentCount > 5){
                                stop("Bad enrollee combination!")
                              }
                              
                              private$enrollees <- enrollees
                            },
                            setLocation = function(zip, county){
                              county <- toupper(county)
                              
                              tempZipRequest <- zipcodeValidation$new(zip)
                              response <- HealthcareAPIRequest(tempZipRequest)
                              results <- processAPIResponse(response)
                              
                              results <- results[CountyName == county]
                              if(results[,.N] != 1){
                                stop("No matching locations!")
                              }
                              
                              private$fipsCode <- results[,FipsCode]
                              private$zip <- zip
                              private$county <- results[,CountyName]
                              private$state <- results[,StateCode]
                            },
                            setEffDate= function(effDate){
                              if (!is.POSIXct(effDate)) {
                                stop("Improperly formatted input for effective date!")
                              }
                              private$effDate <- effDate
                            },
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

HealthcareAPIRequest <- function(request){
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
