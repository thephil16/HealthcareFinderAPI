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
                        format(private$DOB, format = "%F")
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
                        bday <- as.POSIXct(birthDate)
                        if(is.na(bday)){
                          stop("Improperly formatted input for birth date!")
                        }
                        private$DOB <- bday
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
                          return()
                        }
                        
                        lastUsed <- as.integer(tobaccoLastUsed)
                        if(is.na(lastUsed) || lastUsed < 1){
                          stop("Improperly formatted input for tobacco use!\n
                               Months since last used tobacco should be between 1 and 6.")
                        } else {
                          private$tobaccoUse <- min(lastUsed, 6)
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
                                                                     'Content-Length' = nchar(as.character(private$requestXML))
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
                              private$setEnrollees(enrollees)
                              private$setLocation(zipcode, county)
                              private$setEffDate(effDate)
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
                              format(private$effDate, format = "%F")
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
                              resultsCount <- results[,.N]
                              
                              if(resultsCount == 0){
                                stop("Invalid zip code!")
                              } else if (resultsCount > 1){
                                results <- results[CountyName == county]
                                if(results[,.N] != 1){
                                  stop("No matching locations!")
                                }
                              }

                              private$fipsCode <- results[,FipsCode]
                              private$zip <- zip
                              private$county <- results[,CountyName]
                              private$state <- results[,StateCode]
                            },
                            setEffDate= function(effDate){
                              date <- as.POSIXct(effDate)
                              if (is.na(date)) {
                                stop("Improperly formatted input for effective date!")
                              }
                              private$effDate <- date
                            },
                            updateQuery = function(){
                              #TODO: read in xml and update plan query
                              xml <- read_xml("./XML_Templates/PlanQuoteRequest_Template.xml")
                              requestNode <- xml_find_first(xml, "/p:PlanQuoteRequest")
                              
                              # update enrollees
                              for(enr in private$enrollees){
                                ## add enrollee
                                xml_add_child(requestNode, "p:Enrollees", .where = 0)
                                curNode <- xml_find_first(requestNode, "p:Enrollees")
                                
                                ## update DOB
                                xml_add_child(curNode, "p1:DateOfBirth")
                                xml_set_text(xml_find_first(curNode, "p1:DateOfBirth"), enr$getDOB())
                                
                                ## update Gender
                                xml_add_child(curNode, "p1:Gender")
                                xml_set_text(xml_find_first(curNode, "p1:Gender"), enr$getGender())
                                
                                ## update TobaccoUse
                                if(!is.null(enr$getTobaccoUse())){
                                  xml_add_child(curNode, "p1:TobaccoLastUsedMonths")
                                  xml_set_text(xml_find_first(curNode, "p1:TobaccoLastUsedMonths"), as.character(enr$getTobaccoUse()))
                                }
                                
                                ## Update Relation
                                xml_add_child(curNode, "p1:Relation")
                                xml_set_text(xml_find_first(curNode, "p1:Relation"), enr$getRelation())
                                
                                ## Update In-House Status
                                xml_add_child(curNode, "p1:InHouseholdIndicator")
                                xml_set_text(xml_find_first(curNode, "p1:InHouseholdIndicator"), tolower(as.character(enr$getHouseholdIndicator())))
                              }
                              
                              # update location
                              xml_add_child(requestNode, "p:Location")
                              curNode <- xml_find_first(requestNode, "p:Location")
                              ## update zipcode
                              xml_add_child(curNode, "p1:ZipCode")
                              xml_set_text(xml_find_first(curNode, "p1:ZipCode"), as.character(private$zip))
                              ## update county
                              xml_add_child(curNode, "p1:County")
                              curNode <- xml_find_first(curNode, "p1:County")
                              
                              xml_add_child(curNode, "p1:FipsCode")
                              xml_set_text(xml_find_first(curNode, "p1:FipsCode"), private$fipsCode)
                              
                              
                              xml_add_child(curNode, "p1:CountyName")
                              xml_set_text(xml_find_first(curNode, "p1:CountyName"), private$county)
                              
                              xml_add_child(curNode, "p1:StateCode")
                              xml_set_text(xml_find_first(curNode, "p1:StateCode"), private$state)
                              
                              # update effdate
                              xml_add_child(requestNode, "p:InsuranceEffectiveDate")
                              xml_set_text(xml_find_first(requestNode, "p:InsuranceEffectiveDate"), self$getEffDate())
                              
                              # update Market
                              xml_add_child(requestNode, "p:Market")
                              xml_set_text(xml_find_first(requestNode, "p:Market"), "Individual")
                              
                              # update filters
                              xml_add_child(requestNode, "p:IsFilterAnalysisRequiredIndicator")
                              xml_set_text(xml_find_first(requestNode, "p:IsFilterAnalysisRequiredIndicator"), "false")
                              
                              # update pagination info
                              xml_add_child(requestNode, "p:PaginationInformation")
                              curNode <- xml_find_first(requestNode, "p:PaginationInformation")
                              ## update pagenumber
                              xml_add_child(curNode, "p1:PageNumber")
                              xml_set_text(xml_find_first(curNode, "p1:PageNumber"), as.character(1))
                              ## update pagesize
                              xml_add_child(curNode, "p1:PageSize")
                              xml_set_text(xml_find_first(curNode, "p1:PageSize"), as.character(500))
                              
                              # Update sort order
                              xml_add_child(requestNode, "p:SortOrder")
                              curNode <- xml_find_first(requestNode, "p:SortOrder")
                              ## update sort field
                              xml_add_child(curNode, "p1:SortField")
                              xml_set_text(xml_find_first(curNode, "p1:SortField"), "BASE RATE")
                              ## update sort direction
                              xml_add_child(curNode, "p1:SortDirection")
                              xml_set_text(xml_find_first(curNode, "p1:SortDirection"), "ASC")
                              
                              return(xml)
                            }
                          )
)

IFP_PlanDetails <- R6Class("IFPPlanDetails",
                         inherit = healthcareFinderRequest,
                         public = list(
                           initialize = function(enrollees, zipcode, county, effDate, planID){
                             private$setEnrollees(enrollees)
                             private$setLocation(zipcode, county)
                             private$setEffDate(effDate)
                             private$setPlanID(planID)
                             query <- private$updateQuery()
                             super$initialize(query, "getIFPPlanBenefits")
                           },
                           getResponseClass = function(){
                             return("PlanDetailsResponse")
                           },
                           getEnrollees = function(){
                             private$enrollees
                           },
                           getLocation = function(){
                             return()
                           },
                           getEffDate = function(){
                             format(private$effDate, format = "%F")
                           },
                           getPlanID = function(){
                             private$planID
                           }
                         ),
                         
                         private = list(
                           enrollees = list(),
                           zip = "",
                           fipsCode = "",
                           county = "",
                           state = "",
                           effDate = "",
                           planID = "",
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
                             resultsCount <- results[,.N]
                             
                             if(resultsCount == 0){
                               stop("Invalid zip code!")
                             } else if (resultsCount > 1){
                               results <- results[CountyName == county]
                               if(results[,.N] != 1){
                                 stop("No matching locations!")
                               }
                             }
                             
                             private$fipsCode <- results[,FipsCode]
                             private$zip <- zip
                             private$county <- results[,CountyName]
                             private$state <- results[,StateCode]
                           },
                           setEffDate= function(effDate){
                             date <- as.POSIXct(effDate)
                             if (is.na(date)) {
                               stop("Improperly formatted input for effective date!")
                             }
                             private$effDate <- date
                           },
                           setPlanID = function(ID){
                             if(!grepl("^[[:digit:]]{5}[[:upper:]]{2}[[:digit:]]{7}$", ID)){
                               stop("Incorrect Plan ID format!")
                             } else {
                               private$planID <- ID
                             }
                           },
                           updateQuery = function(){
                             #TODO: read in xml and update plan query
                             xml <- read_xml("./XML_Templates/PlanBenefitRequest_Template.xml")
                             requestNode <- xml_find_first(xml, "/p:PlanBenefitRequest")
                             
                             # update enrollees
                             for(enr in private$enrollees){
                               ## add enrollee
                               xml_add_child(requestNode, "p:Enrollees", .where = 0)
                               curNode <- xml_find_first(requestNode, "p:Enrollees")
                               
                               ## update DOB
                               xml_add_child(curNode, "p1:DateOfBirth")
                               xml_set_text(xml_find_first(curNode, "p1:DateOfBirth"), enr$getDOB())
                               
                               ## update Gender
                               xml_add_child(curNode, "p1:Gender")
                               xml_set_text(xml_find_first(curNode, "p1:Gender"), enr$getGender())
                               
                               ## update TobaccoUse
                               if(!is.null(enr$getTobaccoUse())){
                                 xml_add_child(curNode, "p1:TobaccoLastUsedMonths")
                                 xml_set_text(xml_find_first(curNode, "p1:TobaccoLastUsedMonths"), as.character(enr$getTobaccoUse()))
                               }
                               
                               ## Update Relation
                               xml_add_child(curNode, "p1:Relation")
                               xml_set_text(xml_find_first(curNode, "p1:Relation"), enr$getRelation())
                               
                               ## Update In-House Status
                               xml_add_child(curNode, "p1:InHouseholdIndicator")
                               xml_set_text(xml_find_first(curNode, "p1:InHouseholdIndicator"), tolower(as.character(enr$getHouseholdIndicator())))
                             }
                             
                             # update location
                             xml_add_child(requestNode, "p:Location")
                             curNode <- xml_find_first(requestNode, "p:Location")
                             ## update zipcode
                             xml_add_child(curNode, "p1:ZipCode")
                             xml_set_text(xml_find_first(curNode, "p1:ZipCode"), as.character(private$zip))
                             ## update county
                             xml_add_child(curNode, "p1:County")
                             curNode <- xml_find_first(curNode, "p1:County")
                             
                             xml_add_child(curNode, "p1:FipsCode")
                             xml_set_text(xml_find_first(curNode, "p1:FipsCode"), private$fipsCode)
                             
                             
                             xml_add_child(curNode, "p1:CountyName")
                             xml_set_text(xml_find_first(curNode, "p1:CountyName"), private$county)
                             
                             xml_add_child(curNode, "p1:StateCode")
                             xml_set_text(xml_find_first(curNode, "p1:StateCode"), private$state)
                             
                             # update effdate
                             xml_add_child(requestNode, "p:InsuranceEffectiveDate")
                             xml_set_text(xml_find_first(requestNode, "p:InsuranceEffectiveDate"), self$getEffDate())
                             
                             # update Market
                             xml_add_child(requestNode, "p:Market")
                             xml_set_text(xml_find_first(requestNode, "p:Market"), "Individual")
                             
                             # update Plan IDs
                             xml_add_child(requestNode, "p:PlanIds")
                             curNode <- xml_find_first(requestNode, "p:PlanIds")
                             xml_add_child(curNode, "p:PlanId")
                             xml_set_text(xml_find_first(curNode, "p:PlanId"), self$getPlanID())
                             
                             return(xml)
                           }
                         )
)

SMG_PlanQuote <- R6Class("SMGPlanQuote",
                         inherit = healthcareFinderRequest,
                         public = list(
                           initialize = function(enrollees, zipcode, county, effDate){
                             private$setEnrollees(enrollees)
                             private$setLocation(zipcode, county)
                             private$setEffDate(effDate)
                             query <- private$updateQuery()
                             super$initialize(query, "getSMGPlanQuotes")
                           },
                           getResponseClass = function(){
                             return("SMGPlanQuoteResponse")
                           },
                           getEnrollees = function(){
                             private$enrollees
                           },
                           getLocation = function(){
                             return()
                           },
                           getEffDate = function(){
                             format(private$effDate, format = "%F")
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
                             resultsCount <- results[,.N]
                             
                             if(resultsCount == 0){
                               stop("Invalid zip code!")
                             } else if (resultsCount > 1){
                               results <- results[CountyName == county]
                               if(results[,.N] != 1){
                                 stop("No matching locations!")
                               }
                             }
                             
                             private$fipsCode <- results[,FipsCode]
                             private$zip <- zip
                             private$county <- results[,CountyName]
                             private$state <- results[,StateCode]
                           },
                           setEffDate= function(effDate){
                             date <- as.POSIXct(effDate)
                             if (is.na(date)) {
                               stop("Improperly formatted input for effective date!")
                             }
                             private$effDate <- date
                           },
                           updateQuery = function(){
                             #TODO: read in xml and update plan query
                             xml <- read_xml("./XML_Templates/PlanQuoteRequest_Template.xml")
                             requestNode <- xml_find_first(xml, "/p:PlanQuoteRequest")
                             
                             # update enrollees
                             for(enr in private$enrollees){
                               ## add enrollee
                               xml_add_child(requestNode, "p:Enrollees", .where = 0)
                               curNode <- xml_find_first(requestNode, "p:Enrollees")
                               
                               ## update DOB
                               xml_add_child(curNode, "p1:DateOfBirth")
                               xml_set_text(xml_find_first(curNode, "p1:DateOfBirth"), enr$getDOB())
                               
                               ## update Gender
                               xml_add_child(curNode, "p1:Gender")
                               xml_set_text(xml_find_first(curNode, "p1:Gender"), enr$getGender())
                               
                               ## update TobaccoUse
                               if(!is.null(enr$getTobaccoUse())){
                                 xml_add_child(curNode, "p1:TobaccoLastUsedMonths")
                                 xml_set_text(xml_find_first(curNode, "p1:TobaccoLastUsedMonths"), as.character(enr$getTobaccoUse()))
                               }
                               
                               ## Update Relation
                               xml_add_child(curNode, "p1:Relation")
                               xml_set_text(xml_find_first(curNode, "p1:Relation"), enr$getRelation())
                               
                               ## Update In-House Status
                               xml_add_child(curNode, "p1:InHouseholdIndicator")
                               xml_set_text(xml_find_first(curNode, "p1:InHouseholdIndicator"), tolower(as.character(enr$getHouseholdIndicator())))
                             }
                             
                             # update location
                             xml_add_child(requestNode, "p:Location")
                             curNode <- xml_find_first(requestNode, "p:Location")
                             ## update zipcode
                             xml_add_child(curNode, "p1:ZipCode")
                             xml_set_text(xml_find_first(curNode, "p1:ZipCode"), as.character(private$zip))
                             ## update county
                             xml_add_child(curNode, "p1:County")
                             curNode <- xml_find_first(curNode, "p1:County")
                             
                             xml_add_child(curNode, "p1:FipsCode")
                             xml_set_text(xml_find_first(curNode, "p1:FipsCode"), private$fipsCode)
                             
                             
                             xml_add_child(curNode, "p1:CountyName")
                             xml_set_text(xml_find_first(curNode, "p1:CountyName"), private$county)
                             
                             xml_add_child(curNode, "p1:StateCode")
                             xml_set_text(xml_find_first(curNode, "p1:StateCode"), private$state)
                             
                             # update effdate
                             xml_add_child(requestNode, "p:InsuranceEffectiveDate")
                             xml_set_text(xml_find_first(requestNode, "p:InsuranceEffectiveDate"), self$getEffDate())
                             
                             # update Market
                             xml_add_child(requestNode, "p:Market")
                             xml_set_text(xml_find_first(requestNode, "p:Market"), "SmallGroup")
                             
                             # update filters
                             xml_add_child(requestNode, "p:IsFilterAnalysisRequiredIndicator")
                             xml_set_text(xml_find_first(requestNode, "p:IsFilterAnalysisRequiredIndicator"), "false")
                             
                             # update pagination info
                             xml_add_child(requestNode, "p:PaginationInformation")
                             curNode <- xml_find_first(requestNode, "p:PaginationInformation")
                             ## update pagenumber
                             xml_add_child(curNode, "p1:PageNumber")
                             xml_set_text(xml_find_first(curNode, "p1:PageNumber"), as.character(1))
                             ## update pagesize
                             xml_add_child(curNode, "p1:PageSize")
                             xml_set_text(xml_find_first(curNode, "p1:PageSize"), as.character(500))
                             
                             # Update sort order
                             xml_add_child(requestNode, "p:SortOrder")
                             curNode <- xml_find_first(requestNode, "p:SortOrder")
                             ## update sort field
                             xml_add_child(curNode, "p1:SortField")
                             xml_set_text(xml_find_first(curNode, "p1:SortField"), "BASE RATE")
                             ## update sort direction
                             xml_add_child(curNode, "p1:SortDirection")
                             xml_set_text(xml_find_first(curNode, "p1:SortDirection"), "ASC")
                             
                             return(xml)
                           }
                         )
)

SMG_PlanDetails <- R6Class("SMGPlanDetails",
                           inherit = healthcareFinderRequest,
                           public = list(
                             initialize = function(enrollees, zipcode, county, effDate, planID){
                               private$setEnrollees(enrollees)
                               private$setLocation(zipcode, county)
                               private$setEffDate(effDate)
                               private$setPlanID(planID)
                               query <- private$updateQuery()
                               super$initialize(query, "getSMGPlanBenefits")
                             },
                             getResponseClass = function(){
                               return("PlanDetailsResponse")
                             },
                             getEnrollees = function(){
                               private$enrollees
                             },
                             getLocation = function(){
                               return()
                             },
                             getEffDate = function(){
                               format(private$effDate, format = "%F")
                             },
                             getPlanID = function(){
                               private$planID
                             }
                           ),
                           
                           private = list(
                             enrollees = list(),
                             zip = "",
                             fipsCode = "",
                             county = "",
                             state = "",
                             effDate = "",
                             planID = "",
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
                               resultsCount <- results[,.N]
                               
                               if(resultsCount == 0){
                                 stop("Invalid zip code!")
                               } else if (resultsCount > 1){
                                 results <- results[CountyName == county]
                                 if(results[,.N] != 1){
                                   stop("No matching locations!")
                                 }
                               }
                               
                               private$fipsCode <- results[,FipsCode]
                               private$zip <- zip
                               private$county <- results[,CountyName]
                               private$state <- results[,StateCode]
                             },
                             setEffDate= function(effDate){
                               date <- as.POSIXct(effDate)
                               if (is.na(date)) {
                                 stop("Improperly formatted input for effective date!")
                               }
                               private$effDate <- date
                             },
                             setPlanID = function(ID){
                               if(!grepl("^[[:digit:]]{5}[[:upper:]]{2}[[:digit:]]{7}$", ID)){
                                 stop("Incorrect Plan ID format!")
                               } else {
                                 private$planID <- ID
                               }
                             },
                             updateQuery = function(){
                               #TODO: read in xml and update plan query
                               xml <- read_xml("./XML_Templates/PlanBenefitRequest_Template.xml")
                               requestNode <- xml_find_first(xml, "/p:PlanBenefitRequest")
                               
                               # update enrollees
                               for(enr in private$enrollees){
                                 ## add enrollee
                                 xml_add_child(requestNode, "p:Enrollees", .where = 0)
                                 curNode <- xml_find_first(requestNode, "p:Enrollees")
                                 
                                 ## update DOB
                                 xml_add_child(curNode, "p1:DateOfBirth")
                                 xml_set_text(xml_find_first(curNode, "p1:DateOfBirth"), enr$getDOB())
                                 
                                 ## update Gender
                                 xml_add_child(curNode, "p1:Gender")
                                 xml_set_text(xml_find_first(curNode, "p1:Gender"), enr$getGender())
                                 
                                 ## update TobaccoUse
                                 if(!is.null(enr$getTobaccoUse())){
                                   xml_add_child(curNode, "p1:TobaccoLastUsedMonths")
                                   xml_set_text(xml_find_first(curNode, "p1:TobaccoLastUsedMonths"), as.character(enr$getTobaccoUse()))
                                 }
                                 
                                 ## Update Relation
                                 xml_add_child(curNode, "p1:Relation")
                                 xml_set_text(xml_find_first(curNode, "p1:Relation"), enr$getRelation())
                                 
                                 ## Update In-House Status
                                 xml_add_child(curNode, "p1:InHouseholdIndicator")
                                 xml_set_text(xml_find_first(curNode, "p1:InHouseholdIndicator"), tolower(as.character(enr$getHouseholdIndicator())))
                               }
                               
                               # update location
                               xml_add_child(requestNode, "p:Location")
                               curNode <- xml_find_first(requestNode, "p:Location")
                               ## update zipcode
                               xml_add_child(curNode, "p1:ZipCode")
                               xml_set_text(xml_find_first(curNode, "p1:ZipCode"), as.character(private$zip))
                               ## update county
                               xml_add_child(curNode, "p1:County")
                               curNode <- xml_find_first(curNode, "p1:County")
                               
                               xml_add_child(curNode, "p1:FipsCode")
                               xml_set_text(xml_find_first(curNode, "p1:FipsCode"), private$fipsCode)
                               
                               
                               xml_add_child(curNode, "p1:CountyName")
                               xml_set_text(xml_find_first(curNode, "p1:CountyName"), private$county)
                               
                               xml_add_child(curNode, "p1:StateCode")
                               xml_set_text(xml_find_first(curNode, "p1:StateCode"), private$state)
                               
                               # update effdate
                               xml_add_child(requestNode, "p:InsuranceEffectiveDate")
                               xml_set_text(xml_find_first(requestNode, "p:InsuranceEffectiveDate"), self$getEffDate())
                               
                               # update Market
                               xml_add_child(requestNode, "p:Market")
                               xml_set_text(xml_find_first(requestNode, "p:Market"), "Individual")
                               
                               # update Plan IDs
                               xml_add_child(requestNode, "p:PlanIds")
                               curNode <- xml_find_first(requestNode, "p:PlanIds")
                               xml_add_child(curNode, "p:PlanId")
                               xml_set_text(xml_find_first(curNode, "p:PlanId"), self$getPlanID())
                               
                               return(xml)
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

errorCheck <- function(xml){
  stopifnot(length(xml_children(xml_find_first(xml, "//ns2:ResponseHeader"))) == 1)
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
  errorCheck(xmlContent)
  
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

processAPIResponse.IFPPlanQuoteResponse <- function(xmlResponse){
  xmlContent <- read_xml(xmlResponse)
  errorCheck(xmlContent)
  
  planList <- xml_find_all(xmlContent, "//ns2:Plan")
  finalDT <- NULL
  
  for(i in planList){
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

processAPIResponse.SMGPlanQuoteResponse <- function(xmlResponse){
  xmlContent <- read_xml(xmlResponse)
  errorCheck(xmlContent)
  
  planList <- xml_find_all(xmlContent, "//ns2:Plan")
  finalDT <- NULL
  
  for(i in planList){
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

processAPIResponse.PlanDetailsResponse <- function(xmlResponse){
  xmlContent <- read_xml(xmlResponse)
  errorCheck(xmlContent)
  
  benefitList <- xml_find_all(xmlContent, "//ns2:PlanBenefit")
  finalDT <- NULL
  
  childNodes <- xml_children(benefitList)
  
  nodeProcessor <- function(nodeList, prefix = NULL){
    tempList <- list()
    listIdx <- 1
    nodeIdx <- 1
    listLength <- length(nodeList)
    while(nodeIdx <= listLength){
      curNode <- nodeList[nodeIdx]
      data <- NULL
      varName <- ifelse(!is.null(prefix), paste(prefix, xml_name(curNode)), xml_name(curNode))
      if(xml_length(curNode) == 0){
        varValue <- xml_text(curNode)
        names(varValue) <- varName
        data <- data.table(varValue)
        names(data) <- varName
      } else {
        data <- nodeProcessor(xml_children(curNode), varName)
      }
      tempList[[listIdx]] <- data
      listIdx <- listIdx + 1
      nodeIdx <- nodeIdx + 1
    }
    finalData <- do.call(cbind, tempList)
  }
  
  output <- nodeProcessor(childNodes)
}

# Male, 30 years old, non-smoker
defaultIFPRequest <- function(effDate, zip, county){
  # Input validation
  formattedDate <- as.POSIXct(effDate)
  if(is.na(formattedDate)){
    stop("Please provide correctly formatted date!")
  }
  
  zipCode <- as.character(zip)
  validZip <- grepl("^[[:digit:]]{5}", zipCode)
  if(!validZip){
    stop("Please provide correctly formatted zip code!")
  }
  
  countyName <- toupper(county)
  validCounty <- grepl("[-[:alpha:] ,'.]+", countyName)
  if(!validCounty){
    stop("Please provide a correctly formatted county!")
  }
  
  # Generate default enrollee
  defaultDOB <- formattedDate - (30 * years())
  defaultMbr <- enrollee$new(defaultDOB, "Male", NULL, "SELF", TRUE)
  mbrList <- list(defaultMbr)
  defaultRequest <- IFP_PlanQuote$new(mbrList, zipCode, countyName, formattedDate)
}

# Male, 30 years old, non-smoker
defaultSMGRequest <- function(effDate, zip, county){
  # Input validation
  formattedDate <- as.POSIXct(effDate)
  if(is.na(formattedDate)){
    stop("Please provide correctly formatted date!")
  }
  
  zipCode <- as.character(zip)
  validZip <- grepl("^[[:digit:]]{5}", zipCode)
  if(!validZip){
    stop("Please provide correctly formatted zip code!")
  }
  
  countyName <- toupper(county)
  validCounty <- grepl("[-[:alpha:] ,'.]+", countyName)
  if(!validCounty){
    stop("Please provide a correctly formatted county!")
  }
  
  # Generate default enrollee
  defaultDOB <- formattedDate - (30 * years())
  defaultMbr <- enrollee$new(defaultDOB, "Male", NULL, "SELF", TRUE)
  mbrList <- list(defaultMbr)
  defaultRequest <- SMG_PlanQuote$new(mbrList, zipCode, countyName, formattedDate)
}

# Male, 30 years old, non-smoker
defaultIFPDetailsRequest <- function(effDate, zip, county, planID){
  # Input validation
  formattedDate <- as.POSIXct(effDate)
  if(is.na(formattedDate)){
    stop("Please provide correctly formatted date!")
  }
  
  zipCode <- as.character(zip)
  validZip <- grepl("^[[:digit:]]{5}", zipCode)
  if(!validZip){
    stop("Please provide correctly formatted zip code!")
  }
  
  countyName <- toupper(county)
  validCounty <- grepl("[-[:alpha:] ,'.]+", countyName)
  if(!validCounty){
    stop("Please provide a correctly formatted county!")
  }
  
  # Generate default enrollee
  defaultDOB <- formattedDate - (30 * years())
  defaultMbr <- enrollee$new(defaultDOB, "Male", NULL, "SELF", TRUE)
  mbrList <- list(defaultMbr)
  defaultRequest <- IFP_PlanDetails$new(mbrList, zipCode, countyName, formattedDate, planID)
}

# Male, 30 years old, non-smoker
defaultSMGDetailsRequest <- function(effDate, zip, county, planID){
  # Input validation
  formattedDate <- as.POSIXct(effDate)
  if(is.na(formattedDate)){
    stop("Please provide correctly formatted date!")
  }
  
  zipCode <- as.character(zip)
  validZip <- grepl("^[[:digit:]]{5}", zipCode)
  if(!validZip){
    stop("Please provide correctly formatted zip code!")
  }
  
  countyName <- toupper(county)
  validCounty <- grepl("[-[:alpha:] ,'.]+", countyName)
  if(!validCounty){
    stop("Please provide a correctly formatted county!")
  }
  
  # Generate default enrollee
  defaultDOB <- formattedDate - (30 * years())
  defaultMbr <- enrollee$new(defaultDOB, "Male", NULL, "SELF", TRUE)
  mbrList <- list(defaultMbr)
  defaultRequest <- SMG_PlanDetails$new(mbrList, zipCode, countyName, formattedDate, planID)
}
