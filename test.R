defaultDOB <- ymd(20180101) - (30 * years())
defaultMbr <- enrollee$new(defaultDOB, "Male", NULL, "SELF", TRUE)
mbrList <- list(defaultMbr)
detailRequest <- IFP_PlanDetails$new(mbrList, "32003", "CLAY", ymd(20180101), "16842FL0120044")
detailResponse <- HealthcareAPIRequest(detailRequest)
detailResults <- processAPIResponse(detailResponse)

quoteRequest <- IFP_PlanQuote$new(mbrList, "32605", "ALACHUA", ymd("20180101"))

detailRequest2 <- IFP_PlanDetails$new(mbrList, "32003", "CLAY", ymd(20170801), "30252FL0020076")
detailResponse2 <- HealthcareAPIRequest(detailRequest2)
detailResults2 <- processAPIResponse(detailResponse2)

detailRequest3 <- IFP_PlanDetails$new(mbrList, "32003", "CLAY", ymd(20170801), "19898FL0230058")
detailResponse3 <- HealthcareAPIRequest(detailRequest3)
detailResults3 <- processAPIResponse(detailResponse3)

detailRequest4 <- IFP_PlanDetails$new(mbrList, "32003", "CLAY", ymd(20170801), "19898FL0230054")
detailResponse4 <- HealthcareAPIRequest(detailRequest4)
detailResults4 <- processAPIResponse(detailResponse4)

detailRequest5 <- defaultIFPDetailsRequest(ymd(20170801),  "32003", "CLAY", "19898FL0230054")
detailResponse5 <- HealthcareAPIRequest(detailRequest5)
detailResults5 <- processAPIResponse(detailResponse5)

testFunction <- function(enrollees){
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
}
