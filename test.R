defaultDOB <- ymd(20170801) - (30 * years())
defaultMbr <- enrollee$new(defaultDOB, "Male", NULL, "SELF", TRUE)
mbrList <- list(defaultMbr)
detailRequest <- IFP_PlanDetails$new(mbrList, "32003", "CLAY", ymd(20170801), "16842FL0120044")
detailResponse <- HealthcareAPIRequest(detailRequest)
detailResults <- processAPIResponse(detailResponse)

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