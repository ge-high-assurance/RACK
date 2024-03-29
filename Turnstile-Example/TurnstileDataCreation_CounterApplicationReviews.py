#!/usr/bin/env python3
#
# Copyright (c) 2021, General Electric Company, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from Logging import *
from Evidence import *
import Evidence.Add as Add 
import shutil
import os.path


def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True

    createEvidenceFile(ingestionTitle="TurnstileIngestion-SystemReview", ingestionDescription="Manual ingestion of Counter Application Reviews")

    #######################################################################
    #  HLR Review 1
    #######################################################################
    #-------------------------- 
    Add.GE.Engineer(identifier="259863025",
                title = "Public, John",
                emailAddress = "john.public@ge.com",
                employedBy_identifier = "General_Electric")
    Add.AGENTS.ORGANIZATION(identifier = "General_Electric")
    #-------------------------- 
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                author_identifier = "125569538",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "RQ-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "RQ-STD:v1")
    #--------------------------    
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "HLR-1:v1")
    Add.GE.HighLevelRequirement(identifier = "HLR-1:v1")
    #--------------------------     
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "HLR-2:v1")
    Add.GE.HighLevelRequirement(identifier = "HLR-2:v1")
    #--------------------------     
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "HLR-3:v1")
    Add.GE.HighLevelRequirement(identifier = "HLR-3:v1")
    #--------------------------                 
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "inflowEvent")
    Add.GE.DataDictionary(identifier = "inflowEvent")
    #--------------------------     
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "outflowEvent")
    Add.GE.DataDictionary(identifier = "outflowEvent")
    #-------------------------- 
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "counter")
    Add.GE.DataDictionary(identifier = "counter")
    #-------------------------- 
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "display")
    Add.GE.DataDictionary(identifier = "display")
    

    #-------------------------- 
    Add.GE.SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-1",
                reviews_identifier = "HLR-1:v1",
                reviewResult_identifier = "Revise With Review",
                wasGeneratedBy_identifier = "HlrReview")
    Add.GE.HighLevelRequirement(identifier = "HLR-1:v1")
    #-------------------------- 
    Add.GE.SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-2",
                reviews_identifier = "HLR-2:v1",
                reviewResult_identifier = "Revise With Review",
                wasGeneratedBy_identifier = "HlrReview")
    Add.GE.HighLevelRequirement(identifier = "HLR-2:v1")
    #-------------------------- 
    Add.GE.SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-3",
                reviews_identifier = "HLR-3:v1",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "HlrReview")
    Add.GE.HighLevelRequirement(identifier = "HLR-3:v1")

    #######################################################################
    #  HLR Review 2
    #######################################################################
    #-------------------------- 
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview-2",
                author_identifier = "125569538",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "RQ-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "RQ-STD:v1")
    #--------------------------                 
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview-2",
                reviewed_identifier = "HLR-1:v2")
    Add.GE.HighLevelRequirement(identifier = "HLR-1:v2")
    #--------------------------                 
    Add.GE.SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-4",
                reviews_identifier = "HLR-1:v2",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "HlrReview-2")
    Add.GE.HighLevelRequirement(identifier = "HLR-1:v2")
    #--------------------------     
    Add.GE.SoftwareRequirementsReview(identifier="HlrReview-2",
                reviewed_identifier = "HLR-2:v2")
    Add.GE.HighLevelRequirement(identifier = "HLR-2:v2")
    #--------------------------  
    Add.GE.SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-5",
                reviews_identifier = "HLR-2:v2",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "HlrReview-2")
    Add.GE.HighLevelRequirement(identifier = "HLR-1:v2")
    #######################################################################
    #  LLR Review 1
    #######################################################################
    #-------------------------- 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-1")
    #--------------------------                 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-2:v2")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-2:v2")
    #--------------------------                 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-3:v2")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-3:v2")
    #--------------------------                 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-4")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-4")
    #--------------------------                 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-5")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-5")
    #--------------------------                 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-6")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-6")
    #--------------------------                 
    Add.GE.SoftwareDesignReview(identifier="LlrReview1",
                author_identifier = "2125895152",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "SW-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "SW-STD:v1")

    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-1",
                reviews_identifier = "IN-LLR-1",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-1")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-2",
                reviews_identifier = "IN-LLR-2:v2",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-2:v2")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-3",
                reviews_identifier = "IN-LLR-3:v2",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-3:v2")  
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-4",
                reviews_identifier = "IN-LLR-4",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-4")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-5",
                reviews_identifier = "IN-LLR-5",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-5")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-6",
                reviews_identifier = "IN-LLR-6",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview1")
    Add.GE.LowLevelRequirement(identifier = "IN-LLR-6")
    #-------------------------- 
    Add.GE.SoftwareDesignReview(identifier="LlrReview2",
                reviewed_identifier = "OUT-LLR-1")
    Add.GE.LowLevelRequirement(identifier = "OUT-LLR-1")
    #-------------------------- 
    Add.GE.SoftwareDesignReview(identifier="LlrReview2",
                reviewed_identifier = "OUT-LLR-2:v2")
    Add.GE.LowLevelRequirement(identifier = "OUT-LLR-2:v2")
    #-------------------------- 
    Add.GE.SoftwareDesignReview(identifier="LlrReview2",
                author_identifier = "2125895152",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "SW-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "SW-STD:v1")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview2Log-1",
                reviews_identifier = "OUT-LLR-1",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview2")
    Add.GE.LowLevelRequirement(identifier = "OUT-LLR-1")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview2Log-2",
                reviews_identifier = "OUT-LLR-2:v2",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview2")
    Add.GE.LowLevelRequirement(identifier = "OUT-LLR-2:v2")
    #-------------------------- 
    Add.GE.SoftwareDesignReview(identifier="LlrReview3",
                reviewed_identifier = "EXE-LLR-1")
    Add.GE.LowLevelRequirement(identifier = "EXE-LLR-1")
    Add.GE.SoftwareDesignReview(identifier="LlrReview3",
                reviewed_identifier = "EXE-LLR-2")
    Add.GE.LowLevelRequirement(identifier = "EXE-LLR-2")
    Add.GE.SoftwareDesignReview(identifier="LlrReview3",
                reviewed_identifier = "EXE-LLR-3")
    Add.GE.LowLevelRequirement(identifier = "EXE-LLR-3")
    Add.GE.SoftwareDesignReview(identifier="LlrReview3",
                author_identifier = "2125895152",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "SW-STD:v1")
    Add.DOCUMENT.DOCUMENT(identifier = "SW-STD:v1")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview3Log-1",
                reviews_identifier = "EXE-LLR-1",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview3")
    Add.GE.LowLevelRequirement(identifier = "EXE-LLR-1")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview3Log-2",
                reviews_identifier = "EXE-LLR-2",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview3")
    Add.GE.LowLevelRequirement(identifier = "EXE-LLR-2")
    #-------------------------- 
    Add.GE.SoftwareDesignReviewArtifacts(identifier="LlrReview3Log-3",
                reviews_identifier = "EXE-LLR-3",
                reviewResult_identifier = "Passed",
                wasGeneratedBy_identifier = "LlrReview3")
    Add.GE.LowLevelRequirement(identifier = "EXE-LLR-3")

    createCDR("http://rack001/turnstiledata")


if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationReviews")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationReviews"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/CounterApplicationReviews"))
