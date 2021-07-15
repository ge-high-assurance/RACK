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

import XML
import XML.SysML as SysML
from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add
import shutil
import os.path


def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True

    createEvidenceFile(ingestionTitle="TurnstileIngestion-SystemReview", ingestionDescription="Manual ingestion of Counter Application Reviews")

    Add.ORGANIZATION(identifier="GE")
 
    Add.turnstile_Engineer(identifier="259863025",
                title = "Public, John",
                emailAddress = "john.public@ge.com",
                employedBy_identifier = "GE")
    
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "HLR-1")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "HLR-2")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "HLR-3")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "inflowEvent")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "outflowEvent")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "counter")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                reviewed_identifier = "display")
    Add.turnstile_SoftwareRequirementsReview(identifier="HlrReview",
                author_identifier = "125569538",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "RQ-STD")

    Add.turnstile_SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-1",
                reviews_identifier = "HLR-1",
                reviewResult_identifier = "Passed",
                createBy_identifier = "HlrReview")

    Add.turnstile_SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-2",
                reviews_identifier = "HLR-2",
                reviewResult_identifier = "Passed",
                createBy_identifier = "HlrReview")

    Add.turnstile_SoftwareRequirementReviewArtifacts(identifier="HlrReviewLog-3",
                reviews_identifier = "HLR-3",
                reviewResult_identifier = "ReviseWithoutReview",
                createBy_identifier = "HlrReview")

    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-1")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-2")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-3")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-4")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-5")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                reviewed_identifier = "IN-LLR-6")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview1",
                author_identifier = "2125895152",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "SW-STD")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-1",
                reviews_identifier = "IN-LLR-1",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview1")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-2",
                reviews_identifier = "IN-LLR-2",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview1")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-3",
                reviews_identifier = "IN-LLR-3",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview1")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-4",
                reviews_identifier = "IN-LLR-4",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview1")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-5",
                reviews_identifier = "IN-LLR-5",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview1")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview1Log-6",
                reviews_identifier = "IN-LLR-6",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview1")

    Add.turnstile_SoftwareDesignReview(identifier="LlrReview2",
                reviewed_identifier = "OUT-LLR-1")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview2",
                reviewed_identifier = "OUT-LLR-2")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview2",
                author_identifier = "2125895152",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "SW-STD")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview2Log-1",
                reviews_identifier = "OUT-LLR-1",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview2")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview2Log-2",
                reviews_identifier = "OUT-LLR-2",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview2")

    Add.turnstile_SoftwareDesignReview(identifier="LlrReview3",
                reviewed_identifier = "EXE-LLR-1")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview3",
                reviewed_identifier = "EXE-LLR-2")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview3",
                reviewed_identifier = "EXE-LLR-3")
    Add.turnstile_SoftwareDesignReview(identifier="LlrReview3",
                author_identifier = "2125895152",
                reviewer_identifier =  "259863025",
                governedBy_identifier = "SW-STD")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview3Log-1",
                reviews_identifier = "EXE-LLR-1",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview3")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview3Log-2",
                reviews_identifier = "EXE-LLR-2",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview3")

    Add.turnstile_SoftwareDesignReviewArtifacts(identifier="LlrReview3Log-3",
                reviews_identifier = "EXE-LLR-3",
                reviewResult_identifier = "Passed",
                createBy_identifier = "LlrReview3")
    createCDR()


if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationReviews")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/CounterApplicationReviews"))
    CreateCdrs()
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/CounterApplicationReviews"))
