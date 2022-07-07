#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")
echo "$BASEDIR"

rack nodegroups import . 

# Software Requirement Development Objectives (A-2-2, A-2-3)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareReqDev_Objectives" --file softwarereqdevobj.csv --format csv 

# Software Coding Objectives (A-2-6)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareCoding_Objectives" --file softwarecodingobj.csv --format csv 
	
# Software Design Objectives (A-2-4, A-2-5)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareDesign_Objectives" --file softwaredesignobj.csv --format csv 
	
# Software Integration Objectives (A-2-7)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareIntegration_Objectives" --file softwareintegrationobj.csv --format csv 

# Software Code Review Objectives (A-5-1 to A-5-9)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareCodeReview_Objectives" --file softwarecodereviewobj.csv --format csv 

# Software Design Review Objectives (A-4-1 to A-4-13)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareDesignReview_Objectives" --file softwaredesignreviewobj.csv --format csv 

# Software Requirements Review Objectives (A-3-1 to A-3-9)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSoftwareReqReview_Objectives" --file softwarereqreviewobj.csv --format csv 

# Develop Component and Unit Tests Objectives (A-7-1)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forDevelopTests_Objectives" --file developtestsobj.csv --format csv 

# Develop Data and Control Coupling Analysis Objectives (A-7-8)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forCouplingAnalysis_Objectives" --file couplinganalysisobj.csv --format csv 

# Develop Structural Coverage Analysis Objectives (A-7-5, A-7-6, A-7-7)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forStructuralCoverageAnalysis_Objectives" --file structuralcoverageobj.csv --format csv

# Develop Software Component Test Execuation Objectives (A-6-1, A-6-2, A-6-5)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSWCompTestExe_Objectives_A6" --file swcomptestexeobj_A6.csv --format csv

# Develop Software Component Test Execuation Objectives (A-7-2, A-7-3)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSWCompTestExe_Objectives_A7" --file swcomptestexeobj_A7.csv --format csv

# Develop Software Unit Test Execuation Objectives (A-6-3, A-6-4)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSWUnitTestExe_Objectives_A6" --file swunittestexeobj_A6.csv --format csv

# Develop Software Unit Test Execuation Objectives (A-7-2, A-7-4)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forSWUnitTestExe_Objectives_A7" --file swunittestexeobj_A7.csv --format csv

# Develop Problem Reporting Objectives (A-7-2)
rack data export \
	--data-graph http://rack001/turnstiledata \
	--data-graph http://rack001/do-178c \
	"query_forProblemReporting_Objectives" --file problemreportingobj.csv --format csv
	
