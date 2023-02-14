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

from Evidence import createEvidenceFile, createCDR
import Evidence.Add as Add
import shutil
import os.path


def CreateCdrs():
    
    #Logging.TRACE = True
    #Logging.DEBUG = True

    createEvidenceFile(ingestionTitle="TurnstileIngestion-Dev Plan Data", ingestionDescription="Manual ingestion of Turnstile System Development Plan Data")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.Turnstile
    ################################################
    Add.SYSTEM.SYSTEM(identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.CounterApplication
    ################################################
    Add.GE.SystemComponent(identifier="CounterApplication",
                        partOf_identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.Display
    ################################################
    Add.GE.SystemComponent(identifier="Display",
                        partOf_identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.InGate
    ################################################
    Add.GE.SystemComponent(identifier="InGate",
                        partOf_identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.OutGate
    ################################################
    Add.GE.SystemComponent(identifier="OutGate",
                        partOf_identifier="Turnstile")


    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview
    ################################################

    Add.DOCUMENT.SECTION(identifier="_01-SystemOverview",
            content_identifier = "CounterApplication")
    Add.DOCUMENT.SECTION(identifier="_01-SystemOverview",
            content_identifier = "Display")
    Add.DOCUMENT.SECTION(identifier="_01-SystemOverview",
            content_identifier = "InGate")
    Add.DOCUMENT.SECTION(identifier="_01-SystemOverview",
            content_identifier = "OutGate")
    Add.DOCUMENT.SECTION(identifier="_01-SystemOverview",
            content_identifier = "Turnstile")

  
    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview.ExecutiveThread
    ################################################
    Add.GE.SoftwareThread(identifier="ExecutiveThread",
                       partOf_identifier="CounterApplication")


    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview.InputThread
    ################################################
    Add.GE.SoftwareThread(identifier="InputThread",                       
                       partOf_identifier="CounterApplication")


    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview.OutputThread
    ################################################
    Add.GE.SoftwareThread(identifier="OutputThread",                       
                       partOf_identifier="CounterApplication")


    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview
    ################################################

    Add.DOCUMENT.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "CounterApplication")
    Add.DOCUMENT.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "ExecutiveThread")
    Add.DOCUMENT.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "InputThread")
    Add.DOCUMENT.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "OutputThread")


    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.03-CertificationConsiderations
    ################################################

    Add.DOCUMENT.SECTION(identifier="_03-CertificationConsiderations")

    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle
    ################################################

    Add.DOCUMENT.SECTION(identifier="_04-SoftwareLifeCycle")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareConfigurationManagementProcess   
    ################################################

    Add.PROV_S.ACTIVITY(identifier="SoftwareConfigurationManagementProcess",
                wasInformedBy_identifier="SoftwareDevelopmentProcess")
    Add.PROV_S.ACTIVITY(identifier="SoftwareConfigurationManagementProcess",
                wasInformedBy_identifier="SoftwareVerificationProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.System Development Process
    ################################################

    Add.PROV_S.ACTIVITY(identifier="System_Development_Process",
                wasInformedBy_identifier="SoftwareConfigurationManagementProcess")
                
    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwarePlanningProcess
    ################################################

    Add.PROV_S.ACTIVITY(identifier="SoftwarePlanningProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.RequirementStandard
    ################################################

    Add.DOCUMENT.SPECIFICATION(identifier="RQ-STD",
                    wasGeneratedBy_identifier="SoftwarePlanningProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SoftwareStandard
    ################################################

    Add.DOCUMENT.SPECIFICATION(identifier="SW-STD",
                    wasGeneratedBy_identifier="SoftwarePlanningProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SystemSpec
    ################################################
    
    Add.DOCUMENT.SPECIFICATION(identifier="Sys-Spec",
                    wasGeneratedBy_identifier="System_Development_Process")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SystemVerificationReport
    ################################################
    
    Add.DOCUMENT.REPORT(identifier="Sys-Ver-Rep",
                    wasGeneratedBy_identifier="System_Development_Process")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareDevelopmentProcess
    ################################################

    Add.PROV_S.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                wasInformedBy_identifier="System_Development_Process")
    Add.PROV_S.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                wasInformedBy_identifier="SoftwarePlanningProcess")
    Add.PROV_S.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="RQ-STD")
    Add.PROV_S.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="SW-STD")
    Add.PROV_S.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="Sys-Spec")
    Add.PROV_S.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="Sys-Ver-Rep")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareQualityAssuranceProcess
    ################################################

    Add.PROV_S.ACTIVITY(identifier="SoftwareQualityAssuranceProcess",
                wasInformedBy_identifier="SoftwarePlanningProcess")
    Add.PROV_S.ACTIVITY(identifier="SoftwareQualityAssuranceProcess",
                wasInformedBy_identifier="SoftwareDevelopmentProcess")
    Add.PROV_S.ACTIVITY(identifier="SoftwareQualityAssuranceProcess",
                wasInformedBy_identifier="SoftwareVerficationProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationSoftware
    ################################################
    Add.PROV_S.COLLECTION(identifier="CounterApplicationSoftware",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationSourceCode
    ################################################
    Add.DOCUMENT.SPECIFICATION(identifier="SW-Code",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationSoftwareDesign
    ################################################
    Add.DOCUMENT.DESCRIPTION(identifier="Counter-SW-Des",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")
 

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationRequirementSpec
    ################################################
    Add.DOCUMENT.SPECIFICATION(identifier="Counter-Req-Spec",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")
                
                
    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareVerficationProcess
    ################################################

    Add.PROV_S.ACTIVITY(identifier="SoftwareVerificationProcess",
                wasInformedBy_identifier="SoftwareDevelopmentProcess",
                used_identifier="VER-STD")
    Add.PROV_S.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="CounterApplicationSoftware")
    Add.PROV_S.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="SW-Code")
    Add.PROV_S.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="Counter-SW-Des")
    Add.PROV_S.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="Counter-Req-Spec")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SoftwareEnvironmentConfigurationIndex
    ################################################
    Add.DOCUMENT.REPORT(identifier="SECI",
                wasGeneratedBy_identifier="SoftwareConfigurationManagementProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SoftwareVersionDescription
    ################################################
    Add.DOCUMENT.DESCRIPTION(identifier="SW-Ver-Des",
                wasGeneratedBy_identifier="SoftwareConfigurationManagementProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.VerificationStandard
    ################################################
    Add.DOCUMENT.SPECIFICATION(identifier="VER-STD",
                wasGeneratedBy_identifier="SoftwarePlanningProcess")
        
        
    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData
    ################################################
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Counter-Req-Spec")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="CounterApplicationSoftware")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Counter-SW-Des")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SW-Code")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="RQ-STD")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SECI")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SW-STD")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SW-Ver-Des")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Sys-Spec")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Sys-Ver-Rep")
    Add.DOCUMENT.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="VER-STD")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.06-Schedule
    ################################################
    Add.DOCUMENT.SECTION(identifier="_06-Schedule")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.07-AdditionalConsiderations
    ################################################
    Add.DOCUMENT.SECTION(identifier="_07-AdditionalConsiderations")


    ################################################
    #        Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.08-SupplierOversite
    ################################################
    Add.DOCUMENT.SECTION(identifier="_08-SupplierOversite")


    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification
    ################################################

    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_01-SystemOverview")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_02-SoftwareOverview")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_03-CertificationConsiderations")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_04-SoftwareLifeCycle")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_05-SoftwareLifeCycleData")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_06-Schedule")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_07-AdditionalConsiderations")
    Add.DOCUMENT.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_08-SupplierOversite")

  
    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement.01-SystemRequirements
    ################################################
    Add.DOCUMENT.SECTION(identifier="_01-SystemRequirements")
     
            
    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement.02-SystemDesign
    ################################################
    Add.DOCUMENT.SECTION(identifier="_02-SystemDesign")
     

    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement.03-HazardAssesment
    ################################################
    Add.DOCUMENT.SECTION(identifier="_03-HazardAssesment")
     

    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement
    ################################################
    Add.DOCUMENT.PLAN(identifier="_02-SystemDevelopement",
            content_identifier="_01-SystemRequirements")
    Add.DOCUMENT.PLAN(identifier="_02-SystemDevelopement",
            content_identifier="_02-SystemDesign")
    Add.DOCUMENT.PLAN(identifier="_02-SystemDevelopement",
            content_identifier="_03-HazardAssesment")
     

    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment.01-Standards
    ################################################
    Add.DOCUMENT.SECTION(identifier="_01-Standards")


    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment.02-SoftwareDevelopmentLifeCycle
    ################################################
    Add.DOCUMENT.SECTION(identifier="_02-SoftwareDevelopmentLifeCycle")


    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment.03-SoftwareDevelopmentEnvironment
    ################################################
    Add.DOCUMENT.SECTION(identifier="_03-SoftwareDevelopmentEnvironment")

    
    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment
    ################################################
    Add.DOCUMENT.PLAN(identifier="_03-SoftwareDevelopment",
            content_identifier="_01-Standards")
    Add.DOCUMENT.PLAN(identifier="_03-SoftwareDevelopment",
            content_identifier="_02-SoftwareDevelopmentLifeCycle")
    Add.DOCUMENT.PLAN(identifier="_03-SoftwareDevelopment",
            content_identifier="_03-SoftwareDevelopmentEnvironment")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.01-SoftwareReviewProcess
    ################################################
    Add.DOCUMENT.SECTION(identifier="_01-SoftwareReviewProcess")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.02-SoftwareTestGenerationProcess
    ################################################
    Add.DOCUMENT.SECTION(identifier="_02-SoftwareTestGenerationProcess")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.04-SoftwareAnalysis
    ################################################
    Add.DOCUMENT.SECTION(identifier="_04-SoftwareAnalysis")
    
    
    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.03-SoftwareTestExecutionProcess
    ################################################
    Add.DOCUMENT.SECTION(identifier="_03-SoftwareTestExecutionProcess")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification
    ################################################
    Add.DOCUMENT.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_01-SoftwareReviewProcess")
    Add.DOCUMENT.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_02-SoftwareTestGenerationProcess")
    Add.DOCUMENT.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_04-SoftwareAnalysis")
    Add.DOCUMENT.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_03-SoftwareTestExecutionProcess")


    ################################################
    #        Model Location: DevelopmentPlan.05-SoftwareConfigurationManagment.01-ChangeManagement
    ################################################
    Add.DOCUMENT.SECTION(identifier="_01-ChangeManagement")
    

    ################################################
    #        Model Location: DevelopmentPlan.05-SoftwareConfigurationManagment.02-SoftwareConfigurationManagementEnvironment
    ################################################
    Add.DOCUMENT.SECTION(identifier="_02-SoftwareConfigurationManagementEnvironment")

    
    ################################################
    #        Model Location: DevelopmentPlan.05-SoftwareConfigurationManagment
    ################################################
    Add.DOCUMENT.PLAN(identifier="_05-SoftwareConfigurationManagment",
            content_identifier="_01-ChangeManagement")
    Add.DOCUMENT.PLAN(identifier="_05-SoftwareConfigurationManagment",
            content_identifier="_02-SoftwareConfigurationManagementEnvironment")


    ################################################
    #        Model Location: DevelopmentPlan.06-SoftwareQualityAssurance
    ################################################
    Add.DOCUMENT.PLAN(identifier="_06-SoftwareQualityAssurance")


    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareCodeReviewChecklist
    ################################################
    Add.DOCUMENT.PLAN(identifier="SoftwareCodeReviewChecklist",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareDesignReviewChecklist
    ################################################
    Add.DOCUMENT.PLAN(identifier="SoftwareDesignReviewChecklist",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareRequirementReviewChecklist
    ################################################
    Add.DOCUMENT.PLAN(identifier="SoftwareRequirementReviewChecklist",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareTestGuidance
    ################################################
    Add.DOCUMENT.PLAN(identifier="SoftwareTestGuidance",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards
    ################################################
    Add.DOCUMENT.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareCodeReviewChecklist")
    Add.DOCUMENT.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareDesignReviewChecklist")
    Add.DOCUMENT.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareRequirementReviewChecklist")
    Add.DOCUMENT.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareTestGuidance")


    ################################################
    #       //Model Location: DevelopmentPlan
    ################################################
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_01-PlanForSoftwareAspectsOfCertification")
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_02-SystemDevelopement")
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_03-SoftwareDevelopment")
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_04-SoftwareVerification")
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_05-SoftwareConfigurationManagment")
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_06-SoftwareQualityAssurance")
    Add.DOCUMENT.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_07-SoftwareStandards")

            
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileDevelopmentPlanData"))

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileDevelopmentPlanData")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileDevelopmentPlanData"))
    CreateCdrs()
    
