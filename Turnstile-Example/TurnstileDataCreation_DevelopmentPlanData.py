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
    Add.SYSTEM(identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.CounterApplication
    ################################################
    Add.turnstile_SystemComponent(identifier="CounterApplication",
                        partOf_identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.Display
    ################################################
    Add.turnstile_SystemComponent(identifier="Display",
                        partOf_identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.InGate
    ################################################
    Add.turnstile_SystemComponent(identifier="InGate",
                        partOf_identifier="Turnstile")

    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview.OutGate
    ################################################
    Add.turnstile_SystemComponent(identifier="OutGate",
                        partOf_identifier="Turnstile")


    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.01-SystemOverview
    ################################################

    Add.SECTION(identifier="_01-SystemOverview",
            content_identifier = "CounterApplication")
    Add.SECTION(identifier="_01-SystemOverview",
            content_identifier = "Display")
    Add.SECTION(identifier="_01-SystemOverview",
            content_identifier = "InGate")
    Add.SECTION(identifier="_01-SystemOverview",
            content_identifier = "OutGate")
    Add.SECTION(identifier="_01-SystemOverview",
            content_identifier = "Turnstile")

  
    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview.ExecutiveThread
    ################################################
    Add.turnstile_SoftwareThread(identifier="ExecutiveThread",
                       partOf_identifier="CounterApplication")


    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview.InputThread
    ################################################
    Add.turnstile_SoftwareThread(identifier="InputThread",                       
                       partOf_identifier="CounterApplication")


    ################################################
    #       Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview.OutputThread
    ################################################
    Add.turnstile_SoftwareThread(identifier="OutputThread",                       
                       partOf_identifier="CounterApplication")


    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.02-SoftwareOverview
    ################################################

    Add.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "CounterApplication")
    Add.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "ExecutiveThread")
    Add.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "InputThread")
    Add.SECTION(identifier="_02-SoftwareOverview",
            content_identifier = "OutputThread")


    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.03-CertificationConsiderations
    ################################################

    Add.SECTION(identifier="_03-CertificationConsiderations")

    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle
    ################################################

    Add.SECTION(identifier="_04-SoftwareLifeCycle")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareConfigurationManagementProcess   
    ################################################

    Add.ACTIVITY(identifier="SoftwareConfigurationManagementProcess",
                wasInformedBy_identifier="SoftwareDevelopmentProcess")
    Add.ACTIVITY(identifier="SoftwareConfigurationManagementProcess",
                wasInformedBy_identifier="SoftwareVerificationProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.System Development Process
    ################################################

    Add.ACTIVITY(identifier="System_Development_Process",
                wasInformedBy_identifier="SoftwareConfigurationManagementProcess")
                
    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwarePlanningProcess
    ################################################

    Add.ACTIVITY(identifier="SoftwarePlanningProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.RequirementStandard
    ################################################

    Add.SPECIFICATION(identifier="RQ-STD",
                    wasGeneratedBy_identifier="SoftwarePlanningProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SoftwareStandard
    ################################################

    Add.SPECIFICATION(identifier="SW-STD",
                    wasGeneratedBy_identifier="SoftwarePlanningProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SystemSpec
    ################################################
    
    Add.SPECIFICATION(identifier="Sys-Spec",
                    wasGeneratedBy_identifier="System_Development_Process")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SystemVerificationReport
    ################################################
    
    Add.REPORT(identifier="Sys-Ver-Rep",
                    wasGeneratedBy_identifier="System_Development_Process")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareDevelopmentProcess
    ################################################

    Add.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                wasInformedBy_identifier="System_Development_Process")
    Add.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                wasInformedBy_identifier="SoftwarePlanningProcess")
    Add.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="RQ-STD")
    Add.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="SW-STD")
    Add.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="Sys-Spec")
    Add.ACTIVITY(identifier="SoftwareDevelopmentProcess",
                used_identifier="Sys-Ver-Rep")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareQualityAssuranceProcess
    ################################################

    Add.ACTIVITY(identifier="SoftwareQualityAssuranceProcess",
                wasInformedBy_identifier="SoftwarePlanningProcess")
    Add.ACTIVITY(identifier="SoftwareQualityAssuranceProcess",
                wasInformedBy_identifier="SoftwareDevelopmentProcess")
    Add.ACTIVITY(identifier="SoftwareQualityAssuranceProcess",
                wasInformedBy_identifier="SoftwareVerficationProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationSoftware
    ################################################
    Add.COLLECTION(identifier="CounterApplicationSoftware",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationSourceCode
    ################################################
    Add.SPECIFICATION(identifier="SW-Code",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationSoftwareDesign
    ################################################
    Add.DESCRIPTION(identifier="Counter-SW-Des",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")
 

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.CounterApplicationRequirementSpec
    ################################################
    Add.SPECIFICATION(identifier="Counter-Req-Spec",
                wasGeneratedBy_identifier="SoftwareDevelopmentProcess")
                
                
    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.04-SoftwareLifeCycle.SoftwareVerficationProcess
    ################################################

    Add.ACTIVITY(identifier="SoftwareVerificationProcess",
                wasInformedBy_identifier="SoftwareDevelopmentProcess",
                used_identifier="VER-STD")
    Add.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="CounterApplicationSoftware")
    Add.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="SW-Code")
    Add.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="Counter-SW-Des")
    Add.ACTIVITY(identifier="SoftwareVerficationProcess",
                used_identifier="Counter-Req-Spec")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SoftwareEnvironmentConfigurationIndex
    ################################################
    Add.REPORT(identifier="SECI",
                wasGeneratedBy_identifier="SoftwareConfigurationManagementProcess")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.SoftwareVersionDescription
    ################################################
    Add.DESCRIPTION(identifier="SW-Ver-Des",
                wasGeneratedBy_identifier="SoftwareConfigurationManagementProcess")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData.VerificationStandard
    ################################################
    Add.SPECIFICATION(identifier="VER-STD",
                wasGeneratedBy_identifier="SoftwarePlanningProcess")
        
        
    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.05-SoftwareLifeCycleData
    ################################################
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Counter-Req-Spec")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="CounterApplicationSoftware")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Counter-SW-Des")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SW-Code")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="RQ-STD")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SECI")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SW-STD")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="SW-Ver-Des")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Sys-Spec")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="Sys-Ver-Rep")
    Add.SECTION(identifier="_05-SoftwareLifeCycleData",
                content_identifier="VER-STD")

    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.06-Schedule
    ################################################
    Add.SECTION(identifier="_06-Schedule")


    ################################################
    #        //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.07-AdditionalConsiderations
    ################################################
    Add.SECTION(identifier="_07-AdditionalConsiderations")


    ################################################
    #        Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification.08-SupplierOversite
    ################################################
    Add.SECTION(identifier="_08-SupplierOversite")


    ################################################
    #       //Model Location: DevelopmentPlan.01-PlanForSoftwareAspectsOfCertification
    ################################################

    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_01-SystemOverview")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_02-SoftwareOverview")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_03-CertificationConsiderations")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_04-SoftwareLifeCycle")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_05-SoftwareLifeCycleData")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_06-Schedule")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_07-AdditionalConsiderations")
    Add.PLAN(identifier="_01-PlanForSoftwareAspectsOfCertification",
            content_identifier = "_08-SupplierOversite")

  
    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement.01-SystemRequirements
    ################################################
    Add.SECTION(identifier="_01-SystemRequirements")
     
            
    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement.02-SystemDesign
    ################################################
    Add.SECTION(identifier="_02-SystemDesign")
     

    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement.03-HazardAssesment
    ################################################
    Add.SECTION(identifier="_03-HazardAssesment")
     

    ################################################
    #        Model Location: DevelopmentPlan.02-SystemDevelopement
    ################################################
    Add.PLAN(identifier="_02-SystemDevelopement",
            content_identifier="_01-SystemRequirements")
    Add.PLAN(identifier="_02-SystemDevelopement",
            content_identifier="_02-SystemDesign")
    Add.PLAN(identifier="_02-SystemDevelopement",
            content_identifier="_03-HazardAssesment")
     

    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment.01-Standards
    ################################################
    Add.SECTION(identifier="_01-Standards")


    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment.02-SoftwareDevelopmentLifeCycle
    ################################################
    Add.SECTION(identifier="_02-SoftwareDevelopmentLifeCycle")


    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment.03-SoftwareDevelopmentEnvironment
    ################################################
    Add.SECTION(identifier="_03-SoftwareDevelopmentEnvironment")

    
    ################################################
    #        Model Location: DevelopmentPlan.03-SoftwareDevelopment
    ################################################
    Add.PLAN(identifier="_03-SoftwareDevelopment",
            content_identifier="_01-Standards")
    Add.PLAN(identifier="_03-SoftwareDevelopment",
            content_identifier="_02-SoftwareDevelopmentLifeCycle")
    Add.PLAN(identifier="_03-SoftwareDevelopment",
            content_identifier="_03-SoftwareDevelopmentEnvironment")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.01-SoftwareReviewProcess
    ################################################
    Add.SECTION(identifier="_01-SoftwareReviewProcess")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.02-SoftwareTestGenerationProcess
    ################################################
    Add.SECTION(identifier="_02-SoftwareTestGenerationProcess")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.04-SoftwareAnalysis
    ################################################
    Add.SECTION(identifier="_04-SoftwareAnalysis")
    
    
    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification.03-SoftwareTestExecutionProcess
    ################################################
    Add.SECTION(identifier="_03-SoftwareTestExecutionProcess")


    ################################################
    #        Model Location: DevelopmentPlan.04-SoftwareVerification
    ################################################
    Add.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_01-SoftwareReviewProcess")
    Add.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_02-SoftwareTestGenerationProcess")
    Add.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_04-SoftwareAnalysis")
    Add.PLAN(identifier="_04-SoftwareVerification",
            content_identifier="_03-SoftwareTestExecutionProcess")


    ################################################
    #        Model Location: DevelopmentPlan.05-SoftwareConfigurationManagment.01-ChangeManagement
    ################################################
    Add.SECTION(identifier="_01-ChangeManagement")
    

    ################################################
    #        Model Location: DevelopmentPlan.05-SoftwareConfigurationManagment.02-SoftwareConfigurationManagementEnvironment
    ################################################
    Add.SECTION(identifier="_02-SoftwareConfigurationManagementEnvironment")

    
    ################################################
    #        Model Location: DevelopmentPlan.05-SoftwareConfigurationManagment
    ################################################
    Add.PLAN(identifier="_05-SoftwareConfigurationManagment",
            content_identifier="_01-ChangeManagement")
    Add.PLAN(identifier="_05-SoftwareConfigurationManagment",
            content_identifier="_02-SoftwareConfigurationManagementEnvironment")


    ################################################
    #        Model Location: DevelopmentPlan.06-SoftwareQualityAssurance
    ################################################
    Add.PLAN(identifier="_06-SoftwareQualityAssurance")


    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareCodeReviewChecklist
    ################################################
    Add.PLAN(identifier="SoftwareCodeReviewChecklist",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareDesignReviewChecklist
    ################################################
    Add.PLAN(identifier="SoftwareDesignReviewChecklist",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareRequirementReviewChecklist
    ################################################
    Add.PLAN(identifier="SoftwareRequirementReviewChecklist",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards.SoftwareTestGuidance
    ################################################
    Add.PLAN(identifier="SoftwareTestGuidance",
            references_identifier="VER-STD")

    ################################################
    #        Model Location: DevelopmentPlan.07-SoftwareStandards
    ################################################
    Add.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareCodeReviewChecklist")
    Add.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareDesignReviewChecklist")
    Add.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareRequirementReviewChecklist")
    Add.PLAN(identifier="_07-SoftwareStandards",
            content_identifier="SoftwareTestGuidance")


    ################################################
    #       //Model Location: DevelopmentPlan
    ################################################
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_01-PlanForSoftwareAspectsOfCertification")
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_02-SystemDevelopement")
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_03-SoftwareDevelopment")
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_04-SoftwareVerification")
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_05-SoftwareConfigurationManagment")
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_06-SoftwareQualityAssurance")
    Add.PLAN(identifier="_DevelopmentPlan",
            content_identifier = "_07-SoftwareStandards")

            
    createCDR("http://rack001/turnstiledata")
    os.rename(os.path.join(".","RACK-DATA"), os.path.join(".","Turnstile-IngestionPackage/TurnstileDevelopmentPlanData"))

if __name__=="__main__":
    if os.path.exists(os.path.join(".","Turnstile-IngestionPackage/TurnstileDevelopmentPlanData")):
        shutil.rmtree(os.path.join(".","Turnstile-IngestionPackage/TurnstileDevelopmentPlanData"))
    CreateCdrs()
    
