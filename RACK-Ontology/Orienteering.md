The core RACK ontology defines a set of general relationships that is intended to
provide a framework for the organization and representation of data collected for
the system.  In many areas, this core ontology is very generic to allow for
different program configurations and methodologies; it is expected that
program-specific overlays will be developed to further refine and constrain the
relationships described here.

This document provides general perspective on how this core ontology is
establishing the relationship frameworks and constraints on those relationships.
This can be used from the perspective of asking particular questions about the
data in RACK and formulating retrieval.

# Terms

Terminology used throughout this document:

  * Program: a development effort for a particular goal, such as "creating the
    software to install on specific drone hardware to allow for a specific use".

  * Target: the output executable software generated via a Program's efforts that
    will be installed.

  * Thing Tested: each test should be identified with a particular element of the
    Program (e.g. requirement, software unit, etc.) and there should also be
    traceability of the test to the particular (sub-)set of code being tested.

  * Area of Concern: A program is often comprised of a number of inter-related
    activities (areas of concern), including (but not limited to):

      * Requirements Development
      * System Architecture
      * Software Design
      * Target Code Development
      * Build and Deploy Operational Processes
      * Testing Development
      * Evaluation and Certification


# Traceability

Each of the areas of concern has a set of established practices and
state-of-the-art tools and techniques, but often the relationships *between* the
different activities is manually established through loosely coupled mechanisms
like documentation and other out-of-band communications.

One of the goals of RACK is to establish *traceability*, which is an explicit
identification of the relationships between these areas to allow one to trace the
information from one area through to the other areas.  This traceability helps to
ensure that each activity is sufficient and complete relative to the surrounding
areas of concern.

# Testing

Many programs that will be described by RACK data describe programs which use
"requirements-based testing", which is a methodology of developing tests based on
the *requirements* documentation (as opposed to, for example, coverage testing,
which is based on the source code).  However, the core RACK ontology should be
developed to support many different types of testing approaches, so the core
ontology is defined with a level of generality that should allow sufficient
flexibility to capture a particular testing approach in a program-specific
overlay.

## Testing Concerns

To this end, the RACK ontology should support the following relationships and/or
requests:

  * TC1: The ontology should support multiple different types of tests, even
    within the same program.

  * TC2: There should be a heirarchical arrangement of testing implementation,
    where

      * 1: the lowest level captures a single element of functionality in the
           target and has a disposition indicating whether that particular
           functional element has passed or failed
      * 2: the tests can be grouped by higher level concerns (see below)

  * TC3: The same set of target code may have multiple tests that verify the code
    with different inputs or different operational configurations.

  * TC4: Each test should have a relationship to the inputs and surrounding
    configuration to distinguish it from other tests.

  * TC5: Tests may have a sequential ordering constraint that requires previous
    tests to be run prior to running a particular test due to a configuration
    established by a previous test or to validate the transition of the target
    from one state to the next.  This ordering is also normal important because the
    re-certification of a modified target may require re-running this entire set
    of related tests if *any* of them tests the modified portion of the target.

  * TC6: Tests may be independent of each other: they may be run without running
    other independent sets and the results are sufficient and valid.  This is
    important for re-certification because independent tests that verify a
    non-modified portion of the target do not need to be re-executed.

    1. At the top-level, test collections will tend to be independent, and only
       at the bottom-most granular level will the sequential relations between
       tests be expressible, therefore TC5 must be contained within TC6 rather
       than the other way around.

  * TC7: Test failures may be resolved by a number of methods:

      * 1: testing a new version of the target
      * 2: running a new version of the tests
      * 3: user-provided documentation justifying discarding the test failure

    Note that change (1) may be associated with corresponding changes in other
    areas of concern (e.g. an updated version of the requirements document,
    indicating expectation of different target results than the previous version
    specified).

  * TC8: Tests may be run multiple times.  These test runs are subject to the
    following considerations:

     * 1 - Idempotence: It is expected (from the RACK perspective) that tests are
       deterministic: re-running the same tests on the same target yield the same
       results.  If tests are non-deterministic, this should be encapsulated in
       the test itself; loading a test result in RACK should be a conclusive
       statement about that test's disposition relative to a specific target.

     * 2 - Target sensitivity: Running tests against a specific version of an
       target provides *no* information about the results that would be obtained
       by running the same set of tests against a different version of the
       target.

     * 3 - Velocity: test results obtained from successive test runs on updated
       target versions may have more or fewer test failures over the aggregate
       run.  This relative change between target versions is referred to as the
       test "velocity" (aka, the first order derivative) where a positive
       velocity indicates a larger quantity of test success results and a general
       improvement in the status of the target.  [Velocity is perhaps the least
       important aspect for consideration in RACK.]

     * 4 - Subset testing: it may be possible that a test run consists of only a
       subset of the tests.  This is actually a desireable situation in going
       forward with utilizing ARCOS: targeted re-certification should need to
       re-verify the smallest reasonable portion of the target.

    Each instance of testing should be encapsulated in a manner that it can be
    distinguished from other instances, which supports evaluation under the
    considerations listed above.

  * TC9: Test development itself is an activity, independent of running tests,
    and tests are built artifacts that will change over time.

  * TC10: Tests will verify something.  That something could be a requirement, a
    source code function, etc.  There should be an association between the test
    and an element in another area of concern; tests which do not validate an
    element are irrelevant for the purposes of RACK.

  * TC11: Regardless of the specific thing that a test is verifying, a Test is
    run against a specific system or group of systems to validate the behavior of
    that system, where a system is the primary architectural component division
    within the target.  It should be possible to enumerate all of the tests that
    exist for a specific system to determine the totality and applicability of
    groups of tests; an executable is usually comprised of several systems, and
    different systems may have different executables, thus TC6 and TC10 are
    insufficient to fully capture this relationship.


## Testing Ontology Elements

The following identifies the primary elements in the TESTING ontology and the key
properties that relate to the concerns described above.  The actual definition in
the TESTING.sadl file provides a more complete specification of TESTING elements.

  * TEST : the lowest-level element, an ENTITY representing a single verification
    of the smallest portion of functionality. [TC2.1]

      * "verifies" property : the link to the ENTITY the test verifies [TC10]

      * If a test is modified [TC7.2] it must be declared as a new TEST

  * TEST_RESULT : The results of a TEST (via the "confirms" property), encoded as
    TEST_STATUS, which indicates the disposition of running the TEST on the target
    executable. [TC2.1]

  * TEST_STEP : This identifies the ordered sequential relationship between a
    specific TEST and any subsequent TEST_STEPs that it must preceed. [TC5]

      * "thisStep" property : identifies the TEST performed during this step

      * "nextStep" property : 0 or more values of TEST_STEP

  * TEST_PROCEDURE : this COLLECTION identifies a group of independent tests [TC6]

      * "targetSystem" property : identifies 1 or more SYSTEM.SYSTEM entities
        this test is designed to test (if multiple, implies that this is testing
        the interaction/communications between those systems.  [TC11]

  * TEST_RECORD : this ENTITY identifies the configuration under which a set of
    tests was run

      * "testRecordProcedure property : identifies the TEST_PROCEDURE this is a
        record for

      * "testRecordSteps" property : identifies the _initial_ TEST_STEP for a
        sequence of tests that was run.  These must be the starting TEST_STEP
        within a sequence (i.e. these must be the direct members of the
        testRecordProcedure).  If any entry is not a member of the
        testRecordProcedure, the TEST_RECORD is _incomplete_ and the results
        should not be utilized.

        The TEST_RECORD may identify a subset of the TEST_STEP entries for a
        TEST_PROCEDURE as long as they are top-level TEST_STEPS entries. [TC8.4]

      * "testRecordScenario" property : identifies the TEST_SCENARIO under which
        this TEST_RECORD was generated.

      * "testConfiguration" property : identifies an ENTITY that describes the
        configuration information under which the test was run. [this is largely
        TBD at this time]

  * TEST_SCENARIO : this ENTITY defines the target that the TEST_EXECUTION was
    run against and for which the the TEST_RECORD was generated. [TC3]

     * "targetPackage" property : the FILE.FILE entity that was tested [TC8.2]
     * "targetVersion" property : a STRING providing the VERSION of the
       targetPackage that was tested.  [TC8.2]
     * "testPackage" property : the FILE.FILE entity that executed the tests [TC7.2] [TC9]
     * "testVersion" property : a STRING providing the VERSION of the testPackage that was executed. [TC9]

  * TEST_LOG : a COLLECTION of TEST_RECORD that were generated during a specific
    TEST_EXECUTION.  [TC8.3]

  * TEST_EXECUTION : an ACTIVITY of running some or all of a TEST_PROCEDURE and
    generating a TEST_LOG.

      * "testProcedure" property : the TEST_PROCEDURE
      * "testLog" property : the TEST_LOG generated by the TEST_EXECUTION

  * TEST_DEVELOPMENT : an ACTIVITY which results in generating a FILE.FILE (via
"wasGeneratedBy") that can be referenced by the TEST_SCENARIO.testPackage.

  * TEST_ANNOTATION : an ENTITY that provides additional information about a
    TEST_RESULT that might define mitigating circumstances or additional
    context. [TC7.3]

      * "annotatedResult" property : the TEST_RESULT being annotated
      * "annotation" property : the ENTITY providing the annotation information

Also see [this graphical representation](/RACK-Ontology/Graphs/TESTING_scoped.svg)
of these primary TESTING elements and some of their surrounding elements.

# Sample Scenarios

## Scenario 1

The software written during the Program is intended to satisfy a particular
requirement "Req1" (among others).  The software is built via the documented
build process and the resulting artifact of the build is an executable,
identified by Target name and version.  This isn't strictly part of the testing
ontology, this is some information from the software development ontology:

    REQ1 is a REQUIREMENT has identifier "Req1".

    SOURCE1 is a FILE.
    MakeFile1 is a FILE.
    MainDev is a CODE_DEVELOPMENT.
    MainDev has goal SOURCE1.
    MainDev has goal MakeFile1.

    Bld1 is a COMPILE.
    Bld1 has used MakeFile1 has compileInput SOURCE1 has goal TARGET1.
    TARGET1 is a FILE.

The software must be verified to meet
Req1, so 5 requirements-based TESTs are developed (the TESTs 'wasGeneratedBy' a
TEST_DEVELOPMENT), and the 'verifies' target of each is the "Req1" REQUIREMENT.

The test development could be captured by another software development dataset:

    TDEV1 is a TEST_DEVELOPMENT.
    TDEV1 has goal TestSource1.
    TestSource1 is a FILE.

    TestBld1 is a COMPILE.
    TestBld1 has compileInput TestSource1 has goal TestExe.
    TestExe is a FILE.

Describing the 5 tests within the TESTING ontology portion and--since we
documented the `TEST_DEVELOPMENT` above--linking them back to their development
portion (which is not always present):

    TEST1 is a TEST has identifier "Test1".
    TEST2 is a TEST has identifier "Test2".
    TEST3 is a TEST has identifier "Test3".
    TEST4 is a TEST has identifier "Test4".
    TEST5 is a TEST has identifier "Test5".

    TEST1 has wasGeneratedBy TDEV1 has verifies REQ1 has wasDerivedFrom TestSource1.
    TEST2 has wasGeneratedBy TDEV1 has verifies REQ1 has wasDerivedFrom TestSource1.
    TEST3 has wasGeneratedBy TDEV1 has verifies REQ1 has wasDerivedFrom TestSource1.
    TEST4 has wasGeneratedBy TDEV1 has verifies REQ1 has wasDerivedFrom TestSource1.
    TEST5 has wasGeneratedBy TDEV1 has verifies REQ1 has wasDerivedFrom TestSource1.

Two of the TESTs ("Test1" and "Test2") are independent, and the remaining three
are independent of the first two but it is known that they must be run in a
specific order ("Test3" -> "Test4" -> "Test5").  This information is represented
by a TEST_PROCEDURE "VerifyReq1" with three 'independentTest' links to three
TEST_STEPs ("Step1", "Step2", "Step3"), where "Step1" 'thisStep' is "Test1",
"Step2" 'thisStep' is "Test2", and "Step3" 'thisStep' is "Test3".  There are two
more TEST_STEPs ("Step3b" and "Step3c"), where "Step3" has 'nextStep' to "Step3b"
(which has 'thisStep' to "Test4"), and "Step3b" has 'nextStep' to "Step3c" (which
has 'thisStep' to "Test5").  Note that it is not always apparent which tests are
dependent on other tests; the default approach is to simply assume all tests are
independent until otherwise known, as is described in this scenario.

    VR1 is a TEST_PROCEDURE has identifier "VerifyReq1".
    Tstep1 is a TEST_STEP has identifier "Step1" has thisStep TEST1.
    Tstep2 is a TEST_STEP has identifier "Step2" has thisStep TEST2.
    Tstep3 is a TEST_STEP has identifier "Step3" has thisStep TEST3.
    Tstep4 is a TEST_STEP has identifier "Step3b" has thisStep TEST4.
    Tstep5 is a TEST_STEP has identifier "Step3c" has thisStep TEST5.

    VR1 has independentTest Tstep1.
    VR1 has independentTest Tstep2.
    VR1 has independentTest Tstep3.
    Tstep3 has nextStep Tstep4.
    Tstep4 has nextStep Tstep5.

After the Target software and the Tests have been developed, it is time for the
actual testing to be performed (a TEST_EXECUTION activity).  This TEST_EXECUTION
identifies the 'testProcedure' of "VerifyReq1" (it may have additional
'testProcedure' targets if it is running multiple procedures), and it has a
'testLog' connection to a TEST_LOG to log records of the tests performed and the
results.  The TEST_LOG will have at least one 'content' connection to a
TEST_RECORD that is associated with the "VerifyReq1" TEST_PROCEDURE via the
'testRecordProcedure'.  Note that it is possible that the testing might only run
a subset of the TEST_PROCEDURE, so the TEST_RECORD has a 'testRecordSteps' to
each of the independent tests actually run during that TEST_EXECUTION; these
should be a subset of the 'content' links from the TEST_PROCEDURE to the
corresponding TEST_STEPs.  The TEST_RECORD also has a 'testRecordScenario' link
to a TEST_SCENARIO that identifies both the Target software (FILE and string
version) as well as the Testing software (FILE and string version).  The
TEST_RECORD may also have 'testConfiguration' links to ENTITYs that further
describe the configuration (e.g. the hardware elements, the test parameters,
etc.).

    Trun1 is a TEST_EXECUTION has testProcedure VR1.
    Tlog1 is a TEST_LOG.
    Trec1 is a TEST_RECORD.
    Scenario1 is a TEST_SCENARIO.

    Trun1 has testLog Tlog1.
    Tlog1 has content Trec1;

    Trec1 has testRecordProcedure VR1.
    Trec1 has testRecordSteps Tstep1.
    Trec1 has testRecordSteps Tstep2.
    Trec1 has testRecordSteps Tstep3.
    Trec1 has testRecordScenario Scenario1.
    Trec1 has testConfiguration Processor1.

    Processor1 is a HWCOMPONENT has componentType ComputePlatform.

    Scenario1 has targetPackage TARGET1 has targetVersion "v1".
    Scenario1 has testPackage TestExe has testVersion "v1".

Each individual TEST that is run during the TEST_EXECUTION should generate a
TEST_RESULT with a 'result' of TEST_STATUS to indicate if the TEST successfully
validated the 'verifies' Thing-Tested for the test.  Each TEST_RESULT has a
'wasGeneratedBy' to the TEST_EXECUTION, and a 'confirms' link to the TEST that it
is the result of.

    TRES1 is a TEST_RESULT has wasGeneratedBy Trun1 has confirms TEST1.
    TRES2 is a TEST_RESULT has wasGeneratedBy Trun1 has confirms TEST2.
    TRES3 is a TEST_RESULT has wasGeneratedBy Trun1 has confirms TEST3.
    TRES4 is a TEST_RESULT has wasGeneratedBy Trun1 has confirms TEST4.
    TRES5 is a TEST_RESULT has wasGeneratedBy Trun1 has confirms TEST5.

In our sample scenario, we will suppose that the TEST_STATUS for all of our TESTs
is 'Passed', with the exception of "Test2", which has a TEST_STATUS 'result' of
'Failed'.  However, Joe Test (an AGENT) subsequently performs a generic ACTIVITY
where he evaluates the failure and determines that it is not properly indicative
of the scenario defined by the "VerifyReq1" requirement (perhaps due to the
'testConfiguration' information) and therefore he creates a formal "Memo"
(TEST_ANNOTATION) that 'wasGeneratedBy' his ACTIVITY.  That Memo describes his
findings and documents his assessment that the 'Failed' TEST_STATUS should be
ignored.  That "Memo" has an 'annotatedResult' link to the TEST_EXECUTION's
TEST_RESULT for "Test2".

    TRES1 has result Passed.
    TRES2 has result Failed.
    TRES3 has result Passed.
    TRES4 has result Passed.
    TRES5 has result Passed.

    JoeTest is a PERSON has title "Joe Test".
    TestResultAnalysis is an ACTIVITY.
    TestResultAnalysis has wasAssociatedWith JoeTest has goal Memo.

    Memo is a TEST_ANNOTATION has wasGeneratedBy TestResultAnalysis.
    Memo has annotatedResult TRES2.
    Memo has annotatedValue IgnoredFailureOOB.


## Scenario 2

A software developer (AGENT "Alice") in the Program writes a SWCOMPONENT
"bitflipper" with a 'componentType' of 'SourceFunction' as part of a
CODE_DEVELOPMENT activity.  "Alice"'s pair-programmer AGENT "Bob" performs a
TEST_DEVELOPMENT activity to write 3 Unit TESTs that will directly link with
"Alice"'s SWCOMPONENT to create a distinct test executable (FILE and Version
string) FILE with 'filename' "BitFlipperTesting" via a COMPILE activity. The
COMPILE activity has a 'compileInput' link to the FILE containing the
"bitflipper" SWCOMPONENT. These three tests ("Flip", "Flop", "Flap") are
independent and therefore each is the 'thisStep' of "FlipStep", "FlopStep", and
"FlapStep" TEST_STEP's, respectively, all three of which are 'content' linked to
by the "FLIPTest" TEST_PROCEDURE.  All three of "Flip", "Flop", and "Flap" TESTs
have a 'verifies' link to the "bitflipper" SWCOMPONENT.

    ALICE is a PERSON has title "Alice".
    InitialDev is a CODE_DEVELOPMENT wasAssociatedWith ALICE.
    InitialDev has goal BitFlipperSource has goal BitFlipper.
    BitFlipperSource is a FILE.
    BitFlipper is a SWCOMPONENT has title "bitflipper".
    BitFlipper has componentType SourceFunction.
    BitFlipper has partOf BitFlipperSource.

    BOB is a Person has title "Bob".
    TestDev is a TEST_DEVELOPMENT wasAssociatedWith BOB.
    Flip is a TEST has wasGeneratedBy TestDev.
    Flap is a TEST has wasGeneratedBy TestDev.
    Flop is a TEST has wasGeneratedBy TestDev.
    TestDev has goal SrcTestFile.
    SrcTestFile is a FILE.

    TestBld1 is a COMPILE wasAssociatedWith BOB.
    TestBld1 has compileInput SrcTestFile has compileInput BitFlipperSource.
    TestBld1 has goal TestExe.
    TestExe is a FILE has filename "BitFlipperTesting".

    Flip has verifies BitFlipper.
    Flap has verifies BitFlipper.
    Flop has verifies BitFlipper.

    FlipStep is a TEST_STEP has thisStep Flip.
    FlapStep is a TEST_STEP has thisStep Flap.
    FlopStep is a TEST_STEP has thisStep Flop.

    FLIPTest is a TEST_PROCEDURE.
    FLIPTest has independentTest FlipStep.
    FLIPTest has independentTest FlapStep.
    FLIPTest has independentTest FlopStep.

"Bob" then performs a TEST_EXECUTION activity for the 'testProcedure' "FLIPTest"
which generates a 'testLog' link to a TEST_LOG with 'content' of a TEST_RECORD.
The TEST_RECORD has a 'testRecordProcedure' to "FLIPTest", a 'testRecordScenario'
to a TEST_SCENARIO that has a 'testPackage' of "BitFlipperTesting" and
'testVersion' of "1.0" but no 'targetPackage' linkage.  At the completion of the
TEST_EXECUTION, there are three TEST_RESULTs that 'wasGeneratedBy' the
TEST_EXECUTION, where each confirms "Flip", "Flop" and "Flap" TESTs respectively.
Unfortunately, "Flip" and "Flap" have a TEST_RESULT 'result' TEST_STATUS of
'Passed', but "Flop" is 'Failed'.

    Run1 is a TEST_EXECUTION has testProcedure FLIPTest wasAssociatedWith BOB.
    Log1 is a TEST_LOG.
    Record1 is a TEST_RECORD.
    Scenario1 is a TEST_SCENARIO.

    Run1 has testLog Log1.
    Log1 has content Record1.
    Record1 has testRecordProcedure FLIPTest.
    Record1 has testRecordSteps FlipStep.
    Record1 has testRecordSteps FlapStep.
    Record1 has testRecordSteps FlopStep.
    Record1 has testRecordScenario Scenario1.

    Scenario1 has testPackage TestExe has testVersion "1.0".

    Flipped1 is a TEST_RESULT has wasGeneratedBy Run1 has confirms Flip.
    Flapped1 is a TEST_RESULT has wasGeneratedBy Run1 has confirms Flap.
    Flopped1 is a TEST_RESULT has wasGeneratedBy Run1 has confirms Flop.

    Flipped1 has result Passed.
    Flapped1 has result Passed.
    Flopped1 has result Failed.


"Alice" looks at the failed "Flop" test and realizes she needs to make a small
change to the "bitflipper", which she does as a new CODE_DEVELOPMENT.  "Bob" then
performs another COMPILE activity to get a new version of "BitFlipperTesting".
He then performs another TEST_EXECUTION, which has all the aforementioned links,
except this time he only runs the previously-failed Flop test, which now has a
new TEST_RESULT with a 'result' TEST_STATUS of 'Passed'.  The TEST_RECORD
'testRecordScenario' linked TEST_SCENARIO for this second run still has a
'testPackage' of "BitFlipper" but the 'testVersion' is now "1.1", and the
corresponding COMPILE activity has a 'compileInput' to the new FILE containing
"Alice"'s updated "bitflipper" that 'wasGeneratedBy' her second CODE_DEVELOPMENT
activity.

    FixBitFlipper is a CODE_DEVELOPMENT wasAssociatedWith ALICE.
    FixBitFlipper has goal BitFlipperSource2 has goal BitFlipper.
    BitFlipperSource2 is a FILE.
    BitFlipper has partOf BitFlipperSource2.

    TestBld2 is a COMPILE wasAssociatedWith BOB.
    TestBld2 has compileInput SrcTestFile has comipleInput BitFlipperSource2.
    TestBld2 has goal TestExe2.
    TestExe2 is a FILE has filename "BitFlipperTesting".

    Run2 is a TEST_EXECUTION has testProcedure FLIPTest wasAssociatedWith BOB.
    Log2 is a TEST_LOG.
    Record2 is a TEST_RECORD.
    Scenario2 is a TEST_SCENARIO.

    Run2 has testLog Log2.
    Log2 has content Record2.
    Record2 has testRecordProcedure FLIPTest.
    Record2 has testRecordSteps FlopStep.
    Record2 has testRecordScenario Scenario2.

    Scenario2 has testPackage TestExe2 has testVersion "1.1".

    Flopped2 is a TEST_RESULT has wasGeneratedBy Run2 has confirms Flop.
    Flopped2 has result Passed.

Another note to make here is that it's not always the case that this type of
incremental development process is captured in the data provided to RACK.  The
minimum scenario is that there is a `TEST_RECORD` that fully matches the
`TEST_SCENARIO` and contains a `TEST_RESULT` that confirms every `TEST`; the
additional information captured in the above multi-stage scenario is enhanced
information that is not always available.
