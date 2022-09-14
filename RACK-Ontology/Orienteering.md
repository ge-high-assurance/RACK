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
  
---- temporary below --------------------------------------------------

Changes:

1. Currently, TEST_STEP is a collection of TESTs that also has a nextStep to a
   single TEST_STEP.  It seems confusing for a single STEP to have several TESTS,
   and this does not provide any information about the relationship of the TESTS
   in the TEST_STEP. [Or it implies that unrelated tests are a layer below
   sequenced tests, which is the inverse of TC6.1].

   a. Proposed change: TEST_STEP is an ENTITY with thisStep and 0 or more values
      of nextStep, which have a range of type TEST.


2. Nothing points to the executable and version tested

   a: Proposed change: add TEST_SCENARIO

3. How to retire tests, or update tests? [TC7.2]

   Also, should there be a TEST_EXECUTABLE which is a FILE.FILE, but which also
   has a "testFileProcedure" to link to the TEST_PROCEDURE for the files it
   contains?  That might be a duplicate/conflict with
   TEST_RECORD.testRecordProcedure though.

4. Modify TEST_RECORD, which currently parallels TEST_STEP but doesn't add any
   new information (and if TEST_RECORD.nextRecord conflicts with
   TEST_STEP.nextStep that could be problematic.

5. Add fields to TEST_EXECUTION
