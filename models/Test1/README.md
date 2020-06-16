# General Guidelines


* No duplicated data in the stored content
* Test queries are fully completed (no missing data)
* Ingested input can be updated or added to while retaining data not
updated/added.

# Test Scenario

* Step 1 :: provider (TA4) builds software with GCC using RedHat 10
            for native x86_64 execution and delivers to TA1.  TA1 #1
            runs analysis on the binaries.

* Step 2 :: provider delivers source, TA1 #2 cross-compiles to ARM on
            Ubunto 20.04 using Clang 9 and runs analysis on the
            source+result.

* Step 3 :: provider delivers source, TA1 #3 cross-compiles to ARM64
            on CentOS 10 using Clang 10 and runs analysis on the
            source+result.

* Step 4 :: provider delivers updated source, TA1 #3 recompiles
            (ARM64, CentOS 10, Clang 10) and provides updated analysis
            information on the source+result.

* Step 5 :: TA3 attempts to identify if any of the above contain
            openssl version 1.20?  Assumes all 4 of the above are
            present in the TA2 polystore.

* Step 6 :: TA3 attempts to determine if any of the components and/or
            tools got built using the boehm garbage collector library
            version 3.85 as obtained from CenOS.org which was found to
            contain supply-chain malware [a hypothetical situation,
            not an actual event].

* Step 7 :: TA3 attempts to determine if any of the source files were
            not digitally signed with a valid certificate.

* Step 8 :: TA3 attempts to determine the last update date of any the
            source files to compare to the last update date of any LLR
            items and the last update of TestResults.
