This is a brief introduction to the various tools that can be used when doing development work on GnuCOBOL.
================

Getting started:

	before diving into the various tools that can help when building/fixing bugs. It is best to start off with understanding how to invoke such tools and where to do so.

Assuming you have got your hands on a clone of the GnuCOBOL source and finished configuring the compiler. You should first understand the testsuite and getting around it. After doing any kind of work you should always start off by building the compiler with your newly added changes in place and start running tests to verify whatever your work was on about. (You should hopefully have a few testcases laid out before at a high level so you can immediately get to testing your work)

The testsuite is where all this takes place. Whether it be runtime or compile time features the testsuite is aimed to provide coverage for all sorts of features.

To run the testSuite its as simple as going to you build directory and running
- make check

However there are numerous tests and some of which might not even be relevant to your work (i.e running runtime tests when all you did was frontend compiler work)

It is best you get around your tests using the TESTSUITEFLAGS command line argument

This provides you with the lovely facility of simply choosing specific tests like this:

- make check TESTSUITEFLAGS='-k cobc'

The previous make command would run only tests with cobc as keyword (-k cobc).

All your tests results are laid inside the build directory under build/tests/testsuite.dir/

This is particularly important to know with the upcoming section.

================

Valgrind:

Valgrind is the first tool we will cover in this guide. It is a very useful tool for memory debugging and finding memory leaks. Let's look at how to use Valgrind in tests.

As mentioned previously we would ideally use a "make check TESTSUITEFLAGS" to specify some tests we love to find the result for. To add valgrind memory debugging you need to pass an environment value for one VGSUFFIX. This would be a particularly repeating pattern with all the tool we are about to discuss.

- make check TESTSUITEFLAGS='-k somekeywords' VGSUFFIX=vg

The above command simply defines VGSUFFIX env variable with value vg. This vg is passed to the shell script in /build/tests/atlocal to invoke Valgrind and to confirm that the developer indeed wishes to use valgrind.

understand that there are a few default options that are specified when it comes to using valgrind (Refer to atlocal) like "--leak-check=full" and others. All those options are again explicitly defined as the var MEMCHECK. in the shell script it is done so that MEMCHECK options (like --leak-check=full).

MEMCHECK then precede different parts of the compiler like COBC and COBCRUN and it is actually up to you to comment those out if you dont want valgrind tooling in those.

After running testsutie with VGSUFFIX you should refer to /build/tests/valgrind/${YOURSUFFIX} to find the summary for all tests with valgrind.


================

Callgrind:

Callgrind is a tool within Valgrind but instead of using it for memory checks you would primarly use it for profiling. Things you can inspect include: call graphs, function call costs, self cost and decendant (functions called) costs.

Testing Follows the same as with VGSUFFIX except for Callgrind use CGSUFFIX=YOURCGSUFFIX. .out files for callgrind can be found then in tests/callgrind/${YOURCGSUFFIX}.

It is worthy of mentioning that the compilation process (i.e the compiler frontend work COBC) is by default commented out in the atlocal script. If you wish to profile COBC work you will have to uncomment the definition for COBC in the CGSUFFIX section of atlocal.By itself only the runtime is available for callgrind profiling.

Pro tip: install and use Kcachegrind for a visual representation of the summary.

================

rr:

rr (record and replay) is a particularly useful tool when it comes to debugging. It not only allows you to jump forward between instructions but also allows you jump backwards even after an instruction has been executed during your debug session.

rr already supports gdb commands but it adds some for reverse execition. The guide for rr is beyond the scope of this documentation but feel free to refer to the rr official docs.

As per usual, to add rr support for any of the tests you are particularly doing use the rather predicatble RRSUFFIX var when running your make check command. You should refer to build/tests/rr/${YOURRRSUFFIX} to find all the tests with rr recorded that you can replay.

A rather important note is that it might be the case that all your testsuite fails when using RRSUFFIX. This happens because rr expects perf_event_paranoid to be set to <=3. If your machine has it to 4 (As was with mine when starting out), all the tests might fail.

you can check for this by using

- cat /proc/sys/kernel/perf_event_paranoid

One more note, to actually use rr commands like replay you need to set your trace directory after running the testsuite to that of your suffix. To put it in a more digestable manner, when you run a make check with RRSUFFIX you now create a new directory with your chosen suffix under build/tests/rr and to list the files with actual rr trace that you can jump back and fourth in (such as rr replay) you need to set your _RR_TRACE_DIR to the directory where your suffix lives

- make check TESTSUITEFLAGS='-k cobcrun' RRSUFFIX=myrr

you would do _RR_TRACE_DIR=/path/to/myrr rr replay
to replay the rr files in myrr

================

Perf:

Perf will be our last tool for discussion here. Perf can be used for collecting performance counters like the number of instructions an executable took but it can also be used for profiling and finding exactly how much time each function is taking (Very useful when determining what areas to improve on first)

PERFSUFFIX=xyz creats a xyz directory under build/tests/perf/ with the log files to inspect number of instructions and time it took to carry out those instructions.

adding a PERFRECORD add the recording for the those tests so you can profile function calls and see where most of your processing time is being spent.

- make check TESTSUITEFLAGS='-k someflag' PERFSUFFIX=perf

This is simply an example for the use of PERFSUFFIX. You would find the output of such file at build/tests/perf/perf

================

