This is a brief introduction to the various tools that can be used when doing development work on GnuCOBOL.

Note to readers: Before looking up rr and perf for help, keep in mind that virtualised performance counters might not work on virtual machines or Dockerised environments. Root access is required to enable virtualised performance counters.
================

Getting started:

Assuming you have got your hands on a clone of the GnuCOBOL source and finished configuring the compiler, you should first understand the testsuite and how to get around it. After doing any kind of work, you should always start off by building the compiler with your newly added changes in place and start running tests to verify whatever your work was about. (You should hopefully have a few test cases laid out before working (at a high level) so you can immediately get to testing your work.)

The testsuite is where all of this takes place. Whether it be runtime or compile-time features, the testsuite is aimed at providing coverage for all sorts of features.

To run the testsuite, it's as simple as going to your build directory and running
- make check

However, there are numerous tests, and some of them might not even be relevant to your work (i.e., running runtime tests when all you did was frontend compiler work).

It is best to get around your tests using the TESTSUITEFLAGS command-line argument.

This provides you with the facility of simply choosing specific tests like this:

- make check TESTSUITEFLAGS='-k cobc'

The previous make command would run only tests with cobc as a keyword (-k cobc).

All your test results are laid out inside the build directory under $builddir/tests/testsuite.dir/

This is particularly important to know for the next section.

================

Valgrind:

Valgrind is the first tool we will cover in this guide. It is a very useful tool for memory debugging and finding memory leaks. Let's look at how to use Valgrind in tests.

As mentioned previously, we would ideally use a "make check TESTSUITEFLAGS" to specify some tests we would like to find the results for. To add Valgrind memory debugging, you need to pass an environment value for VGSUFFIX. This would be a particularly repeating pattern with all the tools we are about to discuss.

- make check TESTSUITEFLAGS='-k somekeywords' VGSUFFIX=vg

The above command simply defines the VGSUFFIX variable with the value vg. This vg is passed to the shell script in /$builddir/tests/atlocal to invoke Valgrind and to confirm that the developer indeed wishes to use Valgrind.

Understand that there are a few default options that are specified when it comes to using Valgrind (refer to $builddir/tests/atlocal), like "--leak-check=full" and others. All those options are again explicitly defined as the var MEMCHECK.

MEMCHECK then precedes different parts of the compiler, like COBC and COBCRUN, and it is actually up to you to comment those out if you don't want Valgrind tooling in those.

After running the testsuite with VGSUFFIX, you should refer to /$builddir/tests/valgrind/${YOURSUFFIX} to find the summary for all tests with Valgrind.

================

Callgrind:

Callgrind is a tool within Valgrind, but instead of using it for memory checks, you would primarily use it for profiling. Things you can inspect include: call graphs, function call costs, self costs, and descendant (functions called) costs.

Testing follows the same as with VGSUFFIX. However, for Callgrind, use CGSUFFIX=YOURCGSUFFIX. (.out) files for Callgrind can then be found in /$builddir/tests/callgrind/${YOURCGSUFFIX}.

It is worth mentioning that the compilation process (i.e., the compiler frontend work COBC) is by default commented out in the atlocal script. If you wish to profile COBC work, you will have to uncomment the definition for COBC in the CGSUFFIX section of atlocal. By itself, only the runtime is available for Callgrind profiling.

Pro tip: Install and use Kcachegrind for a visual representation of the summary.

================

rr:

rr (record and replay) is a particularly useful tool when it comes to debugging. It not only allows you to run the program forward between instructions but also allows you to run it backwards, even after an instruction has been executed during your debug session.

rr already supports gdb commands, but it adds some others for reverse execution. The guide for rr is beyond the scope of this documentation, but feel free to refer to the official rr docs.

As per usual, to add rr support for any of the tests you are particularly doing, use the rather predictable RRSUFFIX var when running your make check command. You should refer to /$builddir/tests/rr/${YOURRRSUFFIX} to find all the tests with rr recordings that you can replay.

An important note is that all your testsuite tests might fail when using RRSUFFIX. This happens because rr expects perf_event_paranoid to be set to <=3. If your machine has it set to 4 (as was the case with mine when starting out), all the tests might fail.

You can check for this by using

- cat /proc/sys/kernel/perf_event_paranoid

One more note: to actually use rr commands like replay, you need to set your trace directory after running the testsuite to that of your suffix. To put it in a more digestible manner, when you run a make check with RRSUFFIX, you now create a new directory with your chosen suffix under /$builddir/tests/rr, and to list the files with actual rr traces that you can jump back and forth in (such as rr replay), you need to set your _RR_TRACE_DIR to the directory where your suffix lives.

- make check TESTSUITEFLAGS='-k cobcrun' RRSUFFIX=myrr

You would do _RR_TRACE_DIR=/path/to/myrr rr replay
to replay the rr files in myrr.

================

Perf:

Perf will be our last tool for discussion here. Perf can be used for collecting performance counters, like the number of instructions an executable took, but it can also be used for profiling and finding exactly how much time each function is taking (very useful when determining what areas to improve on first).

PERFSUFFIX=xyz creates an xyz directory under /$builddir/tests/perf/ with the log files to inspect the number of instructions and the time it took to carry out those instructions.

Adding a PERFRECORD adds the recording for those tests so you can profile function calls and see where most of your processing time is being spent.

- make check TESTSUITEFLAGS='-k someflag' PERFSUFFIX=perf

This is simply an example of the use of PERFSUFFIX. You would find the output of such files at /$builddir/tests/perf/perf.

As with rr, the tests might very well break when including PERFSUFFIX because it expects perf_event_paranoid to be <=1 to open up system access.

- cat /proc/sys/kernel/perf_event_paranoid
- sudo sysctl kernel.perf_event_paranoid=1

To actually view the results, you can use the following:

- perf report -i /path/to/perf.data

for a command-line view of the report, but tools like Hotspot make it into a GUI that is perhaps more interactive and easier to use.

A final point to mention: one perf_aggregate.sh script exists and is particularly useful when you run multiple tests and want to "aggregate" all the results into one log.

You can achieve this by:

- $rootdir/tests/perf_aggregate.sh $rootdir/$builddir/tests/perf/cobcrun/cobcrun.log

This would aggregate all cobcrun logs into one. This allows the possibility of having multiple suffixes, where some are for old "unoptimized code" and some for new optimized code, and then aggregating each to verify true improvement.

================