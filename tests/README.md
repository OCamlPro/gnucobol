This is a brief introduction to the various tool that can be used when doing dev work on GnuCOBOL.
================

Getting started:

	before diving into the various tools that can help when building/fixing bugs. It is best to start off with understanding how to invoke such tools and where to do so.

Assuming you have got your hands on a clone of the GnuCOBOL source and finished configuring the compiler. You should first understand The Testsuite and getting around the TestSuite. After doing any kind of work you should always start off by building the compiler with your newly added changes in place and start running tests to verify whatever your work was on about. (You should hopefully have a few testcases laid out before at least at a high level so you can immediately get to testing your work)

The Testsuite is where all this takes place. Whether it be runtime or compile time features the Testsuite is aimed to provide coverage for all sorts of features.

To run the TestSuite its as simple as going to you build directory and running
- make check

However there are numerous tests and some of which might not even be relevant to your work (i.e running runtime tests when all you did was frontend compiler work)

It is best you get around your tests using the TESTSUITEFLAGS command line argument

This provides you with the lovely facility of simply choosing specific tests like this:

- make check TESTSUITEFLAGS='-k cobc'

The previous make command would run only tests with cobc as keyword (-k cobc).

All your Tests results are laid inside the build directory under build/tests/testsuite.dir/

This is particularly important to know with the upcoming section.


================

Valgrind:

Valgrind would be our first tool we go over in this guide. Valgrind is a very useful tool for memory debugging and finding memory leaks. Lets go over using valgrind in tests

As mentioned previously we would ideally use a "make check TESTSUITEFLAGS" to specify some tests we love to find the result for. To add valgrind memory debugging you need to pass an environment value for one VGSUFFIX. This would be a particularly repeating pattern with all the tool we are about to discuss.

- make check TESTSUITEFLAGS='-k somekeywords' VGSUFFIX=vg

The above command simply defines VGSUFFIX env variable with value vg. This vg is passed to the shell script in /build/tests/atlocal.in to invoke Valgrind and to confirm that the developer indeed wishes to use valgrind.

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
