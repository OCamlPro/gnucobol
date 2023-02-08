# look at 'build_anchors' to see how the build dir is detected among upper dirs

abs_builddir=${AUTOFONCE_BUILD_DIR}/tests

# these files are generated during configure or first `make check`

. ${abs_builddir}/atconfig
. ${abs_builddir}/atlocal

# the following flags are needed for the NIST testsuite

if test "x$AUTOFONCE_TESTSUITE" = "xnist"; then
  export COB_DISABLE_WARNINGS=Y
  export COB_SWITCH_1=ON
  export COB_SWITCH_2=OFF
  export COMPILE85="$COMPILE -std=cobol85 -Wno-goto-different-section -Wno-goto-section -I copy"
  export COMPILE_MODULE85="$COMPILE_MODULE -std=cobol85 -Wno-goto-different-section -Wno-goto-section -I copy"
  export GREP="$GREP --text"
fi
