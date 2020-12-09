cmake_minimum_required(VERSION 3.13)  # was 3.17

# set the project name
project(GnuCOBOL VERSION 3.1
  DESCRIPTION "GnuCOBOL 4.0-early-dev"
  HOMEPAGE_URL https://www.gnu.org/software/gnucobol/)

set(PACKAGE gnucobol)
set(PACKAGE_BUGREPORT bug-gnucobol@gnu.org)
set(PACKAGE_NAME ${PROJECT_NAME})
set(PACKAGE_STRING ${PROJECT_DESCRIPTION})
set(PACKAGE_TARNAME ${PACKAGE})
set(PACKAGE_URL ${PROJECT_HOMEPAGE_URL})
set(PACKAGE_VERSION ${PROJECT_VERSION})

# configure options
set(PATCH_LEVEL
  0 CACHE STRING "Define a patch level" )
set(MAX_CALL_PARAMS
  192 CACHE STRING "Define maximum CALL parameters" )
set(PIC
  both CACHE STRING "Use only PIC/non-PIC objects" )
set(AIX_SONAME
  aix CACHE STRING "AIX shared library versioning" )
#set(GNU_LD          0 CACHE STRING  "Assume the C compiler uses GNU ld" "no" )
#set(SYSROOT         0 CACHE STRING  "Search directory for libraries")
#set(GNU_LD          0 CACHE STRING  "assume the C compiler uses GNU ld [default=no]" )
set(GCOV
  gcov CACHE STRING "Use given GCOV for coverage" )
#set(MATH            0 CACHE STRING  "Use math multiple precision library," )
#set(XML2             0 CACHE STRING "Use libxml2 as XML handler (default)" )
set(JSON
  yes CACHE STRING "Use json library/source as JSON handler")
#set(DL               0 CACHE STRING "Use system dynamic loader (default)" )
set(VARSEQ
  0 CACHE STRING "Define variable sequential format" )
#set(CURSES           0 CACHE STRING "Use curses library for extended SCREEN" )
#set(SEQRA_EXTFH      0 CACHE STRING "Use external SEQ/RAN handler (obsolete)" )
#set(INDEXED          0 CACHE STRING "Define default INDEXED file handler" )
#set(VBISAM           0 CACHE STRING "Use VBISAM for ISAM I/O" )
#set(DISAM            0 CACHE STRING "Use DISAM for ISAM I/O" )
#set(CISAM            0 CACHE STRING "Use CISAM for ISAM I/O" )
#set(INDEX_EXTFH      0 CACHE STRING "Use external ISAM handler (obsolete)" )
#set(ODBC             0 CACHE STRING "Use ODBC for INDEXED I/O" )
#set(OCI              0 CACHE STRING "Use Oracle OCI for INDEXED I/O" )
#set(DB               0 CACHE STRING "Use Berkeley DB >= 4.1 for ISAM I/O" )
#set(LMDB             0 CACHE STRING "Use Lightning Memory-Mapped Database" )
#set(LIBICONV_PREFIX  0 CACHE STRING "Search for libiconv in DIR/include and DIR/lib" )
#set(LIBINTL_PREFIX   0 CACHE STRING "Search for libintl in DIR/include and DIR/lib" )

#
# AC_C_INLINE for CMake
#
foreach(KEYWORD "inline" "__inline__" "__inline" "")
  if(NOT DEFINED COB_KEYWORD_INLINE)
    try_compile(C_INLINE
      ${PROJECT_BINARY_DIR}
      ${PROJECT_SOURCE_DIR}/build_aux/cmake/inline.c
      COMPILE_DEFINITIONS "-DCOB_KEYWORD_INLINE=${KEYWORD}")
    if(C_INLINE)
      message("inline is spelled '${KEYWORD}' ")
      set(COB_KEYWORD_INLINE ${KEYWORD})
    endif()
  endif()
endforeach()

set(COB_CONFIG_DIR ${CMAKE_INSTALL_PREFIX}/share/${PACKAGE}/config)
set(COB_COPY_DIR ${CMAKE_INSTALL_PREFIX}/share/${PACKAGE}/copy)
set(COB_SCHEMA_DIR ${CMAKE_INSTALL_PREFIX}/share/${PACKAGE}/schema)
set(VARSEQ 0)
set(MAX_CALL_FIELD_PARAMS 192)
set(PATCH_LEVEL 0)

set(COB_BLD_CC		$ENV{CC})
set(COB_BLD_CFLAGS	$ENV{CFLAGS})
set(COB_BLD_CPPFLAGS	$ENV{CPPFLAGS})
set(COB_BLD_LD		$ENV{LD})
set(COB_BLD_LDFLAGS	$ENV{LDFLAGS})
set(COB_BLD_BUILD	${CMAKE_SYSTEM_NAME})

set(COB_DEBUG_FLAGS     "-g")
set(COB_EXPORT_DYN      "-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base" )
set(COB_LDFLAGS         "$ENV{LDFLAGS}")
set(COB_LIBS            "-L${CMAKE_BINARY_DIRECTORY}/libcob -lcob")

set(USE_LIBDL 1)

message("COB_BLD_BUILD is '${COB_BLD_BUILD}'")

# end configure options

link_directories(build_windows/x64 build_windows/x64/Debug)

add_compile_definitions(MSC_USE_DLL)

add_compile_definitions(_POSIX_C_SOURCE=200809L)
add_compile_definitions(_XOPEN_SOURCE=500)
add_compile_definitions(_BSD_SOURCE)

set(COB_MODULE_EXT ${CMAKE_SHARED_LIBRARY_SUFFIX})
set(COB_EXE_EXT    ${CMAKE_EXECUTABLE_SUFFIX})

include(CheckIncludeFile)

check_include_file(cjson/cJSON.h HAVE_CJSON_CJSON_H)
check_include_file(cJSON.h HAVE_CJSON_H)
check_include_file(db.h HAVE_DB_H)
check_include_file(disam.h HAVE_DISAM_H)
check_include_file(dlfcn.h HAVE_DLFCN_H)
check_include_file(fcntl.h HAVE_FCNTL_H)
check_include_file(gmp.h HAVE_GMP_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(isam.h HAVE_ISAM_H)
check_include_file(locale.h HAVE_LOCALE_H)
check_include_file(ltdl.h HAVE_LTDL_H)
check_include_file(malloc.h HAVE_MALLOC_H)
check_include_file(memory.h HAVE_MEMORY_H)
check_include_file(mpir.h HAVE_MPIR_H)
check_include_file(oci.h HAVE_OCI_H)
check_include_file(signal.h HAVE_SIGNAL_H)
check_include_file(sqlext.h HAVE_SQLEXT_H)
check_include_file(sql.h HAVE_SQL_H)
check_include_file(stddef.h HAVE_STDDEF_H)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(strings.h HAVE_STRINGS_H)
check_include_file(string.h HAVE_STRING_H)
check_include_file(sys/stat.h HAVE_SYS_STAT_H)
check_include_file(sys/sysmacros.h HAVE_SYS_SYSMACROS_H)
check_include_file(sys/time.h HAVE_SYS_TIME_H)
check_include_file(sys/types.h HAVE_SYS_TYPES_H)
check_include_file(sys/wait.h HAVE_SYS_WAIT_H)
check_include_file(unistd.h HAVE_UNISTD_H)
check_include_file(vbisam.h HAVE_VBISAM_H)
check_include_file(wchar.h HAVE_WCHAR_H)

include(CheckTypeSize)
check_type_size(sigset_t, HAVE_SIGNAL_H)

include(CheckSymbolExists)

check_symbol_exists(isfinite math.h  HAVE_ISFINITE)

if( ${HAVE_GMP_H} )
  message("HAVE_GMP_H is '${HAVE_GMP_H}'")
  find_library (GMP_LIBRARIES  gmp)
else()
  message("HAVE_MPIR_H is '${HAVE_MPIR_H}'")
  find_library (GMP_LIBRARIES  mpir)
endif()

if( ${JSON} STREQUAL no )
  set(WITH_JSON "disabled")
else()
  message("JSON is '${JSON}' ")
  if( ${JSON} STREQUAL yes )
    find_library (JSON_LIBRARIES  NAMES cjson json-c)
  else()
    find_library (JSON_LIBRARIES  ${JSON})
  endif()  
  set(WITH_JSON ${JSON_LIBRARIES})
endif()  

message("WITH_JSON defined as '${WITH_JSON}' ")

if( ${CMAKE_SYSTEM_NAME} STREQUAL Windows )
  configure_file (
    "${PROJECT_SOURCE_DIR}/build_windows/config.h"
    "${PROJECT_BINARY_DIR}/config.h"
    )
else()
  configure_file (
    "${PROJECT_SOURCE_DIR}/build_aux/cmake/config.cmake.h"
    "${PROJECT_BINARY_DIR}/config.h"
    )
endif()

add_library( cob SHARED
             libcob/call.c libcob/cobgetopt.c
	     libcob/common.c libcob/fbdb.c libcob/fextfh.c
	     libcob/fileio.c libcob/fisam.c libcob/flmdb.c
	     libcob/focextfh.c libcob/foci.c libcob/fodbc.c
	     libcob/fsqlxfd.c libcob/intrinsic.c libcob/libcobci.c
	     libcob/libcobdi.c libcob/libcobvb.c libcob/mlio.c
	     libcob/move.c libcob/numeric.c
	     libcob/reportio.c libcob/screenio.c libcob/strings.c
	     libcob/termio.c )

set_property(TARGET cob PROPERTY RUNTIME_OUTPUT_DIRECTORY libcob)

target_include_directories(cob PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/libcob>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/build_windows>
)

target_link_libraries(cob ${GMP_LIBRARIES} ${CURSES_LIBRARIES} vbisam dl)
	   
set_property(TARGET cob PROPERTY WINDOWS_EXPORT_ALL_SYMBOLS)

find_package(BISON 2.7.1 REQUIRED )
find_package(FLEX  2.6   REQUIRED )
find_package(Curses)

file(MAKE_DIRECTORY
  ${CMAKE_BINARY_DIR}/bin
  ${CMAKE_BINARY_DIR}/cobc
  ${CMAKE_BINARY_DIR}/libcob)

BISON_TARGET(preparser cobc/ppparse.y ${CMAKE_BINARY_DIR}/cobc/ppparse.c)
FLEX_TARGET(prescanner cobc/pplex.l  ${CMAKE_CURRENT_BINARY_DIR}/cobc/pplex.c)
ADD_FLEX_BISON_DEPENDENCY(prescanner preparser)

BISON_TARGET(Parser cobc/parser.y ${CMAKE_CURRENT_BINARY_DIR}/cobc/parser.c)
FLEX_TARGET(Scanner cobc/scanner.l  ${CMAKE_CURRENT_BINARY_DIR}/cobc/scanner.c)
ADD_FLEX_BISON_DEPENDENCY(Scanner Parser)

include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}/cobc)

# add executable, excluding  cobc/debug.c
add_executable(cobc ${BISON_preparser_OUTPUTS}
                    ${FLEX_prescanner_OUTPUTS}
                    ${BISON_Parser_OUTPUTS}
                    ${FLEX_Scanner_OUTPUTS}
                    cobc/cobc.c cobc/codegen.c cobc/codeoptim.c
		    cobc/config.c  cobc/error.c
		    cobc/field.c cobc/help.c cobc/reserved.c
		    cobc/sqlxfdgen.c cobc/tree.c cobc/tree.h cobc/typeck.c )

set_property(TARGET cobc PROPERTY RUNTIME_OUTPUT_DIRECTORY cobc)

target_include_directories(cobc PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/cobc>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/build_windows>
)

target_link_libraries(cobc cob)

add_executable(cobcrun bin/cobcrun.c)

set_property(TARGET cobcrun PROPERTY RUNTIME_OUTPUT_DIRECTORY bin)

set(COBCRUN_NAME "cobcrun")

target_include_directories(cobcrun PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/cobc>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/build_windows>
)

target_link_libraries(cobcrun cob)

add_executable(gcdiff bin/gcdiff.c)

set_property(TARGET gcdiff PROPERTY RUNTIME_OUTPUT_DIRECTORY bin)

target_include_directories(gcdiff PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/cobc>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/build_windows>
)

target_link_libraries(gcdiff cob)


