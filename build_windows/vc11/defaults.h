// Use \ to escape things, for example \\ for using in paths
// Use \" for paths with spaces

// define MAKE_DIST to 1 for using C:\GNU Cobol

#define COB_CC           "cl"
#ifdef MAKE_DIST
#define COB_MAIN_DIR     "C:\\" PACKAGE_STRING
#define COB_CFLAGS       "-I \"" COB_MAIN_DIR "\\include\""
#define COB_LDFLAGS      "/LIBPATH:\"" COB_MAIN_DIR "\\libs\""
#else  // no MAKE_DIST
#define COB_MAIN_DIR     "C:\\GnuCobolCPP"
#define COB_CFLAGS       "-I \"" COB_MAIN_DIR "\" -I \"" COB_MAIN_DIR "\\build_windows\""

#ifdef _WIN64
#define TARGET_PLATFORM "x64"
#else
#define TARGET_PLATFORM "win32"
#endif

#ifdef _DEBUG
#define TARGET_CONFIG "debug"
#else
#define TARGET_CONFIG "release"
#endif

#define COB_LDFLAGS      "/LIBPATH:\"" COB_MAIN_DIR "\\build_windows\\" TARGET_PLATFORM "\\" TARGET_CONFIG "\""

#endif // no MAKE_DIST
#define COB_LIBS         "libcob.lib"

// Do not put quotation marks here:
#define COB_CONFIG_DIR   COB_MAIN_DIR "\\config"
#define COB_COPY_DIR     COB_MAIN_DIR "\\copy"

#define COB_LIBRARY_PATH "."
#define COB_MODULE_EXT   "dll"
#define COB_OBJECT_EXT   "obj"

// /* try to use compile defines for that:
#define COB_BLD_CC       "cl.exe"
#define COB_BLD_CPPFLAGS ""
#ifdef _DEBUG
#define COB_BLD_CFLAGS   "/GS /GL /Zc:wchar_t /Zi /Gm- /O2 /Ob2 /fp:precise /Zc:forScope /Gd /Oy- /Oi /MD /EHsc /Ot"
#else
#define COB_BLD_CFLAGS   "/Zi /O2 /Ob2 /Oi /Ot /Oy- /GL /Gm- /EHsc /MD /GS /fp:precise /Zc:wchar_t /Zc:forScope /Gd"
#endif
#define COB_BLD_LD       "link.exe"
#define COB_BLD_LDFLAGS  ""
#define COB_BLD_BUILD    "i686-pc-windows"
// */

#define LOCALEDIR        "C:\\locale"
