:: Batch for setting GNU Cobol Environment in Windows with MSC compiler
:: win32 version

@echo off

:: Check if this batch was called already and exit if it was called before.
::if "%COB_ENV_SET%"    NEQ "" goto :eof

:: Set the internal env var
set COB_ENV_SET=1

:: Check for valid MSC Environment and let it do it's work.
:: If not found try Windows SDKs in standard installation folders
if exist "%VS120COMNTOOLS%vsvars32.bat" (
   call "%VS120COMNTOOLS%vsvars32.bat"
   goto :gc
)
if exist "%VS110COMNTOOLS%vsvars32.bat" (
   call "%VS110COMNTOOLS%vsvars32.bat"
   goto :gc
)
if exist "%VS100COMNTOOLS%vsvars32.bat" (
   call "%VS100COMNTOOLS%vsvars32.bat"
   goto :gc
)
if exist "%VS90COMNTOOLS%vsvars32.bat" (
   call "%VS90COMNTOOLS%vsvars32.bat"
   goto :gc
)
if exist "%VS80COMNTOOLS%vsvars32.bat" (
   call "%VS80COMNTOOLS%vsvars32.bat"
   goto :gc
)
if exist "%VS71COMNTOOLS%" NEQ "" (
   call "%VS71COMNTOOLS%vsvars32.bat"
   goto :gc
)
if exist "%VS70COMNTOOLS%vsvars32.bat" (
   call "%VS70COMNTOOLS%vsvars32.bat"
   goto :gc
)

echo Warning: Not possible to set environment for Microsoft Visual Studio!
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" /x86 /release
   goto :gcc
)

color 0C
echo Warning: Not possible to set environment for Microsoft Windows SDK!

:gcc
color 07

:gc

echo.
echo.
:: Now the stuff for GNU Cobol
echo Setting environment for GNU Cobol.

:: Get the main dir from the batch's position (only works in NT environments)
set COB_MAIN_DIR=%~dp0

:: Set the necessary folders for cobc
set COB_CONFIG_DIR=%COB_MAIN_DIR%\config
set COB_COPY_DIR=%COB_MAIN_DIR%\copy

SET LOCALEDIR=%COB_MAIN_DIR%\po

:: Set the necessary options for MSC compiler
set COB_CFLAGS=/I "%COB_MAIN_DIR%include"
set COB_LIB_PATHS=/LIBPATH:"%COB_MAIN_DIR%lib"
::if "%COB_LIBS%"       EQU "" (
::   if exist "%COB_MAIN_DIR%lib\mpir.lib"	set COB_LIBS=libcob.lib mpir.lib
::   if exist "%COB_MAIN_DIR%lib\libgmp.lib" 	set COB_LIBS=libcob.lib libgmp.lib
::)

:: Add the bin path of GNU Cobol to PATH for further references
set PATH=%COB_MAIN_DIR%bin;%PATH%
