:: Copyright (C) 2014-2020 Free Software Foundation, Inc.
:: Written by Simon Sobisch
::
:: This file is part of GnuCOBOL.
::
:: The GnuCOBOL compiler is free software: you can redistribute it
:: and/or modify it under the terms of the GNU General Public License
:: as published by the Free Software Foundation, either version 3 of the
:: License, or (at your option) any later version.
::
:: GnuCOBOL is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.
::
:: You should have received a copy of the GNU General Public License
:: along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.

:: Batch for preparing windows binary distribution folder
:: By default, binaries use Release executable. To distribute a debug
:: distributable (y tho), provide DEBUG as an argument.

:: Batch for setting GnuCOBOL Environment in Windows with MSC compiler
:: x86/win32 version

@echo off

:: the architecture to set / check
set arch=x86
set arch_full=x86

:: restore old PATH to not expand it endlessly
if not "%COB_OLD_PATH%" == "" (
   set "PATH=%COB_OLD_PATH%"
   set "COB_OLD_PATH=%PATH%"
)


echo Setup Visual Studio (%arch%/%arch_full%)...
echo.

:: check if cl.exe with matching architecture is already in path
where cl.exe 1>nul 2>nul
if not "%errorlevel%" == "0" (
   goto :vsvars
)
cl.exe 2>&1 | findstr %arch% > nul
if "%errorlevel%" == "0" (
   echo cl.exe already in PATH
   echo no further initialization is done for the C compiler
   echo.
   goto :setup_gc
)

:vsvars

:: Check for valid MSC Environment and let it do it's work.
:: If not found try Windows SDKs in standard installation folders

@echo on

:: Visual Studio 2017 and newer: no VS150COMNTOOLS globally or vsvars any more...
:: check if available, otherwise check on
set "found="
for %%v in (2019 2017) do (
   if not [%found%] == []  goto :eof
   call :vsvars_current %%v
)
if not [%found%] == [] (
   call "%found%" -arch=%arch_full%
   goto :setup_gc
)

:: Visual Studio 2015, 2013, 2012, 2010, 2008
for %%v in ("%VS140COMNTOOLS%" "%VS120COMNTOOLS%" "%VS110COMNTOOLS%" "%VS100COMNTOOLS%" "%VS90COMNTOOLS%") do (
   if not [%found%] == []  goto :eof
   call :vsvars_old %%v
)
if not [%found%] == [] (
   call %found%vsvars32.bat
   goto :setup_gc
)

goto :sdk_setup

:vsvars_current
for %%v in (BuildTools Community Professional Enterprise) do (
  if not [%found%] == []  goto :eof
  if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\%1\%%v\Common7\Tools\VsDevCmd.bat" (
     set "found=%ProgramFiles(x86)%\Microsoft Visual Studio\%1\%%v\Common7\Tools\VsDevCmd.bat"
  )
)
goto :eof

:vsvars_old
if exist %%1%vsvars32.bat  set "found=%1"
goto :eof

:sdk_setup
echo Warning: Not possible to set %arch_full% environment for Microsoft Visual Studio!

:: Windows SDK 10 (Windows 10 / VS 2015 compiler) - untested
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v10\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v10\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v10\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v10\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
:: Windows SDK 8.1 (Windows 8.1 and .NET 4.5.1 / VS 2013 compiler)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v8.1\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
:: Windows SDK 8.0 (Windows 8 and .NET 4.5 / VS 2012 compiler)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v8.0\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
:: Windows SDK 7.1 (Windows 7 and .NET 4 / VS 2010 SP1 compiler)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
:: Windows SDK 7.0 (Windows 7 and .NET 3.5 SP1 / VS 2008 compiler)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v7.0\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
:: Windows SDK 6.1 (Windows 2008 Server and .NET 3.5 / VS 2008 compiler)
if exist "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)
if exist "%ProgramFiles%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" (
   call "%ProgramFiles%\Microsoft SDKs\Windows\v6.1\Bin\SetEnv.Cmd" /%arch% /release
   goto :gcc
)

color 0C
echo Warning: Not possible to set %arch_full% environment for Microsoft Windows SDK!

:gcc
color 07

:setup_gc

:: check if cl.exe is already in path
echo cl.exe now in PATH:
where cl.exe 2>nul
if "%errorlevel%" == "0" (
   echo.
   cl.exe 1>nul
) else (
   echo ERROR: cl.exe not found!
)

echo.
echo.
:: Now the stuff for GnuCOBOL
echo Setting environment for GnuCOBOL.

:: Get the main dir from the batch's position
set "COB_MAIN_DIR=%~dp0../"

:: Set the necessary folders for cobc
set "COB_CONFIG_DIR=%COB_MAIN_DIR%config"
set "COB_COPY_DIR=%COB_MAIN_DIR%copy"

set "LOCALEDIR=%COB_MAIN_DIR%locale"

:: Set the necessary options for MSC compiler
set "COB_CFLAGS=/I "%COB_MAIN_DIR%include""
set "COB_LIB_PATHS=/LIBPATH:"%COB_MAIN_DIR%lib""
::if "%COB_LIBS%"       EQU "" (
::   if exist "%COB_MAIN_DIR%lib\mpir.lib"	set COB_LIBS=libcob.lib mpir.lib
::   if exist "%COB_MAIN_DIR%lib\libgmp.lib" 	set COB_LIBS=libcob.lib libgmp.lib
::)

:: save current PATH and add the bin path of GnuCOBOL to PATH for further references
if "%COB_OLD_PATH%" == "" (
   set "COB_OLD_PATH=%PATH%"
)
set "PATH=%COB_MAIN_DIR%bin;%PATH%"

echo finished.
