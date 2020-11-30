:: Copyright (C) 2014-2020 Free Software Foundation, Inc.
:: Written by Simon Sobisch, Edward Hart
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
:: x64 version

@echo off

:: the architecture to set / check
set arch=x64
set arch_full=amd64

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
   goto :gc
)

:vsvars

:: Check for valid MSC Environment and let it do it's work.
:: If not found try Windows SDKs in standard installation folders

:: Visual Studio 2017: no VS150COMNTOOLS globally or vsvars any more...
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat" (
   call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat" -arch=%arch_full%
   goto :gc
)
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Common7\Tools\VsDevCmd.bat" (
   call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Common7\Tools\VsDevCmd.bat" -arch=%arch_full%
   goto :gc
)
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools\VsDevCmd.bat" (
   call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools\VsDevCmd.bat" -arch=%arch_full%
   goto :gc
)
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\BuildTools\Common7\Tools\VsDevCmd.bat" (
   call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\BuildTools\Common7\Tools\VsDevCmd.bat" -arch=%arch_full%
   goto :gc
)

:: Visual Studio 2015
if exist "%VS140COMNTOOLS%vsvars64.bat" (
   call "%VS140COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS140COMNTOOLS%vcvarsqueryregistry.bat" (
   call "%VS140COMNTOOLS%vcvarsqueryregistry.bat"
)
if %errorlevel% equ 0 (
   if exist "%VCINSTALLDIR%vcvarsall.bat" (
       call "%VCINSTALLDIR%vcvarsall.bat" %arch%
       goto :gc
   )
)

:: Visual Studio 2013
if exist "%VS120COMNTOOLS%vsvars64.bat" (
   call "%VS120COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS120COMNTOOLS%vcvarsqueryregistry.bat" (
   call "%VS120COMNTOOLS%vcvarsqueryregistry.bat"
)
if %errorlevel% equ 0 (
   if exist "%VCINSTALLDIR%vcvarsall.bat" (
       call "%VCINSTALLDIR%vcvarsall.bat" %arch%
       goto :gc
   )
)

:: Visual Studio 2012
if exist "%VS110COMNTOOLS%vsvars64.bat" (
   call "%VS110COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS110COMNTOOLS%vcvarsqueryregistry.bat" (
   call "%VS110COMNTOOLS%vcvarsqueryregistry.bat"
)
if %errorlevel% equ 0 (
   if exist "%VCINSTALLDIR%vcvarsall.bat" (
       call "%VCINSTALLDIR%vcvarsall.bat" %arch%
       goto :gc
   )
)

:: Visual Studio 2010
if exist "%VS100COMNTOOLS%vsvars64.bat" (
   call "%VS100COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS100COMNTOOLS%vcvarsqueryregistry.bat" (
   call "%VS100COMNTOOLS%vcvarsqueryregistry.bat"
)
if %errorlevel% equ 0 (
   if exist "%VCINSTALLDIR%vcvarsall.bat" (
       call "%VCINSTALLDIR%vcvarsall.bat" %arch%
       goto :gc
   )
)

:: Visual Studio 2008
if exist "%VS90COMNTOOLS%vsvars64.bat" (
   call "%VS90COMNTOOLS%vsvars64.bat"
   goto :gc
)
if exist "%VS90COMNTOOLS%vcvarsqueryregistry.bat" (
   call "%VS90COMNTOOLS%vcvarsqueryregistry.bat"
)
if %errorlevel% equ 0 (
   if exist "%VCINSTALLDIR%vcvarsall.bat" (
       call "%VCINSTALLDIR%vcvarsall.bat" %arch%
       goto :gc
   )
)


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

:gc

:: check if cl.exe is already in path
where cl.exe 2>nul
if "%errorlevel%" == "0" (
   echo cl.exe now in PATH:
   cl.exe 1>nul
) else (
   echo ERROR: cl.exe not found!
)
echo finished.
