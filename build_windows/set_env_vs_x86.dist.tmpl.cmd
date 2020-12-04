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

:: cmd script for setting GnuCOBOL environment in Windows with MSC compiler
:: wrapper that calls gcvars.cmd - x86/win32 dist version

@echo off

:: the architecture to set / check
set "arch=x86"
set "arch_full=x86"

:: mode for compiling in the source-tree
set "source_build=Win32"
set "source_config="

:: new cmd to stay open if not started directly from cmd.exe window
:: Note: CI is defined in Appveyor, Travis and other CI-environments
echo %cmdcmdline% | find /i "%~0%CI%" >nul
if %errorlevel% equ 0 (
  set "stay_open=x"
)

call %~dp0gcvsvars.cmd %*
