# RPM spec file for gnucobol
# Adjust/activate "Packager" tag as necessary

Name:		gnucobol
Version:	4.0
Release:	1%{?dist}
Summary:	GnuCOBOL - COBOL compiler and runtime library

# Packager:	Whoever

Group:		Development/Languages/Other
License:	GPLv3+/LGPLv3+

URL:		https://www.gnu.org/software/gnucobol/
Source:		https://ftp.gnu.org/gnu/%{name}/%{name}-%{version}.tar.gz

BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires:	db-devel >= 4.1.24
BuildRequires:	ncurses-devel >= 5.4

Requires:		gcc
Requires:		glibc-devel
Requires:		gmp-devel >= 4.1.4
Requires:		db >= 4.1.24
Requires:		ncurses >= 5.4

Requires(post): /sbin/install-info

%description
GnuCOBOL is a free, modern COBOL compiler. GnuCOBOL implements a substantial part of the COBOL 85,
COBOL 2002 and COBOL 2014 standards, as well as many extensions included in other COBOL compilers.

GnuCOBOL translates COBOL into C and compiles the translated code using a native C compiler.

%prep
%setup -q -n %{name}-%{version}

%build
%configure --disable-rpath
make

%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT
rm -rf $RPM_BUILD_ROOT/%{_infodir}/dir

%find_lang %{name}

%check
make check

%files -f %{name}.lang
%license COPYING
%license COPYING.DOC
%defattr (-,root,root,-)
%doc AUTHORS ChangeLog
%doc NEWS README THANKS
%doc DEPENDENCIES HACKING
%{_bindir}/cobc
%{_bindir}/cobcrun
%{_bindir}/cob-config
%{_includedir}/*
%{_datadir}/gnucobol
%{_infodir}/gnucobol.info*
%{_mandir}/man1/cobc.1.*
%{_mandir}/man1/cobcrun.1.*

%files -n libcob
%license COPYING.LESSER
%{_libdir}/libcob.so*
%{_libdir}/libcob.a
%{_libdir}/libcob.la
%{_libdir}/gnucobol/CBL_OC_DUMP.so

%clean
rm -rf $RPM_BUILD_ROOT
rm -rf $RPM_BUILD_DIR/%{name}-%{version}

%post 
/sbin/install-info %{_infodir}/gnucobol.info %{_infodir}/dir 2>/dev/null || :
/sbin/ldconfig

%postun 
if [ $1 = 0 ]; then
  /sbin/install-info --delete %{_infodir}/gnucobol.info %{_infodir}/dir 2>/dev/null || :
fi
/sbin/ldconfig

%changelog
