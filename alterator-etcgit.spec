%define _altdata_dir %_datadir/alterator

Name: alterator-etcgit
Version: 1.0
Release: alt1

BuildArch: noarch

Source:%name-%version.tar

Summary: Alterator module to control versions of configuration files in /etc using git
License: GPL
Group: System/Configuration/Other
Requires: alterator >= 4.8-alt1
Requires: alterator-fbi >= 5.11-alt2
Conflicts: alterator >= 5.0
Conflicts: alterator-fbi >= 6.0

BuildRequires: alterator rpm-macros-fillup

%description
Alterator module to control versions of configuration files in /etc
using git.

%prep
%setup -q

%build
%make_build

%install
%makeinstall

%files
%_alterator_datadir/applications/*
%_alterator_datadir/ui/etcgit
%_alterator_backend3dir/etcgit

%changelog
* Fri Jul 20 2012 Paul Wolneykien <manowar@altlinux.ru> 1.0-alt1
- Initial release
