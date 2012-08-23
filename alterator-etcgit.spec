%define _altdata_dir %_datadir/alterator

Name: alterator-etcgit
Version: 1.1
Release: alt3

BuildArch: noarch

Source:%name-%version.tar

Summary: Alterator module to control versions of configuration files in /etc using git
License: GPL
Group: System/Configuration/Other
Requires: alterator >= 4.8-alt1
Requires: alterator-fbi >= 5.27-alt1
Requires: git-server
Conflicts: alterator >= 5.0
Conflicts: alterator-fbi >= 6.0

BuildPreReq: alterator rpm-macros-fillup rpm-macros-alterator

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
%_alterator_datadir/design
%_bindir/*
%_sbindir/*

%changelog
* Fri Aug 24 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt3
- Output the last (HEAD) commit message on plain `etcgit` call.
- Fix undefined "in_head" parameter (functions).

* Thu Aug 23 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt2
- Fix/update readonly operations of the etcgit tool.

* Wed Aug 22 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt1
- Refactor the module for working with the local repo copy.

* Fri Jul 20 2012 Paul Wolneykien <manowar@altlinux.ru> 1.0-alt1
- Initial draft release.
