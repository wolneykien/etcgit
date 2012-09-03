%define _altdata_dir %_datadir/alterator

Name: alterator-etcgit
Version: 1.1
Release: alt6

BuildArch: noarch

Source:%name-%version.tar

Summary: Alterator module to control versions of configuration files in /etc using git
License: GPL
Group: System/Configuration/Other
Requires: alterator >= 4.20-alt1.1
Requires: alterator-fbi >= 5.27-alt1
Requires: git-server
Conflicts: alterator >= 5.0
Conflicts: alterator-fbi >= 6.0

BuildPreReq: alterator rpm-macros-fillup rpm-macros-alterator

%description
Alterator module to control versions of configuration files in /etc
using git.

%package -n etcgit
Summary: A tool to control versions of configuration files in /etc using git
License: GPL
Group: System/Configuration/Other

%description -n etcgit
A tool to control versions of configuration files in /etc using git.
Currently is used by the "etcgit" Alterator module.

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

%files -n etcgit
%_bindir/*
%_sbindir/*

%changelog
* Fri Aug 31 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt6
- Use X-Alterator-Monitoring-Control category.

* Wed Aug 29 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt5
- Do not walk-up the paths to .gitignore and the other auxiliary
  files. Thus, do not restart extra services by accident.

* Tue Aug 28 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt4
- Implement asynchronous 'switch' command.

* Fri Aug 24 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt3
- Output the last (HEAD) commit message on plain `etcgit` call.
- Fix undefined "in_head" parameter (functions).

* Thu Aug 23 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt2
- Fix/update readonly operations of the etcgit tool.

* Wed Aug 22 2012 Paul Wolneykien <manowar@altlinux.ru> 1.1-alt1
- Refactor the module for working with the local repo copy.

* Fri Jul 20 2012 Paul Wolneykien <manowar@altlinux.ru> 1.0-alt1
- Initial draft release.
