#!/bin/bash
# Author: Kitlei Róbert <kitlei@elte.hu>

# Use the "--help" option to print the help.

if [ "$#" == "0" -o "$1" = "--help" -o "$1" = "-h" ]
then
    cat <<END
NAME
    build_package.sh

USAGE
    build_package.sh release-name

DESCRIPTION
    This script builds a Debian release package into the tags directory.

ARGUMENTS
    The first argument has to be the version of the release, e.g. 0.7.
END
    exit 0
fi


# Compiling RefactorErl

echo Compiling RefactorErl
cd ..
make
cd bin

# Infos

VERSIONINFO="$1"
PACKAGENAME="refactorerl-emacs"
RELEASEDIR="../../../tags/$PACKAGENAME-$VERSIONINFO-1"


# Preparations

echo Removing previous contents of $RELEASEDIR
rm -rf $RELEASEDIR

echo Creating directories under $RELEASEDIR
mkdir -p $RELEASEDIR/DEBIAN
mkdir -p $RELEASEDIR/etc/emacs/site-start.d
mkdir -p $RELEASEDIR/usr/lib/emacsen-common/packages/install
mkdir -p $RELEASEDIR/usr/lib/emacsen-common/packages/remove
mkdir -p $RELEASEDIR/usr/share/$PACKAGENAME/lib/refactorerl/ebin
mkdir -p $RELEASEDIR/usr/share/$PACKAGENAME/lib/refactorerl/priv
mkdir -p $RELEASEDIR/usr/share/$PACKAGENAME/bin
mkdir -p $RELEASEDIR/usr/share/emacs/site-lisp/$PACKAGENAME
mkdir -p $RELEASEDIR/usr/share/doc/$PACKAGENAME


# Debian configuration files

ACTUALFILE="DEBIAN/control"
echo Adding $ACTUALFILE
echo  \
"Package: $PACKAGENAME
Version: $VERSIONINFO
Section: editors
Priority: optional
Architecture: all
Depends: erlang, emacs22 | emacsen
Conflicts:
Replaces:
Maintainer: RefactorErl Team <refactorerl@plc.inf.elte.hu>
Description: RefactorErl is a refactoring tool for Erlang.
 This package contains an Emacs interface to RefactorErl." \
> $RELEASEDIR/$ACTUALFILE



ACTUALFILE="DEBIAN/conffiles"
echo Adding $ACTUALFILE
echo "/etc/emacs/site-start.d/50$PACKAGENAME.el" \
> $RELEASEDIR/$ACTUALFILE


ACTUALFILE="DEBIAN/postinst"
echo Adding $ACTUALFILE
echo \
"#!/bin/sh
set -e
# Automatically added by dh_installemacsen
if [ "$1" = "configure" ] && [ -x /usr/lib/emacsen-common/emacs-package-install ]
then
    /usr/lib/emacsen-common/emacs-package-install $PACKAGENAME
fi
# End automatically added section" \
> $RELEASEDIR/$ACTUALFILE
chmod 0755 $RELEASEDIR/$ACTUALFILE


ACTUALFILE="DEBIAN/prerm"
echo Adding $ACTUALFILE
echo \
"#!/bin/sh
set -e
# Automatically added by dh_installemacsen
if [ -x /usr/lib/emacsen-common/emacs-package-remove ] ; then
    /usr/lib/emacsen-common/emacs-package-remove $PACKAGENAME
fi
# End automatically added section" \
> $RELEASEDIR/$ACTUALFILE
chmod 0755 $RELEASEDIR/$ACTUALFILE



# Program files

ACTUALFILE="/etc/emacs/site-start.d/50$PACKAGENAME.el"
echo Adding $ACTUALFILE
echo \
";; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux $PACKAGENAME package
;;
;; Originally contributed by RefactorErl Team <refactorerl@plc.inf.elte.hu>

(setq refactorerl-base-path \"/usr/share/$PACKAGENAME\")
(setq refactorerl-server-shell (quote shell))

(add-to-list 'load-path \"/usr/share/emacs/site-lisp/$PACKAGENAME\")

(require 'refactorerl)
" \
> $RELEASEDIR/$ACTUALFILE



ACTUALFILE="/usr/lib/emacsen-common/packages/install/$PACKAGENAME"
echo Adding $ACTUALFILE
cp package_files/refactorerl-install $RELEASEDIR/$ACTUALFILE


ACTUALFILE="/usr/lib/emacsen-common/packages/remove/$PACKAGENAME"
echo Adding $ACTUALFILE
cp package_files/refactorerl-remove $RELEASEDIR/$ACTUALFILE


# RefactorErl files

echo Copying RefactorErl Erlang files
cp ../lib/refactorerl/ebin/*.beam $RELEASEDIR/usr/share/$PACKAGENAME/lib/refactorerl/ebin/
cp ../lib/refactorerl/ebin/*.app $RELEASEDIR/usr/share/$PACKAGENAME/lib/refactorerl/ebin/
cp ../lib/refactorerl/priv/* $RELEASEDIR/usr/share/$PACKAGENAME/lib/refactorerl/priv/

echo Copying RefactorErl Emacs files
cp ../lib/refactorerl/emacs/*.el $RELEASEDIR/usr/share/emacs/site-lisp/$PACKAGENAME/

echo Copying RefactorErl execution files
cp ref* $RELEASEDIR/usr/share/$PACKAGENAME/bin/

echo Copying RefactorErl configuration files
cp ../sys.config $RELEASEDIR/usr/share/$PACKAGENAME/
cp ../refactorerl.boot $RELEASEDIR/usr/share/$PACKAGENAME/
cp ../refactorerl.rel $RELEASEDIR/usr/share/$PACKAGENAME/
cp ../refactorerl.script $RELEASEDIR/usr/share/$PACKAGENAME/


# Documentation files

echo Copying copyright file
cp package_files/copyright $RELEASEDIR/usr/share/doc/$PACKAGENAME/copyright

ACTUALFILE="/usr/share/doc/$PACKAGENAME/changelog"
echo Adding $ACTUALFILE
echo  \
"refactorerl ($VERSIONINFO-1) unstable; urgency=low

  * Initial Release.

 -- RefactorErl Team <refactorerl@plc.inf.elte.hu>  Wed Jun 17 10:53:14 CEST 2009
" \
> $RELEASEDIR/$ACTUALFILE
echo Compressing $ACTUALFILE
gzip -9 $RELEASEDIR/$ACTUALFILE


# Making md5sums

echo Making md5sums
cd $RELEASEDIR
find -type f -exec md5sum "{}" \; | sed -e "s#\./##g" | grep -v DEBIAN > DEBIAN/md5sums
cd - >/dev/null


# Changing owner of files to root

echo Changing all files to root/root
echo This step might require the root password
cd $RELEASEDIR
for i in `find`; do
    sudo chown root $i
    sudo chgrp root $i
done
cd - >/dev/null

# Making Debian package

echo Making Debian package
cd $RELEASEDIR/..
dpkg -b $PACKAGENAME-$VERSIONINFO-1
cd - >/dev/null


# Running Lintian

echo Checking created package with Lintian
cd $RELEASEDIR/..
lintian $PACKAGENAME-$VERSIONINFO-1.deb
cd - >/dev/null


# Reverting file owner to current user

WHOAMI=`whoami`
echo Changing all files back to $WHOAMI/$WHOAMI
cd $RELEASEDIR
for i in `find`; do
    sudo chown $WHOAMI $i
    sudo chgrp $WHOAMI $i
done
cd - >/dev/null
