#!/bin/sh -e

# only execute anything if either
# - running under orb with package = builder-web
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "modulectomy" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/bin
debiandir=$rootdir/DEBIAN
libexecdir=$rootdir/usr/libexec

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$debiandir" "$bindir" "$libexecdir" 

install "$bdir/modulectomy" "$bindir/modulectomy"

ARCH=$(dpkg-architecture -q DEB_TARGET_ARCH)
sed -i -e "s/^Architecture:.*/Architecture: ${ARCH}/" "$debiandir"/control

dpkg-deb --build "$rootdir" "$basedir"/builder-web.deb
echo 'bin: [ "builder-web.deb" ]' > "$basedir/builder-web.install"
