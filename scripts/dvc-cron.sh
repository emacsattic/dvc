#! /bin/bash
# arch-tag: 66ab59c1-6281-4c11-9b5f-bf95fff9c8bb

# Generates the tarball and html documentation, upload it to gna.org
# This file is currently used only by Matthieu MOY, and is provided
# here only as an example. Copy it and modify it if you wish to use it.

cd `dirname $0`/..
mkdir -p tmp
exec > tmp/dvc-cron.log
make tarball
cp dvc-snapshot.tar.gz www/
make -C texinfo dvc.html
cp texinfo/dvc.html docs/dvc-snapshot.html

# upload source and non-source at the same time
rsync -av www/ moy@download.gna.org:/upload/dvc/
