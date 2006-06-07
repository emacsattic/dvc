#! /bin/sh
# arch-tag: ce2bf941-9309-4ac5-aa7d-c75186c26767

if [ $# -ne 1 ]
then
    echo "usage: $(basename $0) string"
    exit 1
fi

perl -pi -e "s/tla-($1)/dvc-\$1/g" *.el
