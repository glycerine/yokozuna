#!/bin/bash
#
# Script to grab Solr and embed in priv dir.
#
# Usage:
#     ./grab-solr.sh

FROM_SRC=false

if [ $(basename $PWD) != "priv" ]
then
    cd priv
fi

case $1 in
    from-src)
        FROM_SRC=true
        shift
        ;;
    *)
        echo Invalid option $1
        exit 1
esac

if $FROM_SRC; then
    dir=$PWD/solr
    src_dir=$PWD/lucene-solr
    example_dir=$src_dir/solr/example
    patch_dir=$PWD/solr-patches
    branch=branch_4x
else
    dir=$PWD/solr
    src_dir=$PWD/apache-solr-4.0.0-alpha
    example_dir=$src_dir/example
fi

apply_patches()
{
    if [ -e $patch_dir ]; then
        echo "applying patches in $patch_dir"
        for p in $patch_dir/*.patch; do
            patch -p1 < $p
        done
    fi
}

build_solr()
{
    pushd $src_dir
    apply_patches
    ant compile
    pushd solr
    mkdir test-framework/lib
    ant dist example
    popd
    popd
}

checkout_branch()
{
    branch=$1
    pushd $src_dir
    git checkout $branch
    popd
}

check_for_solr()
{
    test -e $dir
}

get_solr()
{
    if $FROM_SRC; then
        git clone git://github.com/apache/lucene-solr.git
    else
        wget http://apache.deathculture.net/lucene/solr/4.0.0-ALPHA/apache-solr-4.0.0-ALPHA.tgz
        tar zxvf apache-solr-4.0.0-ALPHA.tgz
    fi
}

if check_for_solr
then
    echo "Solr already exists, exiting..."
    exit 0
fi

if [ ! -e $src_dir ]
then
    get_solr
fi

if $FROM_SRC; then
    checkout_branch $branch
    build_solr
fi

cp -vr $example_dir $dir
rm -rf $dir/{multicore,solr,README.txt}
# mkdir $dir/yokozuna
cp -v solr.xml $dir
cp -v *.properties $dir
