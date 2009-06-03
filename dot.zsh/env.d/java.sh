function nc:jar-packages () {
    : print sorted list of distinct packages of classes
    nc:jar-classes $1 \
    | sed -re 's/\.[^.]*?$//g' \
    | sort -u
}

function nc:jar-classes () {
    : print classes in package form
    jar -tf $1 \
    | grep '\.class$' \
    | sed -re 's!/!.!g' -e 's/\.class//g'
}

function nc:jar-manifest () {
    : cat the MANIFEST file, which contains the library version
    JAR=$(readlink -f $1)
    DIR=$(mktemp -d)
    (
    cd $DIR
    jar -xf $JAR META-INF
    cat META-INF/MANIFEST.MF
    )
    \rm -r "$DIR"
}

function nc:jar-meta-inf () {
    : cat all of META-INF
    JAR=$(readlink -f $1)
    DIR=$(mktemp -d)
    (
    cd $DIR
    jar -xf $JAR META-INF
    cd META-INF
    for f in $(ls); do
        echo "====[FILE $f]===="
        cat $f
    done
    )
    \rm -r "$DIR"
}
