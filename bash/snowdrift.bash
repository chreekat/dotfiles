gg () {
    xdg-open https://git.gnu.io/snowdrift/snowdrift/merge_requests/$1
}

sd () {
    xdg-open https://snowdrift.coop/p/snowdrift/t/$1
}

sd-main-dns () {
    aws --profile snowdrift \
        ec2 describe-instances --instance-ids i-81a6df28 \
        --query 'Reservations[].Instances[].PublicDnsName'
}

sdpush () {
    git push && git push github
}

sdsyncgithub () {
    git pull && git push github
}

sddbdump () {
    # This is almost ISO 8601 format, except tar and bash don't like ':' in
    # filenames. Replaced with '.'.
    stamp=$(date +%FT%H.%m%z)

    prodfile=snowdrift_production--${stamp}
    mxfile=snowdrift_mx1--${stamp}
    dumpfile=$(pwd)/snowdrift_dbs--${stamp}.tar.gz

    ssh `sd-main-dns` sudo -u postgres pg_dump -Fc snowdrift_production \
        | cat > $prodfile
    ssh mx1.snowdrift.coop /home/ubuntu/civicrm-dump.sh \
        | cat > $mxfile
    tar czf $dumpfile $prodfile $mxfile
    gpg -se -r wolftune -r 20068bfb $dumpfile
    rm $prodfile $mxfile $dumpfile
    echo ${dumpfile}.gpg | xsel -i -b
    echo ${dumpfile}.gpg copied to clipboard.
}

_sdmaybetest () {
    res=0
    read -p "Run tests? " a
    if [ "$a" = "y" ]
    then
        make test
        res=$?
    fi
    return $res
}
sdmr () {
    ref=$(git symbolic-ref HEAD)
    branch=${ref##refs/heads/}
    read -r -d '' url <<-EOF
	https://git.gnu.io/chreekat/snowdrift/merge_requests/new?\
	merge_request%5Bsource_branch%5D=${branch}&\
	merge_request%5Bsource_project_id%5D=23&\
	merge_request%5Btarget_branch%5D=master&\
	merge_request%5Btarget_project_id%5D=5
	EOF
    if [ -n "$branch" ]
    then
        if _sdmaybetest
        then
            git push -u ck-gitlab &&
            xdg-open "$url"
        fi
    else
        2>&1 echo "Thou shouldst be upon a named branch, my son."
    fi
}

sdpr () {
    xdg-open https://github.com/snowdriftcoop/snowdrift/pull/$1
}

sddeploy () {
    sdpush &&
    touch src/Settings/StaticFiles.hs &&
    ./keter.sh
}

sdtest () {
    doit () {
        stack build $@ && stack test $@
    }
    if [ "$1" = "merge" ]
    then
        doit --flag Snowdrift:-dev --flag Snowdrift:merge
    else
        doit $@
    fi
}

mypush () {
    git push && git push chreekat
}

yesim () {
    vim $@ -S ~/.vim/yesod.vim
}

sd-filelink () {
    xdg-open https://git.gnu.io/snowdrift/snowdrift/tree/master/${1}\#L${2}
}
