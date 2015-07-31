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

sddbdump () {
    file=snowdrift_production--$(date -Iminutes)
    ssh `sd-main-dns` sudo -u postgres pg_dump -Fc snowdrift_production \
        | cat > $file
    gpg -se -r wolftune -r 20068bfb $file
    echo ${file}.gpg | xsel -i -b
    echo ${file}.gpg copied to clipboard.
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
        read -p "Run tests? "
        if [ -n "$REPLY" ]
        then
            test=./test.sh
        else
            test=true
        fi
        if $test
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
