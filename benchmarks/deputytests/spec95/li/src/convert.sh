for fn in `cat allcfiles `; do echo "--------- $fn --------"; apply ./unvararg.pl $fn; done
