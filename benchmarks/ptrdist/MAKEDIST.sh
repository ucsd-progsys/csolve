#!/bin/sh

cd ..
tar cvf - `find ptrdist-$1 -name \[\^\_\]\* -type f -print` | compress > ptrdist-$1.tar.Z

