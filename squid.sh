#!/bin/bash
# Squid.sh. Scrape all patterns from squidfingers.com
# TODO: Move this to ~/bin (and think about `git`ing that?)

for i in {001..158}; do
  wget http://www.squidfingers.com/_patterns/files/pattern_${i}.zip;
  unzip patter_${i}.zip;
  rm pattern_${i}.zip;
done
