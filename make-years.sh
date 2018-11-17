
wget http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-1.gz
wget http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-2.gz
wget http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-2gram-20120701-ye.gz
gunzip *.gz

grep -P "^.[0-9]{3}\\t" google*1 > googlebooks-1xxx
grep -P "^.0(0|1)[0-9]\\t" google*2 > googlebooks-2xxx
cat googlebooks-1xxx googlebooks-2xxx > googlebooks-years

grep -P "^year (1|2)[0-9[0-9][0-9]\\t" google*ye > googlebooks-years-robust