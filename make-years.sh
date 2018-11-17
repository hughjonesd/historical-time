
grep -P "^.[0-9]{3}\\t" google*1 > googlebooks-1xxx
grep -P "^.0(0|1)[0-9]\\t" google*2 > googlebooks-2xxx
cat googlebooks-1xxx googlebooks-2xxx > googlebooks-years