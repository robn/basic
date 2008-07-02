10 cls
20 let x=10:let y=10
30 let a$=inkey$
40 if a$="" then goto 30
50 if a$="a" then let y=y-1
60 if a$="z" then let y=y+1
70 if a$="n" then let x=x-1
80 if a$="m" then let x=x+1
90 print tab(x,y);"*"
100 goto 30
