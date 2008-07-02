10 cls
20 let n=0:print:print:print
30 let x=int(rnd(1)*10+1)
40 let y=int(rnd(1)*10+1)
50 print x;"  ";y;
60 let a$=inkey$
70 let n=n+1
80 if a$="s" and x=y then goto 110
90 if n<=100 then goto 60
100 goto 10
110 print " well done"
120 for k=1 to 1000:next k
130 goto 10
