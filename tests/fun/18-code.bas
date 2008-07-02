10 cls
20 let c$="":print
30 print "type in your message"
40 input m$:let l=len(m$)
50 if int(l/2)<>l/2 then let m$=m$+" "
60 for k=1 to len(m$) step 2
70 let c$=c$+mid$(m$,k+1,1)
80 let c$=c$+mid$(m$,k,1)
90 next k
100 cls
110 print:print "code message:"
120 print:print c$
