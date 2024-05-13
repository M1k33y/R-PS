#problma mecanici

l1=4
l2=12

p1=1/4
p2=3/4

N=10000
s=0
for (i in 1:N)
  {
  x=p1*rexp(1,l1)+p2*rexp(1,l2)
  s=s+x
  }
expect=s/N
print(expect)