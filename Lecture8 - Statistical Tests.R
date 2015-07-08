##########################
### '150708. lecture 8 ###
##########################

#c.i. for mu(pop. mean) : Pr(mu-2sigma < xbar < mu+2sigma)
pnorm(2)-pnorm(-2)


# hyphothesis errors
# reject h0 when h0 is true = type 1 error
# accept h0 when h1 is true = type 2 error


# one sample t test example

t.value = (2.09 - 2) / (1.644/sqrt(175))
t.value
p.value = pt(t.value, df=(175-1))
p.value



# two sample t test example

control = c(91,87,99,77,88,91)
treat = c(101,110,103,93,99,104)

t.test(control,treat, alternative='less', var.equal=T)
