
# coding: utf-8

# ### calcuclate value of European Call Option using Monte Carlo Simulation
# 

# In[26]:


s0=100
k=105
T=1
r=0.05
sigma=0.2


# In[18]:


from numpy import *


# In[19]:


I=1000000
#print random varable from standard normal distribution
z=random.standard_normal(I)
z


# In[20]:


#calculate future stock price based on all parameters
#using Black-Scholes-Merton Index level at maturity function

St=s0*exp((r-0.5*sigma**2)*T+sigma*sqrt(T)*z)

#ht is all inner values of the option at maturity
ht=maximum(St-k,0)
Co=exp(-r*T)*sum(ht)/I


# In[25]:


print("Value of the European Call Option", Co)


# #### Python snytax is indeed quite close to the mathematical syntax, when it comes to the parameter value assignments
# 
