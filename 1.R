# https://blog.csdn.net/weixin_48041573/article/details/117587608
# ### 线性规划 + 整数规划
# max z = 2x_1 + x_2
# s.t.
#     5 x_2 <= 15
#     6 x_1 + 2 x_2 <= 24
#       x_1 +   x_2 <= 5
#       x_1         >= 0
#               x_2 >= 0
library(lpSolve)
direction = 'max'
objective.vec = c(2,1)  #目标函数系数
a1 = c(0,5)   #每行约束条件中各变量的系数
a2 = c(6,2)
a3 = c(1,1)
a4 = c(1,0)
a5 = c(0,1)
a = rbind(a1,a2,a3,a4,a5)  #系数矩阵
a.dir = c(rep('<=',3),rep('>=',2))  #约束方向
a.rhs = c(15,24,5,0,0)  #约束值（等式右端）
# 线性规划
solution = lp(direction, objective.vec, a, a.dir, a.rhs,)
# 整数规划
# solution = lp(direction, objective.vec, a, a.dir, a.rhs,all.int ='TURE')
solution$solution  #变量的值
solution$objval  #目标函数的值



## 运输问题
library(lpSolve)
M = 100000000
a1 = c(41,27,28,24,0)
a2 = c(40,29,M,23,0)
a3 = c(37,30,27,21,0)
a = rbind(a1,a2,a3)  # 运费
col.rhs = c(20,30,30,40,75)  # 最后一行，总需求
col.dir = rep('=',5)
row.rhs = c(75,75,45)  #最后一列，总供给
row.dir = rep("=",3)
solution = lp.transport(a, row.signs = row.dir, row.rhs = row.rhs, col.signs = col.dir, col.rhs = col.rhs)
solution$solution


## 指派问题
library(lpSolve)
M = 100000000
a1 = c(820,810,840,960,0)
a2 = c(820,810,840,960,0)
a3 = c(800,870,M,920,0)
a4 = c(800,870,M,920,0)
a5 = c(740,900,810,840,M)
a = rbind(a1,a2,a3,a4,a5)  # 表中数据
solution = lp.assign(a)
solution$solution


## 非线性规划
# min x1^2 + x2^2 + 8
# s.t. x1        <= 4
#      x1^2 - x2 >= 0
#     -x1 - x2^2 + 2 = 0
#      x1, x2 >= 0
library(nloptr) 
eval_f = function(x) # 目标方程
  {return(list('objective'=x[1]^2 + x[2]^2 + 8,
               'gradient'=c(2*x[1],2*x[2])))}  # 求偏导
eval_g = function(x) # 不等式约束，默认 <=0
  {return(list('constraints' = c(x[2]-x[1]^2, x[1]-4),
              'jacobian' = rbind(c(-2*x[1],1),c(1,0))))} # 求偏导
eval_h = function(x) # 等式约束   # 有等式约束才用
  {return(list('constraints' = c(-x[1]-x[2]^2+2),
              'jacobian' = rbind(c(-1,-2*x[2]))))} # 求偏导
res3 = nloptr(x0 = c(2,0),
              eval_f = eval_f,
              lb = c(0,0), # x下限
              ub = c(4,Inf), # x上限
              eval_g_ineq = eval_g,
              eval_g_eq = eval_h, # 有等式约束才用
              opts = list('algorithm'='NLOPT_LD_SLSQP'))
res3$solution

## 图论
# 最小路
library('igraph')
library('magrittr')
a1 = c(0,2,5,4,rep(0,3))  # 给每一个点标号，例如：3->4的权是1，没有链接、自己与自己链接为0
a2 = c(2,0,2,0,7,0,0)
a3 = c(5,2,0,1,4,3,0)
a4 = c(4,0,1,0,0,4,0)
a5 = c(0,7,4,0,0,1,5)
a6 = c(0,0,3,4,1,0,7)
a7 = c(0,0,0,0,5,7,0)
a = rbind(a1,a2,a3,a4,a5,a6,a7)
g = graph.adjacency(a, mode = c('undirected'), weighted = TRUE)  #生成图,无向，数字为权重,否则当作多重边处理
shortest.paths(g,1,7)   # 1到7的最短路
## 最小生成树
minimum.spanning.tree(g)  #有权重
minimum.spanning.tree(g, algorithm = 'unweighted')  #无权重

# 最大流
library('igraph')
#输入的数字为流量限制，自己到自己标为0，无法到达标为0，逆流标0
r1 = c(0,5,4,3,rep(0,4))    
r2 = c(rep(0,4),5,3,0,0)
r3 = c(rep(0,5),3,2,0)
r4 = c(rep(0,6),2,0)
r5 = c(0,5,rep(0,5),4)
r6 = c(rep(0,7),3)
r7 = c(rep(0,7),5)
r8 = rep(0,8)
adj = rbind(r1,r2,r3,r4,r5,r6,r7,r8)
g = graph.adjacency(adj, mode = c('directed'), weighted = TRUE) 
vcount(g)  #顶点数
ecount(g)  #边数
E(g)  #边的方向
E(g)$weight  #表示每条边的最大承载力（流量限制）
flow1 = graph.maxflow(g,1,8,E(g)$weight)  
flow1
