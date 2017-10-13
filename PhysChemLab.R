#虽然大概除了我以外没人同时用R LaTeX还要写物化实验报告==
#本文件包括三个R函数，可以自动生成计算标准差、合成不确定度以及平均值的LaTeX公式代码。
#示例：
# PhysChemLab_sigma(y="y",x="x",xs=c(1,2,3,4),xmean=2.5)
# PhysChemLab_sigma2(y = "y",
#                    expression = "x_1+x_2",
#                    x = c("x_1","x_2")
# )
# PhysChemLab_ave(y="y",x="x",xs=c(1,2,3,4))

PhysChemLab_sigma <- function(y,x,xs,xmean){
  output <- paste("$$","\\sigma_{",y,"}",
                  "=\\sqrt{\\frac{\\Sigma (",x,"_i-\\bar{",x,"})^2}{n-1}}",
                  "=\\sqrt{\\frac{"
  )
  for(i in xs[1:(length(xs)-1)]){
    output <- paste(output,"(",i,"-",xmean,")^2","+")
  }
  output <- paste(output,"(",xs[length(xs)],"-",xmean,")^2}{",length(xs),"-1}} $$")
  cat(output)
}


PhysChemLab_sigma2 <- function(y,expression,x){
  output <- paste("$$","\\sigma_{",y,"}=",
                  "\\sqrt{"
  )
  for(i in x[1:(length(x)-1)]){
    output <- paste(output,"[",
                    "\\frac{\\partial (",expression,")}{\\partial",i,"}",
                    "\\sigma_{",i,"}",
                    "]^2",
                    "+")
  }
  output <- paste(output,"[",
                  "\\frac{\\partial (",expression,")}{\\partial",x[length(x)],"}",
                  "\\sigma_{",x[length(x)],"}",
                  "]^2"
  )
  output <- paste(output,"}","$$")
  cat(output)
}

PhysChemLab_ave <- function(y,x,xs){
  output <- paste("$$","\\bar{",y,"}",
                  "=\\frac{\\Sigma (",x,"_i)}{n}",
                  "=\\frac{"
  )
  for(i in xs[1:(length(xs)-1)]){
    output <- paste(output,i,"+")
  }
  output <- paste(output,xs[length(xs)],"}{",length(xs),"} $$")
  cat(output)
}
