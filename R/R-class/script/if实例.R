#if
#else要跟在}后不能换行
#eg1
a <- 3 
if( a == 1) { 
        print("a == 1") 
}else { print("a != 1") }
#eg2判断是否是因子，是则输出，不是则转化
grade <- as.factor(c("grade1","grade2")) 
if (!is.factor(grade)) { 
        grade <- as.factor(grade) 
}else { 
        print("Grade already is a factor") 
        }
#eg3多条件，中间的用else if
a <- 4 
if( a == 1) { 
        print("a == 1") 
}else if( a == 2) { 
        print("a == 2") 
}else { 
        print("Not 1 & 2") 
        }

#ifelse
#eg1 开方处理
x <- matrix(1:6, 2, 3) 
ifelse(x >= 0, sqrt(x), NA)


