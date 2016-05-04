R.home('bin')
切换到lyx，工具-首选项-路径-路径前缀-把bin放进去改成正斜杠\-保存-关闭
工具-重配置-重启R
文档-首选项-模块-kintr-添加-确认
ctrl+L进入输入界面，英文输入
<<>>=
rnorm(5)
@
按左上角小眼睛可以查看pdf效果
<<fig.height=3,fig.width=3>>=改变尺寸
<<fig.height=3,fig.width=3，echo=false#只出结果不出命令，eval=false只出命令不出结果，message=flase不显示包信息）>>=
中文操作
文档-首选项-文档类-Chinese article ctex
然后语言-繁体中文-编码-其他-unicode latex utf8
然后字体-使用非tex字体
然后输出-输出格式-pdf Xetex
然后导言区-
\usepackage{xeCJK}
\setCJKmainfont{simSun}
确定提示fail不管

文档类-bima什么鬼的
APA APA6论文
BEAMER 展示用
CETX 中文模版
elsarticle 论文
frletter 信件
IEEE 论文
kluwer论文
post 发表
revetx 论文
