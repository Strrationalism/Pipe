# Pipe 构建系统

详细信息参见[GitHub Wiki](https://github.com/Strrationalism/Pipe/wiki)。

## 脚本示例

```shell
# 将CreateFont定义为task，在构建依赖图的阶段会提前执行，可用于添加依赖和输出，一个task可以调用其他更多task
- task CreateFont %ttfFile% for { Windows, Linux } to Nintendo-Switch
set %more% (tail %ttfFile%)
set %ttfFile% (head %ttfFile%)
set %outFile% (changeExtension %ttfFile% ".out")
input %ttfFile%
output %outFile%


# CreateFont的具体实现，它可以直接使用task定义中的本地变量
- operation CreateFont %ttfFile%
sdftool %ttfFile% %temp%
spritepacker %temp% %outFile%

- action build for Windows to Nintendo-Switch    # 这里是顶级定义“action”

CreateFont "1.ttf"
CreateFont "2.ttf"


- before action build for Windows to Nintendo-Switch           # 在build之前执行，这是一个task
let %out% "./output"
mkdir %out%

- after action build for Windows to Nintendo-Switch            # 在build之后执行


- include "More.pipe"

# 同一个顶级定义可以拆分写到不同位置
```
