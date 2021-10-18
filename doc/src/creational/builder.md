# 生成器模式

正文：https://refactoringguru.cn/design-patterns/builder

Go代码：https://refactoringguru.cn/design-patterns/builder/go/example

```fortran
{{#include ../../../src/creational/builder/builder_module.f90}}
```

```fortran
{{#include ../../../src/creational/builder/builder_main.f90}}
```

使用默认结构体构造函数来赋值可分配字符型类型的子元素，在GFortran上出现bug，ifort正常：

```fortran
{{#include ../../../src/creational/builder/builder_module.f90:110:116}}
```