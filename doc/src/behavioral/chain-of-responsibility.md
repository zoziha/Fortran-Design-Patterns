# 责任链模式

正文：https://refactoringguru.cn/design-patterns/chain-of-responsibility

Go代码：https://refactoringguru.cn/design-patterns/chain-of-responsibility/go/example

## 示例

本例子模拟医院的看病缴费的责任链。

病人需要进行的大致步骤是：

1. 进院，注册信息
2. 医生检查
3. 药房给药
4. 病人缴费，出院

```fortran
{{#include ../../../src/behavioral/chain-of-responsibility/CoR_module.f90}}
```

```fortran
{{#include ../../../src/behavioral/chain-of-responsibility/CoR_main.f90}}
```

## 评价

责任链很像流水线，上一节点处理完进入下一节点。

可以应用于科学计算的文件输入检查过程。