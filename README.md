# Fortran的23种设计模式

[![BSD-3](https://img.shields.io/github/license/zoziha/Fortran-Design-Patterns?color=pink)](LICENSE)
[![fpm](https://github.com/zoziha/Fortran-Design-Patterns/workflows/fpm/badge.svg)](https://github.com/zoziha/Fortran-Design-Patterns/actions)
[![mdbook](https://github.com/zoziha/Fortran-Design-Patterns/workflows/mdbook/badge.svg)](https://github.com/zoziha/Fortran-Design-Patterns/actions)

《Fortran的23种设计模式》是一份Fortran面向对象编程中文实用教程。

|项目|描述|
|:-:|:-:|
|版本：|0.0.13|
|作者：|左志华（zoziha）|
|网页：|https://zoziha.github.io/Fortran-Design-Patterns/|
|版权：|Copyright (c) 2021 zoziha|

## 开始

### 软件依赖

- Git
- [Rust](https://www.rust-lang.org/zh-CN/)
- [mdbook](https://github.com/rust-lang/mdBook)

### 获取代码

```sh
git clone https://github.com/zoziha/Fortran-Design-Patterns.git
cd Fortran-Design-Patterns
```

### 使用[fortran-lang/fpm](https://github.com/fortran-lang/fpm)构建代码

Fortran包管理器（fpm）是Fortran-lang社区驱动、为Fortran生态设计的包管理器和代码构建器。<br>
你可以通过提供的`fpm.toml`构建代码：

```sh
fpm test --list  # 获取已提供的设计模式示例
fpm test <pattern_name, see `fpm.toml` or test list>
```

### 使用[mdbook](https://github.com/rust-lang/mdBook)构建文档

mdBook是一个从Markdown文件创建现代在线书籍的实用程序。<br>
你可以通过提供的`book.toml`文件来构建《Fortran的23种设计模式》。

```sh
cd doc && mdbook build
```

### 链接

- [设计模式](https://refactoringguru.cn/design-patterns)
- [farhanjk/FortranPatterns](https://github.com/farhanjk/FortranPatterns)