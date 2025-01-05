# Heimdall

Guard your data like Heimdall guards Asgard.

这是一个用 Haskell 实现的密码学基础算法集合，主要用于教学和个人学习目的。

## 项目结构

```
src/
├── Math/ -- 数学基础
│ ├── FiniteField/ -- 有限域相关实现
│ ├── ModularArith/ -- 模运算
│ └── PrimeNumber/ -- 素数相关
├── Basic/ -- 基础密码学
│ ├── Classical/ -- 古典密码
│ └── Stream/ -- 流密码
├── Modern/ -- 现代密码学
│ ├── Block/ -- 分组密码
│ ├── Public/ -- 公钥密码
│ └── Hash/ -- 哈希函数
└── Utils/ -- 工具函数
├── Binary/ -- 二进制处理
└── Encoding/ -- 编码工具
```

## 开发路线图

### 第一阶段：数学基础 (Math)

1. 模运算基础实现

   - GCD 和扩展欧几里得算法
   - 模乘法逆元
   - 中国剩余定理

2. 有限域实现

   - 素域 GF(p)
   - 扩展域 GF(2^n)
   - 多项式运算

3. 素数相关
   - 素性测试
   - 大素数生成

### 第二阶段：古典密码 (Basic/Classical)

1. 替换密码
   - 凯撒密码
   - 维吉尼亚密码
2. 置换密码
3. 仿射密码

### 第三阶段：现代对称密码

1. 流密码

   - RC4
   - ChaCha20

2. 分组密码
   - DES
   - AES

### 第四阶段：现代非对称密码

1. RSA
2. ElGamal
3. 椭圆曲线密码系统基础

### 第五阶段：哈希函数

1. MD5
2. SHA 家族
