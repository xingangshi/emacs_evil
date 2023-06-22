# 我的 Emacs 配置和使用

## Emacs 安装

```bash
Debian
apt-get install emacs

Ubuntu
apt-get install emacs

Alpine
apk add emacs

Arch Linux
pacman -S emacs

Kali Linux
apt-get install emacs

CentOS
yum install emacs-1

Fedora
dnf install emacs-1

OS X
brew install emacs

Raspbian
apt-get install emacs

Docker
docker run cmd.cat/emacs emacs
```

## 打开 Emacs

1. GUI 模式，命令行输入 `$ emacs`
2. 终端模式，命令行输入 `$ emacs -nw`

## [Emacs evil](https://github.com/emacs-evil/evil) 使用

> My emacs with evil mode and config for emacs.
>
> One-step for install and config emcas by run [`bash install_evil_emacs.sh`](/install_evil_emacs.sh)

## [Prelude Emacs](https://github.com/bbatsov/prelude) 使用

> My emacs with prelude Emacs and config for emacs.
>
> One-step for install and config emcas by run [`bash install_prelude_emacs.sh`](/install_prelude_emacs.sh)

## 主题安装和使用

### 安装主题

需要 emacs 安装好以后，手动安装 `monokai-theme` 或其他主题，

```BASH
M-x package-install RET monokai-theme RET
```

### 使用主题

- 打开 `Emacs` 后输入命令 `M-x enable-thme RET monokai RET`

- 在配置文件里面配置如下, **Emacs evil** 配置在 `~/.emacs` 文件中， **Prelude Emacs** 配置在 `~/.emacs.d/personal/custom.el` ：

```lisp
(load-theme 'monokai t)
```

## 学习进阶

## [Emacs 快捷键及 Org Mode 笔记](https://github.com/xingangshi/org_mode)

## [Org mode 相关资料](https://www.geekpanshi.com/panshi/study/org_modes.html)
