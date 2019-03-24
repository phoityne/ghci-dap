
# ghci-dap

A GHCi having [DAP](https://code.visualstudio.com/docs/extensions/example-debuggers) interface.


![101_deploy.png](https://raw.githubusercontent.com/phoityne/haskell-debug-adapter/master/docs/design/101_deploy.png)

Started developing based on [haskell-dap-0.0.10.0](https://hackage.haskell.org/package/haskell-dap).  
Separated an implementation from the interface package.

# Requirement
  - haskell-dap

```
> stack install haskell-dap ghci-dap
```


# Limitation
Currently this project is an __experimental__ design and implementation.

* Supported ghc-8.6, ghc-8.4
* Dev and checked on windows10

