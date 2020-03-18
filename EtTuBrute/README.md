## Пакеты
cabal install cryptohash

cabal install deepseq

cabal install bytestring

cabal install base16-bytestring-0.1.1.6

cabal install utf8-string-1.0.1.1

## Ключи компиляции
ghc bruteWithForks.hs -O2 -threaded

## Ключи запуска
./bruteWithForks +RTS -s -Nn

## Время работы

Всё для **95** различных ascii символов

length = 4, **6**/12 ядер: **20s**

length = 5, **6**/12 ядер: **2225.395s**
