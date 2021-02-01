# CEChecker

文字コードを判定します．ByteStringをためす用．

## 実行方法

```
$ ghc CEChecker.hs
[1 of 1] Compiling Main             ( CEChecker.hs, CEChecker.o )
Linking CEChecker ...
$ echo 自分のことがわかっていたところで、先のことはわからないものだ。 | nkf -j > jis_text.txt
$ echo 自分のことがわかっていたところで、先のことはわからないものだ。 | nkf -s > sjis_text.txt
$ echo 自分のことがわかっていたところで、先のことはわからないものだ。 | nkf -e > euc_text.txt
$ ./CEChecker
FilePass?: jis_text.txt
ISO-2022-JP
$ ./CEChecker
FilePass?: sjis_text.txt
Shift-JIS
$ ./CEChecker
FilePass?: euc_text.txt
EUC-JP
$
```
