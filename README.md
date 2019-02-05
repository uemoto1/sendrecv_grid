## ファイル構造
```
├── Makefile
├── Makefile.inc -> ./Makefile.inc.gnu シンボリックリンク
├── Makefile.inc.gnu
├── Makefile.inc.knl
├── README.md
├── common
│   ├── pack_unpack.f90
│   ├── sendrecv_grid.f90 開発中モジュール
│   └── structures.f90
├── main.f90 エントリーポイント
├── misc
│   └── unusedvar.f90
├── parallel
│   ├── salmon_communication.f90
│   └── salmon_parallel.f90
└── test00.f90 実験用ドライバルーチン
```

## sendrecv_grid.f90
