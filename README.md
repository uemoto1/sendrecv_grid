## ファイル構造
```
├── Makefile                           (ビルド用)
├── Makefile.inc.gnu
├── Makefile.inc.knl
├── Makefile.inc -> ./Makefile.inc.gnu (シンボリックリンクする)
├── misc
│   └── unusedvar.f90
├── parallel
│   ├── salmon_communication.f90
│   └── salmon_parallel.f90
├── common
│   ├── pack_unpack.f90
│   ├── sendrecv_grid.f90　　　　　　　　 (実験中のコード)
│   └── structures.f90
├── main.f90                           (Entry point)
└── test00.f90                         (テスト用ドライバルーチン)
```
