## TODO

- 袖通信は４次元配列(`double`/`dcomplex`)を対象にするように実装可能にする予定
- 袖の長さ(`ND`)を調整可能にする予定
- 袖通信関数のdouble/dcomplex切り替えに`interface`を使う

## ファイル構造
```
├── Makefile
├── Makefile.inc -> ./Makefile.inc.gnu シンボリックリンクを作る
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

### s_sendrecv_grid型

グリッド間通信の情報・永続通信用の一時変数を保持

```
integer :: icomm !! MPI group
logical :: use_complex !! .true.:complex(8), .false.:real(8)
logical :: use_corner !! .false.:orthogonal, .true.:non-orthogonal
integer :: neig(1:3, 1:2) !! 1:x,2:y,3:z, 1:upward,2:downward
integer :: ireq(1:3, 1:2, 1:2) !! 1:x,2:y,3:z, 1:upward,2:downward, 1:send,2:recv
type(s_wavefunction) :: srmatbox7d(1:3, 1:2, 1:2)
type(array_shape) :: nshape(1:3, 1:2, 1:7)
```

- `use_complex`: 変数の複素数・実数判定
- `neig(idim, idir)`: 隣接ノードのIDを格納
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward} 
- `ireq(idim, idir, imode)`: 非同期通信のリクエストを格納
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward} 
    - imode: 通信{1:send, 2:recv}
- `srmatbox7d(idim, idir, iside)`: 永続通信用の一時記憶領域
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward}
    - iside: 送信元・送信先{1:src, 2:dst}
- `nshape(1:3, 1:2, 1:7)`: 一時記憶領域の形
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward}
    - iaxis: 軸・次元(1...7)

### sendrecv7d(srg, wf)
- 通信用サブルーチン

### pack_smatbox7d(srg, wf, jdim, jdir)
- 永続通信用の記憶領域へ送信データを書き込み

### unpack_smatbox7d(srg, wf, jdim, jdir)
- 永続通信用の記憶領域から受信データを取り出し
