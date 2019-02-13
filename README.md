## TODO

- 対処済み：袖通信は４次元配列(`double`/`dcomplex`)を対象にするように実装可能にする予定
- 対処済み：袖の長さ(`ND`)を調整可能にする予定
- 対処済み：袖通信関数のdouble/dcomplex切り替えに`interface`を使う

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
 type s_sendrecv_grid4d
    ! Size of grid system
    type(s_rgrid) :: rg
    ! Number of orbitals (4-th dimension of grid)
    integer :: nb
    ! Communicator
    integer :: icomm, myrank
    ! Neightboring MPI id (1:x,2:y,3:z, 1:upside,2:downside):
    integer :: neig(1:3, 1:2) 
    ! Communication requests (1:x,2:y,3:z, 1:upside,2:downside, 1:send,2:recv):
    integer :: ireq(1:3, 1:2, 1:2)
    ! PComm cache (1:x,2:y,3:z, 1:upside,2:downside, 1:src/2:dst)
    type(s_pcomm_cache4d) :: cache(1:3, 1:2, 1:2)
    ! Range (dim=1:x,2:y,3:z, dir=1:upside,2:downside, 1:src/2:dst, axis=1...3)
    integer :: is_block(1:3, 1:2, 1:2, 1:3)
    integer :: ie_block(1:3, 1:2, 1:2, 1:3)
    logical :: pcomm_initialized
  end type s_sendrecv_grid4d
```
- `rg`: 実空間グリッドの情報を格納(`s_rgrid`型)
- `nb`: ４次元方向の要素数（軌道 * スピン）
- `icomm, myrank`: MPIコミュニケータで初期化時に代入される
- `neig(idim, idir)`: 隣接ノードのIDを格納
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward} 
- `ireq(idim, idir, imode)`: 非同期通信のリクエストを格納
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward} 
    - imode: 通信{1:send, 2:recv}
- `is_block, ie_block(1:3, 1:2, 1:2, 1:3)`: 一時記憶領域の形
    - idim: 方向{1:x, 2:y, 3:z}
    - idir: 向き{1:upward, 2:downward}
    - iaxis: 軸・次元(1...7)
- `pcomm_initialized`: 永続通信が初期化済みであることを確認するためのフラグ

### init_sendrecv_grid4d(srg, rg, nb, icomm, myrank, neig)
- 初期化用サブルーチン（複素・実数共通）

### alloc_cache_real8(srg), alloc_cache_complex8(srg)
- キャッシュ領域のアロケーション用関数（複素・実数別）

### update_overlap(srg, data)
- 袖通信用関数（複素・実数共通）、`data`の型により切り替わる

## 使用手順（実数の場合）
```
type(s_sendrecv_grid4d) :: srg
real(8), allocatable :: psi4d(:, :, :, :)

! rg = 実空間グリッドの情報（s_rgrid型）
! nb = 軌道数
! icomm, myrank = MPIコミュニケータの情報
! neig = 隣接ノードの情報

! allocate(psi4d(上のrg%is_array:ie_arrayに範囲))


call init_sendrecv_grid4d(srg, rg, nb, icomm, myrank, neig)
call alloc_cache_real8(srg)

do iter = 1, niter

    ! psi4d = ...何らかのデータが代入される
    call update_overlaps(srg, data4d)

end do

```