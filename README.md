# futharst

## Demo build steps and requirements

To build the demo, simply run:

```bash
futhark pkg sync
make -j $(nproc)
cd build
./futhrast-gui
```

The code was tested with `nightly` version of the `futhark` compiler. You must build `futhark` manually.

You need to install the SDL-dependencies `libsdl2-dev` and `libsdl2-ttf-dev` libraries to build the `lys` futhark library.

More details on the requirements of the `lys` library can be found [here](https://github.com/abxh/lys?tab=readme-ov-file#requirements).
