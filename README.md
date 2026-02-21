# futharst

## Demo build steps and requirements

To build the demo, simply run:

```bash
futhark pkg sync
make
cd build
./futhrast-gui
```

The code was tested with version `0.25.35-1` of the `futhark` compiler.

You may need to install `xxd`, `libsdl2-dev` and `libsdl2-ttf-dev` libraries to build the `lys` futhark library.

More details on the requirements of the `lys` library can be found [here](https://github.com/diku-dk/lys?tab=readme-ov-file#requirements).
