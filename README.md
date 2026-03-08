# futharst

## Demo build steps and requirements

To build the demo, simply run:

```bash
futhark pkg sync
make
cd build
./futhrast-gui
```

The code was tested with [this](https://github.com/diku-dk/futhark/tree/11ed647c187efe43ccc9e808dbcbb040de99d2ba)
 `nightly` version of the `futhark` compiler. You must build `futhark` manually.

You may need to install `xxd`, `libsdl2-dev` and `libsdl2-ttf-dev` libraries to build the `lys` futhark library.

More details on the requirements of the `lys` library can be found [here](https://github.com/abxh/lys?tab=readme-ov-file#requirements).
