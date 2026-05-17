# futharst

## Demo build steps and requirements

To build the examples, run:

```bash
cd examples
futhark pkg sync
cd <example directory>
make -j $(nproc)
./lys
```

You will have to get the latest version of futhark to run the code.

You will also need to install the SDL-dependencies `libsdl2-dev` and `libsdl2-ttf-dev` libraries to build the `lys` futhark library.

More details on the requirements of the `lys` library can be found [here](https://github.com/abxh/lys?tab=readme-ov-file#requirements).
