# futhrast

## Demo build steps and requirements

To build the examples, run:

```bash
futhark pkg sync
cd examples
futhark pkg sync
cd <example directory>
make -j $(nproc)
./lys
```

You will have to get a version of futhark compatible with version `0.26.3` to run the code.

You will also need to install the SDL-dependencies `libsdl2-dev` and `libsdl2-ttf-dev` libraries to build the `lys` futhark library.

More details on the requirements of the `lys` library can be found [here](https://github.com/abxh/lys?tab=readme-ov-file#requirements).

## Addendum

The Pineda rasterizers make use of the custom [expand_masked](https://github.com/abxh/expand_masked/) design API,
a expand-filter implementation based on sequential use of bitmasks.

On an Nvidia M2000M, `expand_masked` provides a ~2x speedup for irregular segment sizes 8-32, 
and a 1.7x speedup at size 64, compared to a naive implementation of expand-filter. The advantage
disappears for segment sizes >=128.
