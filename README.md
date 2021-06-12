# [mikail-khan.com](https://mikail-khan.com)

This is the source code for my personal website, found at <https://mikail-khan.com>.

The website is mostly static, but I needed an excuse to use Haskell, so it runs on the [Servant Framework](https://hackage.haskell.org/package/servant) and uses [Blaze HTML](https://github.com/jaspervdj/blaze-html) for HTML generation. 

The [Resume](https://mikail-khan.com/resume) and [Portfolio](https://mikail-khan.com/portfolio) pages are generated using the TOML files found in `static/Assets`. Since the data files don't change frequently they're cached. The TOML files are parsed using [htoml](https://hackage.haskell.org/package/htoml) and processed using [aeson](https://hackage.haskell.org/package/aeson).

The interactive circles are drawn with WebGL using [PixiJS](https://www.pixijs.com). The script for the gravity simulation is [here](https://github.com/mkhan45/mikail-khan.com/blob/main/static/js/index.js).
