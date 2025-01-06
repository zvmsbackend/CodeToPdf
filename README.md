# Code to PDF

A little script based on [ouuyu's work](https://github.com/ouuyu/code-to-pdf) with extra customizability.

## Requirements

- .NET SDK version 6.0 upwards (I've only tested it on 8.0)
- Any OS (Third party dependencies promise to be cross-platform)

## Usage

```bash
dotnet fsi codetopdf <local-path/remote-repo> [-u] [-html] [-pdf] [-b] [-map <path>] [-style <path>] [-ignore <path>] [-o <path-without-extension>]
    -local-path: a path
    -github-repo: https://<host>/<owner>/<repo> / git@<host>:<owner>/<repo>.git
    -u: remove cloned repo after generation. used in conjunction with <remote-repo>
    -html: generate html output
    -pdf: generate pdf output, which is the default
    -b: generate pdf using a browser
    -map: specify a json file use to determine file's language from its extension. default to "extmap.json"
    -style: specify a css file injected into the output. default to "styles.css"
    -ignore: specify a .gitignore-like file to exclude unwanted files. default to directory's .gitignore (if it has)
    -o: output the production file to path-without-extension + .html/.pdf
```

## Customization

You may modify "extmap.json" and "styles.css" in this folder to customize converter's default behavior.

## Watermarks

The script uses [Dynamic PDF](https://www.dynamicpdf.com/), which is commercial and adds watermarks.

If you wish to remove them, purchase a license on [Dynamic PDF's website](https://www.dynamicpdf.com/forums/displaythread.aspx?F=rasterizer-for-net-v1&T=addremove-water-mark).

Otherwise you may save an .html file and print it to pdf using your favorite browser.

-b option automates this, but needs preinstalled [Playwright](https://playwright.dev/docs/intro) dependency.
