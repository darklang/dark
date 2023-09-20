## Adding Vector Graphics

All our SVG icons are converted into a font set via [Glyphter](https://glyphter.com/).
They are stored in `client/static/dark-icons-font-vX`. And defined for usage in `client/styles/_dark-icons.scss`.
To add a new font:

1. Login to glyphter via 1Password. There should be account credentials stored in Dark's shared vault.
2. Once you are logged in, there is a dropdown that saids "My Fonts" at the top. Select "Dark Icons" from it.
3. Make sure your SVG filename(s) is something recongizable (ie: new-logo.svg). Drag SVG file(s) into the empty spaces.
4. Once you are done adding the new icon(s), save this font and download it.
5. Inside the downloaded zip there are two folders `css` and `fonts`. Rename the fonts folder to `dark-icons-font-vX`, where X is the new version number.
6. Move your `dark-icons-font-vX` folder into `client/static` and delete the old version folder.
7. Open `css/Dark-Icons.css` in your zip, and `client/styles/_dark-icons.scss` in your Dark source code.
8. In `_dark-icons.scss`, update the variable `$dark-icons-version` to the new version.
9. In `Dark-Icons.css` you should see something like `.icon-new-logo:before{content:'\00A1';}`, '\00A1' is the ASCII notation for you glyph. In `_dark-icons.scss`, find the variable `$dark-icons`, use the ASCII notation to update the map. ie: `"new-logo":"\00A1"`.
10. Now you can use your awesome new icons! Like this `<i class="di di-new-logo" />`.

If you need to design your own icon, remember to look at the [icon requirements](https://www.notion.so/darklang/Font-Icon-Requirements-9ea9b3f06bd94f6a842cd2ea473bc334) first.
