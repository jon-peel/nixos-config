{ pkgs }:

let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-medium
      alphalph
      bbding
      capt-of
      cfr-initials
      changepage
      cm-unicode
      contour
      comment
      csquotes
      dblfloatfix
      enumitem
      fontaxes
      framed
      gensymb
      geometry
      gillius
      hang
      initials
      keycommand
      kpfonts
      lettrine
      listingsutf8
      multitoc
      numprint
      pdfcol
      pgf
      pgf-blur
      preprint
      tcolorbox
      tikzfill
      titlesec
      tocloft
      wargame
      wrapfig
      xstring

      
      #ec                # European Computer Modern fonts (for T1 fontenc)
      #metafont          # For font generation
      #graphics          # For graphicx package
      #tools             # For longtable package
      #wrapfig           # For wrapfig package
      #ulem              # For ulem package
      #amsmath           # For amsmath package
      #amsfonts          # For amssymb package
      #capt-of           # For capt-of package
      #hyperref          # For hyperref package
      #titling
      #xcolor
      #titlesec
      # pgf
      #tikzfill
      #booktabs
      #fontspec
      #cmbright
      ;
  });
in
  tex
