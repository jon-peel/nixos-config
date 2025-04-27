{ pkgs }:

let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-medium
      wargame
      alphalph
      pgf
      pgf-blur
      wrapfig
      capt-of
      
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
      #tcolorbox
      # pgf
      #tikzfill
      #booktabs
      #fontspec
      #cmbright
      ;
  });
in
  tex
