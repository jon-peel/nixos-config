{ config, pkgs, ... }: {
  programs.emacs.enable = true;
  
  home = {
    file = {
      #".config/emacs" = { source = ./doom-emacs/repo; recursive = true; };
      #".emacs.d/.keep".text = "";
      #".emacs.d/init.el".source = ./init.el;
      #".emacs.d/quotes/meditations.txt".source = ./quotes/meditations.txt;
      #".emacs.d/quotes/quotes".source = ./quotes/quotes;
      #".emacs.d/code/org-include-generator.el".source = ./org-include-generator.el;
    };
  };
}
