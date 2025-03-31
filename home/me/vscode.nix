{ pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    profiles.default = {
      extensions = with pkgs.vscode-extensions; [
        ms-azuretools.vscode-docker
        ms-vscode-remote.remote-containers
        ms-vscode-remote.vscode-remote-extensionpack
      ];
      userSettings = {
        "editor.fontSize" = 14;
        "files.autoSave" = "afterDelay";
        "window.restoreWindows" = "none";
        "window.autoDetectColorScheme" = true;
        "workbench.preferredDarkColorTheme" = "Default Dark+";
        "workbench.preferredLightColorTheme" = "Default Light+";
        "git.enableSmartCommit" = true;
        # "dev.containers.dockerPath" = "podman";
      };
    };
  };
}
