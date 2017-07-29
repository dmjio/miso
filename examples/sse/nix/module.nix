{ options, lib, config, pkgs, modulesPath }:
let
  cfg = config.services.sse-haskell-miso;
in {
   options.services.sse-haskell-miso.enable = lib.mkEnableOption "Enable the sse.haskell-miso.org service";
   config = lib.mkIf cfg.enable {
     systemd.services.sse-haskell-miso = {
       path = with pkgs; [ sse-haskell-miso bash ];
       wantedBy = [ "multi-user.target" ];
       script = ''
	 ./bin/server +RTS -N -A4M -RTS
       '';
       description = ''
         https://sse.haskell-miso.org
       '';
       serviceConfig = {
         WorkingDirectory=pkgs.sse-haskell-miso;
	 KillSignal="INT";
	 Type = "simple";
         Restart = "on-abort";
         RestartSec = "10";
      };
    };
  };
}
