{ pkgs, config, ... }:
{
  imports = [ ./module.nix ];
  nixpkgs.config.packageOverrides = pkgs: {
    haskell-miso = import ./../default.nix {};
    misoPkgs = import ../../../default.nix { examples = true; };
  };
  services = {
    haskell-miso.enable = true;
    nginx = {
      enable = true;
      virtualHosts = {
        "haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
              proxyPass = "http://127.0.0.1:3002";
            };
          };
        };
        "todo-mvc.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/todo-mvc.jsexe";
            };
          };
        };
        "flatris.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.pkgs.more-examples.flatris}/bin/app.jsexe";
            };
          };
        };
        "miso-plane.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = pkgs.misoPkgs.pkgs.more-examples.miso-plane;
            };
          };
        };
        "2048.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = pkgs.misoPkgs.pkgs.more-examples.the2048;
            };
          };
        };
        "threejs.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/threejs.jsexe";
            };
          };
        };
        "snake.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.pkgs.more-examples.snake}/bin/app.jsexe";
            };
          };
        };
        "router.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/router.jsexe";
            };
          };
        };
        "mario.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/mario.jsexe";
            };
          };
        };
        "simple.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-ghcjs}/bin/simple.jsexe";
            };
          };
        };
        "canvas.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/canvas2d.jsexe";
            };
          };
        };
        "svg.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/svg.jsexe";
            };
          };
        };
        "file-reader.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/file-reader.jsexe";
            };
          };
        };
        "xhr.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/xhr.jsexe";
            };
          };
        };
        "websocket.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
           "/" = {
             root = "${pkgs.misoPkgs.miso-examples}/bin/websocket.jsexe";
            };
          };
        };
      };
    };
  };
}
