{ pkgs }:

{
  globalDefaults = {
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 3600;
    enableemail = false;
    emailoverride = "";
  };
  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };
}
