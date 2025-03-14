{ config, ... }:
{
  age.secrets.snowdrift-gitlab-authtoken = {
    file = ../agenix-secrets/snowdrift-gitlab-token.age;
  };
  services.gitlab-runner = {
    enable = true;
    services.snowdrift = {
      description = "chreekat-honk";
      authenticationTokenConfigFile = config.age.secrets.snowdrift-gitlab-authtoken.path;
      dockerImage = "alpine:latest";
      dockerAllowedServices = [
        "postgres:*"
        "docker:*"
      ];
      # What does this do, exactly?
      requestConcurrency = 2;
    };
  };
}
