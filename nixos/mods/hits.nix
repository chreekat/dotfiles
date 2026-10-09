# Page-view counter for words.chreekat.net. Pages request
# https://hits.chreekat.net/hit?p=<path>&r=<referrer>; nginx answers from
# memory and appends one JSON line per hit. There is no application behind it,
# so a traffic spike costs a log write per view and nothing else.
{ config, ... }:
let
  host = "hits.chreekat.net";
  # Not *.log, so the stock nginx logrotate rule (26 weeks) leaves it alone.
  log = "/var/log/nginx/hits.jsonl";
in
{
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    # Visitors' addresses are only held in memory, for rate limiting.
    appendHttpConfig = ''
      log_format hits escape=json '{"time":"$time_iso8601","path":"$arg_p","referrer":"$arg_r","ua":"$http_user_agent","status":$status}';
      limit_req_zone $binary_remote_addr zone=hits_per_client:1m rate=1r/s;
      limit_req_zone $server_name zone=hits_total:64k rate=100r/s;
    '';
    virtualHosts.${host} = {
      enableACME = true;
      forceSSL = true;
      locations."= /hit".extraConfig = ''
        limit_req zone=hits_per_client burst=10 nodelay;
        limit_req zone=hits_total burst=500 nodelay;
        access_log ${log} hits;
        add_header Cache-Control "no-store";
        # empty_gif, not return: return would answer before limit_req runs.
        empty_gif;
      '';
      locations."/".return = "404";
    };
  };

  # Monthly archives, kept forever.
  services.logrotate.settings.hits = {
    files = [ log ];
    frequency = "monthly";
    rotate = -1;
    dateext = true;
    dateyesterday = true;
    dateformat = "-%Y-%m";
    compress = true;
    delaycompress = true;
    su = "${config.services.nginx.user} ${config.services.nginx.group}";
    postrotate = "[ ! -f /var/run/nginx/nginx.pid ] || kill -USR1 `cat /var/run/nginx/nginx.pid`";
  };
}
