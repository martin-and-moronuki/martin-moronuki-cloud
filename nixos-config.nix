{ nixpkgs, pkgs, ... }:
{
    ec2.hvm = true;
    environment.systemPackages = with pkgs; [ ccze cloud-utils curl fish git htop lsof openssl tmux tree vim wget which ];
    imports = [ "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix" ];
    networking.firewall.allowedTCPPorts = [ 80 443 36411 ];
    networking.firewall.allowPing = true;
    networking.hostName = "martin-moronuki";
    nix.binaryCaches = [ "https://cache.nixos.org" "https://chris-martin.cachix.org" ];
    nix.binaryCachePublicKeys = [ "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs=" "chris-martin.cachix.org-1:O/29OCGL8P7qk/NmEaZzzZk3DDmUfa9nGA74OrX9/1g=" ];
    nix.gc.automatic = true;
    nix.gc.options = "--delete-older-than 30d";
    nix.trustedUsers = [ "@wheel" ];
    security.acme.acceptTerms = true;
    security.acme.email = "ch.martin@gmail.com";
    security.sudo.wheelNeedsPassword = false;
    services.nginx.enable = true;
    services.nginx.recommendedGzipSettings = true;
    services.nginx.recommendedOptimisation = true;
    services.nginx.recommendedProxySettings = true;
    services.nginx.recommendedTlsSettings = true;
    services.nginx.virtualHosts."chris-martin.org" = {
        enableACME = true;
        forceSSL = true;
        locations."/".root = "/var/www/chris-martin.org";
        locations."/".index = "index.html";
        locations."/".extraConfig = ''default_type text/html;'';
    };
    services.nginx.virtualHosts."jarclasses.com" = {
        enableACME = true;
        forceSSL = true;
        locations."/".root = "/var/www/jarclasses.com";
        locations."/".index = "index.html";
        locations."/".extraConfig = ''try_files $uri $uri.html /index.html =404;'';
    };
    services.openssh.enable = true;
    services.openssh.passwordAuthentication = false;
    services.openssh.ports = [ 36411 ];
    system.stateVersion = pkgs.lib.mkDefault "19.03";
    time.timeZone = "America/Denver";
    users.users.chris = {
        description = "Chris Martin";
        extraGroups = [ "wheel" ];
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDFJ4UYsEh+JZQGCMdbNrKfjH1F3rwKBRewwgaehwnijBADYSJ8iwDZji09vVfCxSQSMjZJS54mEBtjcOBOpM7+mR585wI6jhsfdsNqNwzJdxV47Bi4jAkg7XlWf9IYv7EUhRzsKGdoSqefh/7bN6MPcJQ9ccHKqBxtmGJ6eHfgLmgnb8+ozwDlwQKz5QDtdEnt8TUqucUB4AOyReBV7GRnwkTyGCForb5nhTftuVi7GO1qApJKBIpYlC1gbuCWDX9CIl7IzfAMyng6u6Ty9x31ZWKA0sJzRIX5cw3e8Ct7sWzZB3O/2FOwjyYadqTRQdR472Dz/f6mqqIl1ioxzfXRfh33bREg2opLc6bnYaWTXY6aAc5/wUbC7z4CTKBGZJHxY5mrRSlpQ2Rn8EvgyyxgxokLdTZqoiKw/tSmE9Mlle5JGh+m8agGe41dszZxBf41j/ORE+N5p0k02fvUWuG0PL3aFE77qUbOgxxXOYMtBV0YiJPzeBXDGrkW1wqKC2voJ6PuCZWOHaLxDqkUDgAMYyGMKoj5C53OZneVeSMgZG+/lxygAduyBx/RfQYrt4WsPfPnhl95Kxx8PTYuFfLXmcMNMhZ7rYW+Thvo40W+VjiqTUSCxLHr16SFSOj2mGl0A29VPPHA3H+ckprCo8pldPo3AYrwkV/zHlyLjuEQfQ== renzo-1"
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDAu0nM8baJOgaBB44w/rCczNK7pST0b/B3isYtqYw8QI2nqkmp4zddAFTjqL/zyVgIC+I2iIINVdQrC6jRWWZc82VRBzHjUtcz3sPppkLvZvfVupSXli1bDtnKNLA73bGr+odZFyR2zWsgcPwpGkzaWotGVhsW/F/2J+6HwG/D+9pWTlFAPwK/OV2J3axdnWWcihXjMIWT5/5ksOPqQWUBQnrQDQt2Rtw+3Qg36UlAOQgj2SMmMg6ppf8k0A7WRVqGxJE0BrZwUfvfqmUidPOsitkUphOtM424+HyXv66bDdLr80apPCqrwllHv0tt4N6vWVQg5OeKQe2w4NvIjQlR1ROf3MC3/1itiGvo5saEUh0DtQrl/g9LGXmIkVHFzJsmfnIx74VXH9MESugHturxniB+8Rq7ICg4eWZAal46tgcMdOm7u313fm5HpumQRwqTWcghXWRmOopqDNzXT2CEOJ6gzJS50E9SI65Y501NTtXwBgQIyOSt5wp2edBdxZUIEj4yGBBaVwDREbuR0mEMNnEmWyd7DsyOzKnFCsb5fwm77S5uSISzbzy2boVyb/zBEFRAorYNIaWRPX+a9aN5Kki+xCIsfMhVBFxSc5PMVrn/a05pYg9eb0jJoWmeBCXVLxOitSX0UrpNMw9yd1Jrgv6wVe5wr5kiIad2ooxQvQ== cubby-1"
        ];
        uid = 1000;
    };
    users.users.julie = {
        description = "Julie Moronuki";
        extraGroups = [ "wheel" ];
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDUA5QiRaMriKQrvA1H74UsJcnGieAcHYrHp27KBZjOhhKvinTkNyErY5JNFbgrCh3oC+6HeMSUOp+5qb2/hF0aO6kdYbGnYB3BchrbsPYFDccqW9eQONXdvYsa4mavfLzHIvXktkxVsd71CBRXqVFxxolYps99iJvFfhmgnn9Iz+zUIuOSRCPQ/Bcxbh3+fZHD2wlPpOZ352V3/utR6n6nlhK5W1Gq/mp5dmE32zCEOEse85xXQNxJpjdhIDZbu8PnNo2LuYIfRBFzIj/Gh6MIHJU3gQskt8MvKA+POdseflPLznxoDWYT3FqBO+agObr3FGnlnwpi1g9ym+U8T6SNFGUwR7cX5VkegLNSl7FTLMtaXoqUTKhBQH+ZIpNwlyepYnFo1BHR3IgsFDSaD/zLUjesBQIw6j+mtDCK9P3EnUcXo5v04OuGoyqTtAF4TTAz2kuC8ZsCs0cQEZRIoXqVIRcvPmlFr208o0SeGakCHVxIj6VnrQ+aHisVwuGAkq+Kb5mwE6b0xvuFGHo+bqHIUePymWQihbh0pZpYeSaJTLb2YG17HmE2rUFeO66CADmIMi2EWEI0xisT/e9FoqDcMnG/Z5sLBBDkerXwyfLA63zAkF0P3LtWX1Q0vYFhCn+VtMwE56qW/Z0sDdNiVwxn/a1GGh12gqXkNHBEePYjoQ== jdog74@gmail.com"
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCYvVsYGzkmlh0/C+7X5Uwe9NY3gUrd+5OfK72HJAqZJYAl/PCedgS/5c1Ncn+Yf0gr/UycFwitkZaP2OmPofcERg1JmHqCaLLsR48kZKZFYwGpCBWDH/cyoh3gYDhyJHj4I/bS75M0W48KTb8wiNRvfrMQ5T8XZ+PJprzz/cjNKkx1guUWGkwxFrfO8nb9YFK6Q5FDrk4O7C2hSKAAzfmMMj6F/htm4XVsr0g3ZBfq6TOWmpgKqKb6BMGhby10wDU2TWgWFyW61h1mld03ufZBHcOrGFEf3yx0IcmLW3Yd9a0obJEAIzrmAGi6+Kr/84xy3DNLBv6dwGqXZ62IErb5 julie@doriangray"
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSnB6dODD8gWA0iaW0PPZSmQbPnevl8TNrq+xbziCeb1H4dizv2dcM9JVpokCPfnt5hn0bp01dpjDgDsMJxCpW4DfUd7/eM1q2EqaJ5wAqy5xhtxlv44xgFfJEjwr2GX/RKS8VOf6OSTGqtCEudpQA+vmQSEW9kdIII/KXjdwj+vn2fX/XwxuvTt9qCnh1TP1mdNjU5ZwSi4Yco++w+THnUFAZ/NP0iZZawLtKIIRRBRLEikRNWtcFDAnTVNE7O9eRlDotmm12qKqzQhDO1HANJ5E+QHOf+zh8Yg2aCwBuKd0Z19zusaCOUUjacuxKY8Lx3LOvpBi7QniicMhfqX/n jdog74@gmail.com"
        ];
        uid = 1004;
    };
}
