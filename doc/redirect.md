## Explanation

Some subdomains of Scheme.org are just HTTP redirects. Each of those
subdomains has a DNS CNAME pointing to `redirect.scheme.org`.

The subdomain `redirect.scheme.org` itself has a CNAME pointing to the
origin server of Scheme.org. This subdomain exists to make [our DNS
zone](https://raw.githubusercontent.com/schemeorg/schemeorg/master/dns/scheme.org.zone)
easier to read. If the zone listed over 20 subdomains pointing to the
origin server, it would be hard to figure out which sites are actually
hosted on that server and which are mere redirects.

It may seem excessive to put so much effort into the readability of a
DNS zone but the zone is the most central part of Scheme.org. The
long-term maintenance of the domain relies on its structure being
transparent to any interested party, not just to one or two sysadmins.

## Maintenance

The ground truth lies in
[`projects.scm`](https://raw.githubusercontent.com/schemeorg/schemeorg/master/projects.scm).
Search for `redirect` in that file and you will find everything
pertinent.
