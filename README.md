# Scheme.org DNS and front page

This is the main version control repository for the internet domain
name `scheme.org`.

## Related domains

DNS zones for the following domains are also managed here.

`schemers.org` moved here in January 2023.

`schemeworkshop.org` moved here in December 2023.

## Projects

Scheme.org is divided into _projects_. Each project corresponds to a
subdomain under `scheme.org`. The project ID `example` corresponds to
the subdomain `example.scheme.org`. That project also controls all
nested subdomains `*.example.scheme.org`.

The file `projects.scm` holds the ground truth for all project
definitions, including DNS records. Edits should be made to this file.

Each project is responsible for its own servers. This top-level repo
does not deal with any project-specific configuration except for DNS
records.

## Special projects

The `www` project corresponds to `www.scheme.org`, the front page of
`scheme.org`. The website at this subdomain is generated from
`projects.scm` as well as the Markdown (`doc/*.md`) files in this
repo.

The `lists` project is responsible for `@scheme.org` mailing lists
(TODO).

The `redirect` project tracks subdomains that are just HTTP redirects.

The `staging` project gives staging subdomains to projects that want
them. For example, `www.staging.scheme.org` is a staging version of
`www.scheme.org`. Each project must bring its own staging server,
which may be the same as its main server.

## Maintenance scripts

Scripts are written for R7RS-small. The WWW scripts use a few Chicken
Scheme eggs. The `scripts/*.sh` wrappers use Chicken 5 with the `r7rs`
egg.

### DNS maintenance

- `scripts/dns.sh` -- generate `dns/scheme.org.zone` based on `projects.scm`.
- `scripts/dnsput.sh scheme.org` -- upload `dns/scheme.org.zone` to Gandi.
- `scripts/dnsput.sh schemers.org` -- upload `dns/schemers.org.zone` to Gandi.
- `scripts/dnsget.sh scheme.org` -- download `dns/scheme.org.zone` from Gandi.
- `scripts/dnsget.sh schemers.org` -- download `dns/schemers.org.zone` from Gandi.

### WWW maintenance

- `scripts/www.sh` -- generate HTML files in the `www` subdirectory
  based on `projects.scm` and `doc/*.md`.
- `scripts/wwwstaging.sh` -- upload `www` subdirectory to staging server.
- `scripts/wwwproduction.sh` -- upload `www` subdirectory to production server.

## See also

- https://github.com/schemeorg-community
- https://github.com/schemedoc
