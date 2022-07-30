# Scheme.org DNS and front page

This is the main version control repository for the internet domain
name `scheme.org`.

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
`projects.scm` as well as the Markdown (`*.md`) files in this repo.

The `lists` project is responsible for `@scheme.org` mailing lists
(TODO).

The `staging` project gives staging subdomains to projects that want
them. For example, `www.staging.scheme.org` is a staging version of
`www.scheme.org`. Each project must bring its own staging server,
which may be the same as its main server.

## Maintenance scripts

Scripts are written for R7RS-small. The WWW scripts use a few Chicken
Scheme eggs. The `*.sh` wrappers use Chicken 5 with the `r7rs` egg.

### DNS maintenance

- `./dns.sh` -- generate `dns.zone` based on `projects.scm`.
- `./dnsput.sh` -- upload `dns.zone` to Gandi.
- `./dnsget.sh` -- download `dns.zone` from Gandi.

### WWW maintenance

- `./www.sh` -- generate `www` subdirectory based on `projects.scm`
  and `*.md`.
- `./wwwlocal.sh` -- start localhost web server to browse `www`
  subdirectory.
- `./wwwstaging.sh` -- upload `www` subdirectory to staging server.
- `./wwwproduction.sh` -- upload `www` subdirectory to production server.
