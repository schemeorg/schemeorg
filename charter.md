# The Scheme.org charter

## Introduction

## Technical assets

* The `scheme.org` domain name. The current registrar is Gandi.

* DNS service for `scheme.org`. Currently provided by Gandi.

* Web server for serving the `www.scheme.org` website.

* Git repositories at `github.com/schemeorg`.

* Mailing list `schemeorg@srfi.schemers.org`.

## Social roles

* **Domain registrant.** The person who is on file at the domain
  registrar as the registrant of `scheme.org`. Currently Magnus
  Ahltorp.

* **Domain techical contact.** The person who is on file at the domain
  registrar as the technical contact of `scheme.org`.

* **Domain billing contact.** The person who is on file at the domain
  registrar as the billing contact of `scheme.org`. Currently Arthur
  Gleckler.

* **DNS administrators.** One or more people who have the credentials
  to update the live DNS records for `scheme.org`.

* **Top-level administrators.** One or more people who are responsible
  for the DNS, git repo and web server.

## Current people

* **Domain registrar:** Gandi.net (Magnus Ahltorp, Arthur Gleckler)
* **DNS host:** Gandi.net (Magnus Ahltorp, Arthur Gleckler)
* **Git host:** GitHub.com (Arthur Gleckler, Lassi Kortela)
* **WWW host:** Vultr.com (Lassi Kortela)
* **Mailing list host:** SimpleLists.com (Arthur Gleckler)

## Special projects

`www` (subdomain `www.scheme.org`): The `scheme.org` web front page.

`lists` (subdomain `lists.scheme.org`): `@scheme.org` mailing lists.

`staging` (subdomain `staging.scheme.org`): Staging versions of
production projects.

`servers` (subdomain `servers.scheme.org`): Server pool for Scheme
projects.

## Email

The following standard `@scheme.org` email addresses:

* `hostmaster`
* `postmaster`
* `webmaster`

All other `@scheme.org` addresses are reserved for mailing lists
hosted at `lists.scheme.org`.

_Community projects_ shall not expose any email addresses on their
subdomains.

_Owned projects_ are free to run public and/or private mailing lists,
and to have private mailboxes, on their subdomains if they so choose.

## Rules

All Scheme.org projects need to follow these rules:

* No cookies or other local storage. Exception: session for users who
  explicitly logged in.

* No tracking of user behavior. Exception: ordinary server logs (web
  server and ssh logs, etc.) So-called _analytics_ services are not
  allowed.

* No embedded content from commercial services (video, audio, images,
  maps, status badges, etc.). These contain hidden trackers so often
  that it's easier to have a blanket ban than examine on a
  case-by-case basis. Links to commercial hosts are fine as long as
  the link itself does not contain a tracking or affiliate token.

* No commerce. Links to commerce outside scheme.org are fine, as long
  as the link does not read like an advertisement.

* No political, religious, or lifestyle advocacy.

When in doubt, please ask on the `schemeorg` mailing list.

## Procedures

### Adding a project

### Re-assigning a community project

If a _subdomain administrator_ of a community project is out of reach
for an extended period of time, the _top-level administrators_ may
remove the _subdomain administrator_ status of that person from
community projects.

If **all** _subdomain administrators_ of a community project are out
of reach for an extended period of time, the _top-level
administrators_ may remove the DNS records for the subdomain or change
them to point to a new server.

### Settling disputes between project administrators

_Subdomain administrators_ strive to settle all disputes between
themselves and with the subdomains' users without requiring the
intervention of _top-level administrators_.

If they cannot come to an agreement, _top-level administrators_ will
act as tie breakers, deciding what to do about the subdomain.

### Dealing with content that is against the rules

If someone notifies the _top-level administrators_ of content against
the rules, the _top-level administrators_ send a private email to all
_subdomain administrators_ of the subdomain in question.

7 full days from the time the first mail was sent, if the content has
not been removed, the _top-level administrators_ may hide the
subdomain link on the `www.scheme.org` front page.

30 full days from the time the first mail was sent, if the content has
not been removed, the _top-level administrators_ may drop DNS records
for the subdomain.

Once all problems have been fixed, the _top-level administrators_ will
restore front-page links and DNS records ASAP.

### Dealing with security problems

If a subdomain compromises the security of visitors or of `scheme.org`
infrastructure, the _top-level administrators_ may immediately hide
the subdomain link on the `www.scheme.org` front page. If the problem
is especially severe, the _top-level administrators_ may also drop DNS
records for the subdomain. In both cases the _top-level
administrators_ send a private email to all _subdomain administrators_
of the subdomain in question. If the _top-level administrators_ judge
that it is safe to do so, they will also send a public email to the
`schemeorg` mailing list - either immediately, after consulting with
the _subdomain administrators_, or after waiting a while and the
_subdomain administrators_ have not responded.

Once all problems have been fixed, the _top-level administrators_ will
restore front-page links and DNS records ASAP.

### Dealing with reliability problems

If a subdomain server has been unresponsive for 7 full days, the
_top-level administrators_ may remove it from the front page so as to
not confuse visitors.

Once all problems have been fixed, the _top-level administrators_ will
restore front-page links ASAP.
