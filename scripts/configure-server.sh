#!/bin/sh
set -eu
R7RS=${R7RS:-chibi-scheme}
cd "$(dirname "$0")"
cd ..
rm -rf server/ansible/
mkdir -p server/ansible
mkdir -p server/ansible/roles/nginx/files
$R7RS lib/nginx.scm >server/ansible/roles/nginx/files/nginx.conf
$R7RS lib/sensible.scm >server/ansible/schemeorg.pose
cd server/ansible
echo "Entering directory '$PWD'"
set -x
sensible schemeorg.pose
ansible-playbook schemeorg.yaml "$@"
