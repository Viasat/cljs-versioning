# ClojureScript versioning tools: bump, voom-like-version, docker-compose

##  Bump

The bump command allows you to query and bump a set of software
components using a declarative specification of those components. The
declarative specification includes the type of components (docker
image, npm module, RPM package, git dir/submodule) and where the
components are sourced from (artifactory, npmjs, docker hub, RPM repo,
etc).

Detailed documentation of the bump command line parameters and
specification format can be displayed by running `bump --help`.

An example specification file including all types and locations
supported by bump is located at `version-spec-example.yaml`.

## Copyright & License

This software is copyright Viasat, Inc and is released under the terms
of the Eclipse Public License version 2.0 (EPL.20). A copy of the
license is located at in the LICENSE file at the top of the
repository.
