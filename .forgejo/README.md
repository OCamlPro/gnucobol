# Forgejo CI of the gitside branches

This directory is only read by Forgejo, on the forge where OCamlPro
runs the CI of the gitside branches; GitHub ignores it and keeps running
`.github/workflows`. Forgejo, in turn, ignores `.github/workflows` as
soon as `.forgejo/workflows` exists.

**On GitHub, `.github/` can be changed freely**: the changes made to a
gitside branch of `OCamlPro/gnucobol` are merged into the gitside
branches of Forgejo at the next sync, and come back with them. Please do
not remove or change `.forgejo/` on GitHub: the merge would bring the
change into Forgejo, and removing it would stop this CI and the sync.

## Workflows

- `workflows/ci.yml`: on every push and pull request, builds GnuCOBOL
  (bootstrap, configure with BDB) and runs the testsuite on Linux. On a
  push to `gitside-master` or `gitside-gnucobol-3.x`, when the build
  passes, the job `mirror-github` pushes the tested commit to the same
  branch of `github.com/OCamlPro/gnucobol` and of the GitHub repository
  of the customers (not forced; each push is skipped while its secrets
  are not set).
- `workflows/sync-svn.yml`: every 6 hours, and by hand (Actions, "Sync
  with SVN", "Run workflow"), merges the branches that the SVN mirror
  pushes to `github.com/OCamlPro/gnucobol` into the gitside branches of
  Forgejo: `master` into `gitside-master`, `gnucobol-3.x` into
  `gitside-gnucobol-3.x`. The gitside branch of GitHub is merged first,
  so that nothing done there by hand is lost. A conflict makes the job
  fail without pushing anything.

The whole loop, without touching the SVN mirror:

```
SVN --(existing mirror)--> GitHub master, gnucobol-3.x
     --(sync-svn.yml, every 6 h)--> Forgejo gitside-*  (merge)
     --(ci.yml: build + testsuite)--> GitHub gitside-*  (when green)
```

Forgejo runs a `schedule:` from the default branch of the repository
only: `sync-svn.yml` must be in that branch (today
`gitside-gnucobol-3.x`).

## Secrets (Forgejo, repository settings, Actions, Secrets)

Names starting with `GITHUB_`, `GITEA_` or `FORGEJO_` are refused.

- `FORGE_PUSH_KEY` (required by `sync-svn.yml`): the private part of a
  deploy key **with write access** on this Forgejo repository. A push
  made with the automatic token of the job would not start `ci.yml`,
  hence a key.
  1. `ssh-keygen -t ed25519 -C gitside-sync -N '' -f gitside-sync`;
  2. Forgejo, repository settings, Deploy keys: add `gitside-sync.pub`,
     with write access;
  3. secret `FORGE_PUSH_KEY`: the whole file `gitside-sync`;
  4. delete the two local files.
- `MIRROR_DEPLOY_KEY` (optional, for `mirror-github`): the private part
  of a deploy key with write access on `github.com/OCamlPro/gnucobol`
  (GitHub, Settings, Deploy keys, "Allow write access"). A deploy key
  can push to every branch of the repository: protect `master` and
  `gnucobol-3.x` (the SVN mirror) with a GitHub ruleset that only lets
  the mirror push to them.
- `CUSTOMERS_REPO_URL` and `CUSTOMERS_TOKEN` or `CUSTOMERS_DEPLOY_KEY`
  (optional, for `mirror-github`): the SSH URL of the GitHub repository
  of the customers (kept in a secret, so that it does not appear here),
  and either a fine-grained token restricted to that repository, with
  the permissions Contents and Workflows (read and write), or the
  private part of a deploy key with write access on it. The token is
  used when set: GitHub refuses a push that changes `.github/workflows`
  from a deploy key, which happens each time those files change on
  `OCamlPro/gnucobol`. The gitside branches are pushed there too.
