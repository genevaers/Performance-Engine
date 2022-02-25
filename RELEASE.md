# Releases

This page describes the release process and the currently planned schedule for upcoming releases as well as the respective release shepherd. Release shepherds are chosen on a voluntary basis.

## Release schedule

Release cadence of is currently variable and no time table has been published

| release series | date of release (year-month-day)           | release shepherd                            |
|----------------|--------------------------------------------|---------------------------------------------|
| v4.50.001  | 2021-08-12  pe repo                            | Bob McCormack (GitHub: @bobmcc9)            | 


If you are interested in volunteering please create a pull request against the [GenevaERS/pe](https://github.com/genevaers/pe) repository and propose yourself for the release series of your choice.

## Release Shephered responsibilities 

The release shepherd is responsible for the entire release series of a minor release, meaning all pre- and patch releases of a minor release. 
The process formally starts with the initial release, but some preparations may be done a few days in advance.

* We aim to keep the master branch in a working state at all times. In principle, it should be possible to cut a release from master at any time.
  In practice, things might not work out as nicely. A few days before the release is scheduled, the shepherd should check the state of master. 
  Following their best judgement, the shepherd should try to expedite bug fixes that are still in progress but should make it into the release.
  On the other hand, the shepherd may hold back merging last-minute invasive and risky changes that are better suited for the next minor release.
* On the date listed in the table above, the release shepherd cuts the first release (using the suffix `-rc.0`) and creates a new branch 
  called  `release-<major>.<minor>` starting at the commit tagged for the release. In general, a release is considered a release candidate
  (that's what `rc` stands for) and should therefore not contain any known bugs that are planned to be fixed in the final release.
* With the release, the release shepherd is responsible for running and monitoring a benchmark run of the release for about  3 days, after which, 
  if successful, the release is promoted to a stable release.
* If regressions or critical bugs are detected, they need to get fixed before cutting a new pre-release (called `-rc.1`, `-rc.2`, etc.). 

See the next section for details on cutting an individual release.

## How to cut an individual release

These instructions are currently valid for the genevaers project, i.e. the [pe repository](https://github.com/genevaers/).
Applicability to other genevaers repositories depends on the current state of each repository.
We aspire to unify the release procedures as much as possible.

### Branch management and versioning strategy

We use [Semantic Versioning](https://semver.org/).

We maintain a separate branch for each minor release, named `release-<major>.<minor>`, e.g. `release-1.1`, `release-2.0`.

The usual flow is to merge new features and changes into the master branch and to merge bug fixes into the latest release branch.
Bug fixes are then merged into master from the latest release branch. The master branch should always contain all commits from the 
latest release branch. As long as master hasn't deviated from the release branch, new commits can also go to master,
followed by merging master back into the release branch.

If a bug fix got accidentally merged into master after non-bug-fix changes in master, the bug-fix commits have to be cherry-picked
into the release branch, which then have to be merged back into master. Try to avoid that situation.

Maintaining the release branches for older minor releases happens on a best effort basis.

### Prepare your release

For a patch release, work in the branch of the minor release you want to patch.

For a new major or minor release, create the corresponding release branch based on the master branch.

Bump the version in the `VERSION` file and update `CHANGELOG.md`. Do this in a proper PR as this gives others the opportunity
to chime in on the release in general and on the addition to the changelog in particular.

Note that `CHANGELOG.md` should only document changes relevant to users of genevaers, including external API changes, 
performance improvements, and new features. Do not document changes of internal interfaces, code refactorings and clean-ups,
changes to the build process, etc. People interested in these are asked to refer to the git history.

Entries in the `CHANGELOG.md` are meant to be in this order:

* `[CHANGE]`
* `[FEATURE]`
* `[ENHANCEMENT]`
* `[BUGFIX]`

### Draft the new release

Tag the new release with a tag named `v<major>.<minor>.<patch>`, e.g. `v2.1.3`. Note the `v` prefix.

You can do the tagging on the commandline:

```bash
$ tag=$(< VERSION)
$ git tag -s "v${tag}" -m "v${tag}"
$ git push --tags
```

Signing a tag with a GPG key is appreciated, but in case you can't add a GPG key to your Github account using the
following [procedure](https://help.github.com/articles/generating-a-gpg-key/),
you can replace the `-s` flag by `-a` flag of the `git tag` command to only annotate the tag without signing.

Once a tag is created, the release process will be actioned.

Once that has happened, click _Publish release_, which will make the release publicly visible and create a GitHub notification.

### Wrapping up

If the release has happened in the latest release branch, merge the changes into master.

To update the docs, a PR needs to be created to `genevaers/doc`. 

Once the binaries have been uploaded (if provided), announce the release on
`https://openmainframeproject.slack.com/' in the genevaers channel.
Check out previous announcement mails for inspiration. 

### Pre-releases (if used)

The following changes to the above procedures apply:

* In line with [Semantic Versioning](https://semver.org/), append something like `-rc.0` to the version (with the corresponding changes to the tag name, the release name etc.).
* Tick the _This is a pre-release_ box when drafting the release in the Github UI.
* Still update `CHANGELOG.md`, but when you cut the final release later, merge all the changes from the pre-releases into the one final update.

