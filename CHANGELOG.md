# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][]

[Unreleased]: https://github.com/atomist/artifact-source/compare/1.0.0-m.21...HEAD

## [1.0.0-m.21] - 2017-09-08

[1.0.0-m.21]: https://github.com/atomist/artifact-source/compare/1.0.0-m.20...1.0.0-m.21

### Added

-   Added list commits on a pull request

## Changed

-   Default PR merge method is now "merge", the GitHub default

## [1.0.0-m.20] - 2017-08-30

[1.0.0-m.20]: https://github.com/atomist/artifact-source/compare/1.0.0-m.19...1.0.0-m.20

### Added

-   Added `html_url` and `repository_url` to issue and commit response objects where possible

## [1.0.0-m.19] - 2017-08-24

[1.0.0-m.19]: https://github.com/atomist/artifact-source/compare/1.0.0-m.18...1.0.0-m.19

### Fixed

-   Added a null check for the InputStream returned in a 200 or 201 response, as it appears the GitHub API
    is now not returning a 204 when a collaborator is added to a repository [#51][51]
    
[51]: https://github.com/atomist/artifact-source/issues/51

## [1.0.0-m.18] - 2017-08-23

[1.0.0-m.18]: https://github.com/atomist/artifact-source/compare/1.0.0-m.17...1.0.0-m.18

### Added

-   `Add function for listing issue events` [#49][49]

[49]: https://github.com/atomist/artifact-source/issues/49

### Changed

-   Simplified retrying of calls
-   All logging is now `debug`

## [1.0.0-m.17] - 2017-08-16

[1.0.0-m.17]: https://github.com/atomist/artifact-source/compare/1.0.0-m.16...1.0.0-m.17

### Fixed

-   Fixed `methods using httpRequestOption are not retried` [#46][46]

[46]: https://github.com/atomist/artifact-source/issues/46

### Changed

-   GitRepositoryCloner `clone` method now returns a `File` object and throws an 
    ArtifactSourceException in the event of a failure instead of returning an
    `Option[File]` [#47][47]

[47]: https://github.com/atomist/artifact-source/issues/47

## [1.0.0-m.16] - 2017-08-02

[1.0.0-m.16]: https://github.com/atomist/artifact-source/compare/1.0.0-m.15...1.0.0-m.16

### Changed

-   Simplified git package structure by removing `github` package and just using `git` package
    to contain all Git and GitHub artifacts

## [1.0.0-m.15] - 2017-07-31

[1.0.0-m.15]: https://github.com/atomist/artifact-source/compare/1.0.0-m.14...1.0.0-m.15

### Changed

-   Enhanced `searchRepositories` and `searchIssues` methods

## [1.0.0-m.14] - 2017-07-28

[1.0.0-m.14]: https://github.com/atomist/artifact-source/compare/1.0.0-m.13...1.0.0-m.14

### Added
    
-   Retry of all GitHub API calls

### Fixed

-   Fixed issue where git cloning was not working when a commit sha was specified

### Changed

-   **BREAKING** Removed `ArtifactSourceAccessException`, `ArtifactSourceUpdateException`, and `ArtifactSourceCreationException`
    in favour of just `ArtifactSourceException`, which is now a concrete class

## [1.0.0-m.13] - 2017-07-18

[1.0.0-m.13]: https://github.com/atomist/artifact-source/compare/1.0.0-m.12...1.0.0-m.13

### Fixed

-   Fixed issue where multiple blobs were created for the same file

### Changed

-   Removed ambiguous `toJson` method with an Option as parameter in `JsonUtils`

## [1.0.0-m.12] - 2017-07-11

[1.0.0-m.12]: https://github.com/atomist/artifact-source/compare/1.0.0-m.11...1.0.0-m.12

### Fixed

-   Fixed commitFiles to commit the right set of files [#38][38]

[38]: https://github.com/atomist/artifact-source/issues/38

## [1.0.0-m.11] - 2017-07-11

[1.0.0-m.11]: https://github.com/atomist/artifact-source/compare/1.0.0-m.5...1.0.0-m.11

### Changed

-   Removed auxiliary constructors in favour of companion object apply methods
-   Renamed `StringFileArtifact` `toStringFileArtifact` method to `apply`
-   Renamed `ByteArrayFileArtifact` `toByteArrauFileArtifact` method to `apply`
-   Prevent creating artifact sources having files with relative paths

### Fixed

-   Fixed `issue with editing new file` [#36][36]
-   Fixed issue where multiple updates to the same file were not being persisted to GitHub

[36]: https://github.com/atomist/artifact-source/issues/36

## [1.0.0-m.5] - 2017-06-28

[1.0.0-m.5]: https://github.com/atomist/artifact-source/compare/1.0.0-m.4...1.0.0-m.5

### Changed

-   Merged artifact-source and atomisthq/github-artifact-source repositories

## [1.0.0-m.4] - 2017-06-07

[1.0.0-m.4]: https://github.com/atomist/artifact-source/compare/1.0.0-m.3...1.0.0-m.4

### Fixed

-   Fixed `pathElementsFromFile` [#29][29]

[29]: https://github.com/atomist/artifact-source/issues/29

## [0.19.0]

### Changed

-   Added RepoArtifactSourceLocator, RepoBranchAndPath, GitArtifactSourceLocator,
    and GitArtifactSourceIdentifierwhich were previously in the 
    github-artifact-source repo
    
[Unreleased]: https://github.com/atomist/artifact-source/compare/0.14.4...HEAD

## [0.14.4]

[0.14.4]: https://github.com/atomist/artifact-source/compare/0.14.3...0.14.4

### Fixed

-   Changes are no longer lost when adding artifact sources
    https://github.com/atomist/rug/issues/199
