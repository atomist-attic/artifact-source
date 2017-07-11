# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [1.0.0-m.11] - 2017-07-11

[1.0.0-m.11]: https://github.com/atomist/artifact-source/compare/1.0.0-m.10...1.0.0-m.11

### Changed

-   Removed auxiliary constructors in favour of companion object apply methods
-   Renamed `StringFileArtifact` `toStringFileArtifact` method to `apply`
-   Renamed `ByteArrayFileArtifact` `toByteArrauFileArtifact` method to `apply`
-   Prevent creating artifact sources having files with relative paths

### Fixed

-   Fixed `issue with editing new file` [#36][36]
-   Fixed issue where multiple updates to the same file were not being persisted to GitHub

[36]: https://github.com/atomist/artifact-source/issues/36

## [1.0.0-m.10]

[1.0.0-m.10]: https://github.com/atomist/artifact-source/compare/1.0.0-m.9...1.0.0-m.10

## [1.0.0-m.9]

[1.0.0-m.9]: https://github.com/atomist/artifact-source/compare/1.0.0-m.8...1.0.0-m.9

## [1.0.0-m.8]

[1.0.0-m.8]: https://github.com/atomist/artifact-source/compare/1.0.0-m.7...1.0.0-m.8

## [1.0.0-m.7]

[1.0.0-m.7]: https://github.com/atomist/artifact-source/compare/1.0.0-m.6...1.0.0-m.7

## [1.0.0-m.6]

[1.0.0-m.6]: https://github.com/atomist/artifact-source/compare/1.0.0-m.5...1.0.0-m.6

## [1.0.0-m.5] 

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
