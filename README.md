# Atomist 'artifact-source'

[![Build Status](https://travis-ci.org/atomist/artifact-source.svg?branch=master)](https://travis-ci.org/atomist/artifact-source)
[![Slack Status](https://join.atomist.com/badge.svg)](https://join.atomist.com/)

Defines an ArtifactSource abstraction for source code.

## About

A Scala library exposing the core ArtifactSource abstraction,
representing a set of artifacts (probably source code) that may be
read from or written to a range of sources, including the file system,
a zip file or (in a separate library built on this), GitHub.

This library is used by most Atomist Java and Scala projects.

## Design notes

An ArtifactSource is typically constructed in one of two ways:

*   To front a directory-oriented abstraction such as a file
    system. In this case, directories will front their counterparts in
    the other representation.
*   Through adding files containing path information. In this case,
    directories will be inferred.

## Path conventions

Files and directories can be given directory paths. Paths begin
without any special character, and use / as a delimiter, e.g.,
`src/main/java`.

## Using

Most users will not need to use this project directly, but will use
tools, e.g., [rug-cli][cli], that build on this project.

[cli]: https://github.com/atomist/rug-cli

If you wish to develop tools using this project, you will need to add
this project as a dependency and the maven repository where it is
published to your build tool's configuration.  For example, if you use
maven, add the dependency to the `<dependencies>` section and the
repository to the `<repositories>` section of your `pom.xml`:

```xml
<?xml version='1.0' encoding='UTF-8'?>
<project
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">
	<modelVersion>4.0.0</modelVersion>
    ...
    <dependencies>
        ...
        <dependency>
            <groupId>com.atomist</groupId>
            <artifactId>artifact-source</artifactId>
            <version>0.5.0</version>
        </dependency>
        ...
	</dependencies>
    ...
	<repositories>
		<repository>
			<id>public-atomist-release</id>
			<name>Atomist Release</name>
			<url>https://atomist.jfrog.io/atomist/libs-release</url>
			<snapshots>
				<enabled>false</enabled>
			</snapshots>
		</repository>
	</repositories>
    ...
</project>
```

Be sure to change the `<version>` to the one you want to use.

## Support

General support questions should be discussed in the `#support`
channel on our community slack team
at [atomist-community.slack.com](https://join.atomist.com).

If you find a problem, please create an [issue][].

[issue]: https://github.com/atomist/rug-resolver/issues

## Development

You can build, test, and install the project locally with [maven][].

[maven]: https://maven.apache.org/

```sh
$ mvn install
```

To create a new release of the project, simply push a tag of the form
`M.N.P` where `M`, `N`, and `P` are integers that form the next
appropriate [semantic version][semver] for release.  For example:

```sh
$ git tag -a 1.2.3
```

The Travis CI build (see badge at the top of this page) will
automatically create a GitHub release using the tag name for the
release and the comment provided on the annotated tag as the contents
of the release notes.  It will also automatically upload the needed
artifacts.

[semver]: http://semver.org
