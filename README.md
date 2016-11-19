# Atomist 'artifact-source'

![Build Status](https://travis-ci.com/atomist/artifact-source.svg?token=z8gZcM8P34vnbufzQBTa&branch=master)

Defines an ArtifactSource abstraction for source code

Scala library exposing core ArtifactSource abstraction, representing a set of artifacts (probably source code) that may be read from
or written to a range of sources, including the file system, a zip file or (in a separate library built on this), GitHub.

This library is used by most Atomist Java and Scala projects.

## Design notes

An ArtifactSource is typically constructed in one of two ways:
* To front a directory-oriented abstraction such as a file system. In this case, directories will front
their counterparts in the other representation.
* Through adding files containing path information. In this case, directories will be inferred.

## Path conventions

Files and directories can be given directory paths. Paths begin without any special character, and use / as a delimiter.
E.g. src/main/java