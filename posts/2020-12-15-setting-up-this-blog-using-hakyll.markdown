---
title: Setting up this blog using Hakyll
---

To kick things off, I will write about the birth of this blog. I decided to use 
Hakyll, which is a static site generator written in Haskell. It uses its own 
domain-specific language (DSL) for configuration, hosted within Haskell.

FYI: I wrote this in a Google docs and then transferred it to a real blog post 
once the site was set up.

# Installation

The setup process started off rather rocky - following the Hakyll tutorial, I 
ran `stack install hakyll` and immediately encountered the following error.

```abc
Error: While constructing the build plan, the following exceptions were 
encountered:

In the dependencies for hakyll-4.13.4.1:
    pandoc-2.7.3 from stack configuration does not match >=2.10 && <2.11 (latest
                 matching version is 2.10.1)
needed since hakyll is a build target.

Some different approaches to resolving this:

  * Set 'allow-newer: true'
    in /home/alyta/.stack/config.yaml to ignore all version constraints and 
    build anyway.

  * Recommended action: try adding the following to your extra-deps
    in /home/alyta/.stack/global-project/stack.yaml:

- pandoc-2.10.1@sha256:23d7ec480c7cb86740475a419d6ca4819987b6dd23bbae9b50bc3d42a
  7ed2f9f,36933

Plan construction failed.
```

Hakyll seems to be using a much older version of Pandoc than what is available. 
I tried following the recommended action of adding `pandoc-2.10` to my global 
dependencies, but this seems to lead down a rabbit hole where `pandoc-2.10` 
requires unmatched dependencies which themselves have unmatched dependencies, 
etc. I also tried setting `allow-newer: true`, in the hopes that Hakyll will be 
able to build with a newer version of Pandoc anyway, but alas that was not the 
case...

After a Google search for similar problems, the solution seems to be using a 
different package resolver. Indeed, the 
[stackage page for Hakyll](https://www.stackage.org/package/hakyll) mentions the 
`lts-16.26` resolver (at the time of writing) whereas my resolver on stack is 
`lts-14.17`. Therefore to "resolve" the issue, I just had to specify the 
resolver via `stack install hakyll --resolver lts-16.26`. Now Hakyll installs 
successfully!

# Initializing the site

The next step is to start a github pages repo and initialize a template Hakyll 
site in it. To start a github pages repo, all I had to do was create a repo with
the name `alyata.github.io`. Then any html files on the repo's master branch can
be accessed by opening `alyata.github.io/path/to/file.html` on the browser, 
though it takes a couple minutes to refresh pushed content sometimes. If there 
is a `index.html` file on the root of the repo, then this can be accessed just 
by opening the site `alyata.github.io`. 

I then cloned the repo and initialized a stack project inside the folder using 
the existing Hakyll project template: 
`stack new --bare alyata-blog hakyll-template --resolver lts-16.26`. 
Here's a dissection of the command:

- `new` - create a new project.

- `--bare` - create the project in the current directory. Default, non-bare 
  behavior is to create a new directory.

- `alyata-blog` - name of the project. This would be the name of the new 
  directory if --bare was not used.

- `hakyll-template` - use the Hakyll project template that initializes a bare 
  bones blog site.

- `--resolver lts-16.26` - use the same package resolver I installed Hakyll
    with.

This generated a haskell source file along with some directories and
configuration files which I committed to a new `develop` branch. Finally, I
compiled the project using `stack build`. The compiled program is the site
generator, which generates the html/css files for the actual site. I can run the
generator by running `stack exec alyata-blog build`, which will then generate
the entire site in the `_site` folder. This is slightly confusing as both are
build commands - just keep in mind that the latter builds the site, and the
former builds the builder... (Who watches the watchmen?)

To keep things separate, the code for the generator goes to the `develop`
branch, while the result of running the compiled generator goes to the `master`
branch to be displayed. To do this, I ran the generator, switched back to
`master` branch and copied everything in the output folder `_site` to the root
of the branch. After committing and pushing the freshly copied files, I was able
to see the template site online:

![Screenshot of the template site](../images/screenshot_of_site.png){width=100%}

I will have to do this every time I want to deploy a change in the `develop` 
branch, which sounds rather tedious. Therefore, my next step was to configure 
a deploy command for doing all of these steps automatically.

# Configuring deployment

# References
- https://jaspervdj.be/hakyll/tutorials/01-installation.html
- https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html

