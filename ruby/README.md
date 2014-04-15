## HWO2014 Ruby bot template

Use MRI Ruby 2.0.0 if possible.

# Installing dependencies

## Step 1: Install Ruby

### A note for OS X users

If you are running Mavericks (10.9), you should already have MRI 2.0.0 and rubygems.

For older OS X versions install using homebrew or rvm (instructions below).

### Install Ruby using Homebrew (OS X only)

    brew install ruby20

### Install Ruby using RVM (OS X/Ubuntu/Debian)

Follow instructions for installing [rvm](http://rvm.io/rvm/install) (you will need [curl](http://curl.haxx.se/)).

Install Ruby:

    rvm install 2.0.0
    rvm use 2.0.0

## Step 2: Install bundler

Install bundler using rubygems:

    (sudo) gem install bundler

# Vendoring extra dependencies

If you need to use gems, they have to be vendored in the repository.

    bundle package
    git add vendor/cache
    git commit

In development use it's better **not** to use the `build` script (and delete
`.bundle/config`), and use `bundle install/update` directly.
