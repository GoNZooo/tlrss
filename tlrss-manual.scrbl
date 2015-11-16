#lang scribble/doc

@(require (for-label racket)
          scribble/manual
          scribble/basic
          scribble/extract)

@title{TLRSS}

@section{Configuration}
Every bit of configuration is to be done in the file @bold{'configuration.rkt'}. This file is imported in @bold{'download.rkt'}.

@subsection{downloads}
Download items are specified as follows, with a perl regexpression inside the string on each line.

Example:
@codeblock|{
(downloads
  "The.Big.Bang.Theory.S07E\\d\\d.720p.*"
  "Hells.Kitchen.US.S12E\\d\\d.*x264.*"
  "Game.of.Thrones.S\\d\\dE\\d\\d.720p.*"
  ".*Legend.[Oo]f.Korra.S03E.*720p.*")
}|

@subsection{user-rss-url}
The file we want to download from TL has a specific URI for
your user. You can find this in the "Profile" section on the page.

Example:
@codeblock|{
(rss-url "http://rss.torrentleech.org/<NUMBERS_AND_LETTERS>")
}|

@subsection{user-base-path}
This is simply the directory you want to place the downloaded torrent files in. It should optimally point to a directory your torrent client is watching for new files. This way, it will start downloading any torrent that is downloaded through the RSS, allowing fully automated downloads.

Example:
@codeblock|{
(base-path "<path to torrent file directory>")
}|

@section{Running the downloader}
Running the script/program is just like running any other Racket program and requires no setup beyond the initial personal configuration and downloading the Racket language toolkit.

@subsection{Without compiling}
If you want to run the script directly from the file you can run the downloader like so:

@codeblock|{$ racket download.rkt}|

The program will run in the current terminal window and should be downloading the RSS file immediately.

@subsection{Compiling a binary}

Compiling a binary to run is as simple as running the following command:

@codeblock|{$ raco exe -o ./rss-downloader download.rkt}|

This creates a binary in the current folder called @bold{'rss-downloader'} that can be run like this:

@codeblock|{$ ./rss-downloader}|

And this will start the program just like it would if it were run without creating the binary. Note that this does not improve the performance of the program, as a Racket script will be as performant as the compiled binary created from it.
