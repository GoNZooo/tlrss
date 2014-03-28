#lang scribble/manual

@(require (for-label racket))

@title[
#:date "2014-03-09"
"Setup"]

Every bit of configuration is to be done in the file @bold{'configuration.rkt'}. This file is imported in @bold{'download.rkt'}.

@section{Base components}
These are the basic contents of the file that need to be defined exactly as follows. This is so that the right symbols are exported so they can be used in other files.

@codeblock|{
#lang racket/base

(provide downloads
	 user-rss-url
	 user-base-path)
}|

@subsection{downloads}
This variable is a list of pairs. The left part (or 'car') of the cell contains the name you want to give the show. This has no bearing on anything at the moment, but is shown here because it makes it fairly readable.

The right part (or cdr) of the cell contains a perl regular expression. The leading @bold{#px} tells Racket that it is in fact a @hyperlink["http://www.anaesthetist.com/mnm/perl/regex.htm" "Perl Regular Expression"] and not a @hyperlink["http://www.regular-expressions.info/posix.html" "POSIX Regular Expression"]. This doesn't matter, as the matching function (@racket[regexp-match]) can use both, so you can replace the @bold{#px} with @bold{#rx} if you feel like it. Note, though, that the shorthands (\\d for digits) used in the examples are only available in perl regexes.

Example:
@codeblock|{
(define downloads
  (list '("Community" . #px"Community.S05E\\d\\d.720p.*")
  	'("Archer" . #px"Archer.S05E\\d\\d.720p.*")))
}|

@subsection{user-rss-url}
The file we want to download from TL has a specific URI for
your user. You can find this in the "Profile" section on the page.

Example:
@codeblock|{
(define user-rss-url "http://rss.torrentleech.org/<NUMBERS_AND_LETTERS>")
}|

@subsection{user-base-path}
This is simply the directory you want to place the downloaded torrent files in. It should optimally point to a directory your torrent client is watching for new files. This way, it will start downloading any torrent that is downloaded through the RSS, allowing fully automated downloads.

Example:
@codeblock|{
(define user-base-path "/home/gonz/torrents/torrentfiles/")
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

And this will start the program just like it would if it were run without creating the binary.