NGramCrackers! CLI for quantitative text analysis.
===

LICENCING INFO
---
NGramCrackers, Copyright 2015, Arianna Morgan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

Project Purpose
---
While studying Applied Linguistics and working on my MA, I found the study
of automated, corpus-based text analysis. While the analyses were fascinating,
the actually tools were frustrating for two reasons:

- The software was non-Free, and thus not easily extendedable or patchable
- The software interfaces were not satisfactory

### Free/Libre software and Applied Linguistics
Corpus linguists need high quality, Free software analytic tools that are
capable of handling 5+ million word corpora spread over many documents.
Corpus linguists should not be bound to proprietary OSes because the best
tools available are not cross-platform. 

Unfortunately, many of the most well-known programmes in my AppLing circles are 
proprietary. This is to say that the copyright holders have likely not published 
the source code -- or if they have, without the freedom to modify and distribute
the source code. However, I prefer to use Free( https://www.gnu.org/philosophy/free-sw.html), 
software.  Free, though, is not the same as *gratis*. Free software is that for
which the source code is available and modifiable. While Free software is often
gratis, it need not be.

Publishing source code and permitting modification and redistribution has
several advantages over the proprietary model. First, others can study the
source to *understand* how the programme does what it does. Second, other users
can implement new features or otherwise extend the programme. The developer(s)
can then choose to accept or reject the proposed change or extension. The dev(s)
then has been saved work! Finally, publishing the source code saves everyone 
work. Another programmer can then build on previously done work. Thus, the
species does not have to keep reinventing the wheel, as it were.

In Applied Linguistics, though, there is much we can't be sure about in the
common software for corpus-based analytics. For instance, MonoconcPro's language
of implementation is not readily acertained, much less the source code. 
Similarly, AntConc, which is provided gratis and written in Perl (as far as I 
know), does not ship with the source! If this has changed, I'd love to know. 

As a Free software user, programmer, and advocate, this frustrated me to no end
because the programmes simply did not do all of what I wished, and there was no
way to fix them to provide that functionality to myself and others. 

It is also bit disturbing when academics, especially those with tenure, release
proprietary software without source code. Although there is nothing statutorily
criminal in this, I do believe it violates the idea of education as a force of
intellectual liberation. Failing to release the source code of programmes 
impedes the further pursuit of knowledge that should be at the centre of 
academia. This (in)action also makes independent peer review more challenging or
even impossible. If your code isn't easily peer-reviewable, why should anyone 
use, much less trust or trust or pay, for it? To this end, this code has been
licenced under the GNU GPL v3 to ensure that all derivative works are also Free.
As an educator, this is critical and why a modified BSD or MIT type licence is
not satisfactory for me.

### CLI-based solutions for quantitative text analysis
My other gripe is the interface. Programmes like MonoconcPro or AntConc are 
intended for Applied Linguists who are less like to be Computational Linguists
who do not use the CLI. Thus, in order to promote accessibility, these
programmes make heavy use of graphical user interfaces (GUIs). While many users
expect this functionality, a GUI can reduce the productivity of users who mostly
use the CLI. 

At the other end of the spectrum are full NLP programming libraries. When I was
thesising, I was not a good enough programmer to pick up a new language super
quickly, much less implement all the functions I needed. I was trapped in the
odd limbo of hating GUIs but not being a skilled enough programmer to figure
a solution with a programming library.

These experiences inspired the idea of an intermediary step from consumer-GUI
frontends to fully functional text processing libraries and  APIs.  Thus, the
idea of NGramCrackers was born: A GPLed, CLI for doing simple text analysis.

### But Python NLTK!

While I have played with the wonderful Python Natural Language Toolkit, I am not
a Pythonista. Python is a great language, and I encourage friends who want to
start coding to learn it. That said, I see no reason to limit development of NLP
tools to Python.

I was first introduced to Haskell in about 2005 by a friend who was writing an
HPSG grammar of Dutch in it. He told me that because of Haskell's rich,
strict typing system, it was possible to write verifiable code. I didn't
understand the significance at the time, but it sounded sweet. Fast forward 9
years, I decided to give it a try as my first general-purpose language because
of these reasons, plus the possibility for excellent performance from a
high-level language, with developed libraries for concurrent and parallel
computing.

### Won't a CLI make it in accessible to your target audience?

Initially, perhaps, but by waiting to develop a GUI, the text processing can
exist independently of the GUI and other interfaces could be developed. My own
interest is in primarily in developing the CLI because I interact with my system
almost exclusively through the terminal, so this interface will be the most
immediately useful.

The gaps this project is intended to fill:

 - Simple commandline tool for text analysis, similar to Monoconc or Antconc
   in functionality. NGramCrackers falls in between limited-platform 
   proprietary binaries distributed to be consumed (MonoconcPro or Antconc) 
   and fully developed NLP APIs (e.g., NLTK).

 - Tools for intermediate to advanced users of GNU/Linux or other 'nixen.
   Windows functionality will depend on my ability to cross-compile to
   such a binary.
 
 - A concordancer that is capable of handling huge corpora and making use of
   parallel computing resources.
  
I have met few programmers like me: non-professional, self-taught (with 
community support!), hobbiest programmers. During the writing of my thesis,
I was frustrated by what sort of tools were available: Consumer products 
intended for non-programmers or full libraries for much better programmers 
than I was at that time. NGramCrackers is both a CLI tool and Libre backend
API, written in Haskell, of the same name.

 - NGramCrackers is intended to provide easy outputting of usefully formatted
   (e.g., .csv, .tsv, etc.) data. No one should ever have to manually copy and
   paste output from a GUI to a spreadsheet programme to get a CSV to use in 
   an analysis conducted with external software.

Versioning Information
------------------------

Below are the definitions of versions for this project. The definition for the
next release and later versions constitutes the functionality goals for 
NGramCrackers. The programme is still quite unstable with major API changes
planned.

### Version 0.2.7
New features added in this latest version of NGramCrackers! Now included are:

 - ngram extraction from CLI

 - words/sent statistics and access to them from CLI

 - Map- and Set-based look up for word/phrasal stats

 - Bigram Mutual Information calculation for all bigrams in a parsed document

This version has become rather expanded than originally intended. 

### Version 0.2.6 
More major breaking API changes have been made:

 - Hierarchical module system more inline with the usual Haskell module style

 - Scripts to allow compilation and execution of a binary with profiling
   capabilities enabled. The scripts must be sourced for the functions to
   available. ngc-pure executes the binary without profiling.

### Version 0.2.5
 - Return of multi-paragraph Parsing

### Version 0.2.4

Major, breaking API changes have been implemented!

 - All code has been refactored to use Data.Text as the basic
   type of text representation. This includes processing functions,
   parsers, and IO. However, older versions of String processing
   functions are still included in the source but are not actually
   called.

 - The CLI is currently only processing single paragraphs at a time
   because of the difficulties associated with multiple possible
   paragraph separators. Addressing this likely requires some way to
   try parsers without consuming input. 

 - Rudimentary metadata parsing funcitonality

 - Expanded type system for metadata descriptions

### Version 0.2.3
New features:
 - Cabal-based build system for installation in a cabal sandbox. 

 - Skeleton testing facilities. Only one test is implemented, so
   this is very barebones.

 - More complete directory structure, along with specific example
   texts that work with the binary.

 - The commandline options have been exapanded to take bigram and
   trigram extraction specifications

 - NGramCrackers.StringOps module introduced to hold operations that
   take Strings as the base processing unit. Since parsing involves
   putting elements into lists, it seemed most effective to transition
   towards a more list based set of operations. 

   This module contains some string operations that need to be
   used in the processing stage so as not to entirely break
   the build, but they will likely be phased out at some point.

### Version 0.2.2
 - Changes to backend profiling functions now reflect their general
   nature: Such functions will also profile larger ngrams than just
   words.

### Version 0.2.1
 - Limited multiparagraph parsing is now available in NGramCrackers!

### Version 0.2.0 
 - Rudimentary profiling and extraction capabilities.
     
  NGramCrackers can now print a profile of a single paragraph input file
  to a specified output file. The statistics provided are a word count
  or a type-token-ratio report.

  These very basic capabilities provide a very useful skeleton for adding
  more functionality, such as ngram extraction, and more statistics, such
  as mean words per sentence, mean words per paragraph, mean sentences per
  paragraph, etc.

### Version 0.1.1
 - Modualised argument handling system for ease of future development. 
   Future modifications to argument handling can take place in a contained
   environment and can be reused in other applications

### Version 0.1.0 -- Release Date: 04.01.2015

Primary features:

 - First public alpha release. The CL tool and the backend library will be
    changing a great deal.

 - Very basic commandline argument parsing that enables the 
   specification of input and output files.

 - Basic word count properties and type-token ratio functionality

 - Separation of version, help, summary, etc. from myArgs, the argument
   record for the programme. getOpts is an abstraction to handle the use of
   cmdArgs before being passed into the optionHandler

Future Versions
---
Planned releases. These are in a high degree of flux and will be tweaked as
needed for the forseeable future. Version 0.2.4 is untagged in the commit tree
because of poor planning on the part of yours truly. Version 0.2.5 has been
properly tagged in the tree. Sorry about that if you find this! <3 rm

### Version 0.2.8 -- Definition
 - Bigram Mutual Information calculation CLI integration
 - More concide argument handling case statement. This involves moving the
   behaviour of each case into their own functions.
 - Test module with various types of text that are known to work with the
   current version of NGC

### Version 0.2.9 -- Definition
 - Metadata analytics
 - Trigram Mutual Information calculation and CLI integration

### Version 0.3.0 -- Definition (Break up into other releases)
- Specific Data Types:
  - NGrams (including 1-grams, i.e., words)
  - Sentences
  - Paragraph

- Pretty printing capabilities
- Metadata analytics
- Vector-based sequences?

### Version 0.4.0 -- Definition
- Literate Haskell Documentation

### Version 1.0.0  -- Definition 4, Jan, 2015

My vision for 1.0.0 is a fully functional text analysis programme. This project
is in the early stages, so I may revise this standard substantially over the
coming months. 

Version 1.0.0 should include:

- Output options:
    
 - CSV
 - TSV
 - Tabular output for human readability

- Concordancing capabilities

  - N-gram extraction
  - Frequencies
  - MI and LL computations
  - T-scores
  - Other text stats
  - Number of differents files that the ngram occurs
   - Possible across genres
  
- Explicit corpus metadata parsing and analysis capabilities

- Fully specified data types for:
     
  - Words
  - Sentences
  - Paragraphs
  - Documents
  - Metadata
  - Corpora

- Descriptive test statistics:
     
  - Corpora
  - Documents
  - Paragraphs
  - Words

   - Very useful to consider things like, mean/variance words per sent, 
     paragraph in a Document or Corpus. 

- CLI Argument Handling

- Testing Suite implemented in quickCheck

- Cabal-based installation of library!

- Plugable format parsing and analysis.

- Support for popular text formats and API output formats:
 - HTML
 - SGML
 - JSON-based API output
 - Markdown
 - Twitter/GNUSocial
  
- Literate Haskell documentation
