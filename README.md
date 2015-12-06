NGramCrackers! CLI for quantitative text analysis.
===

LICENCING INFO
---
NGramCrackers, Copyright 2014-2015, Arianna Morgan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

Project Purpose
---
My graduate work in Applied Linguistics required the study of automated, 
corpus-based text analysis. While the analyses were fascinating, the actual 
tools were frustrating for several reasons:

- The graphical interfaces were clunky and unsatisfactory
- The programmes would not output results in useable formats
- A lack of metadata analytics
- The software was non-Free, and thus not easily extendedable or patchable

### Free/Libre software and Applied Linguistics
Unfortunately, many of the most well-known programmes in my AppLing circles are
proprietary. This is to say that the copyright holders have likely not published
the source code -- or if they have, without the freedom to modify and distribute
the source code. Corpus linguists should not be bound to proprietary software
because the best tools available are not cross-platform.

These researchers need high-quality, [Free](https://www.gnu.org/philosophy/free-sw.html) 
software analytic tools because this blackbox approach to research is not
adequate. These tools must capable of handling corpora of millions--or
billions!--of words that are spread over many documents. Current concordance
software also seems to lack the capacity for automated metadata analysis.

In Applied Linguistics, though, there is much we can't be sure about in the
common software for corpus-based analytics. For instance, MonoconcPro's language
of implementation is not readily ascertained, much less the source code.
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
use, much less trust or trust or pay, for it? As a side note, *patenting*
software is ijustcanteven.

To this end, this code has been relicenced under the [GNU AGPL 
v3](https://gnu.org/licenses/why-affero-gpl.html) to ensure that all derivative
works are also Free. As an educator, this is critical and why a [modified
BSD](https://www.gnu.org/licenses/license-list.html#ModifiedBSD), [Expat
(MIT)](https://www.gnu.org/licenses/license-list.html#Expat), [Apache
2.0](https://www.gnu.org/licenses/license-list.html#apache2), or other Free, 
permisive, non-copyleft licence is not satisfactory for me. Even the GPL v3 does
not offer strong enough protections because NGramCrackers could be swept up into
a software-as-a-service without provision of the source code. I will not 
tolerate this software being used to take advantage of those seeking to educate
themselves. NGramCrackers is chiefly an educational project for the public good,
and it must always stay Free.

### CLI-based solutions for quantitative text analysis
My other gripe with the most common concordance software are the interfaces. 
Programmes like MonoconcPro or AntConc feature Graphical User Interfaces (GUIs)
in order to promote accessibility. While many users expect this functionality, 
a GUI can reduce the productivity of users who mostly use the CLI. 

At the other end of the spectrum are full NLP programming libraries. When I was
thesising, I was not a good enough programmer to pick up a new language super
quickly, much less implement all the functions I needed. I was trapped in the
odd limbo of hating GUIs but not being a skilled enough programmer to figure
a solution with a programming library in a timely fashion.

These experiences inspired the idea of an intermediary step from consumer-GUI
frontends to fully functional text processing libraries and  APIs. Thus, the
idea of NGramCrackers was born: A GPLed, CLI for doing simple text analysis.

### But Python NLTK!

While I have played with the wonderful [Python Natural Language 
Toolkit](http://www.nltk.org/), I am not a Pythonista.
[Python](https://www.python.org/) is a great  language, and I encourage friends
who want to start coding to learn it. That said, I see no reason to limit
development of NLP tools to Python or any particular language.

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

Module Map
------------------------
NGramCrackers has many submodules that may need some explanation. The modules
are currently divided into five sets:

### Top Level
Where NGramCrackers.DataTypes lives! All types and typeclass instances are
currently defined here.

### Ops
Operations on various types of text bop about in here, including Text, String,
and the NG record type defined in DataTypes.
 - Infixes.hs are infixes, often to obviate the need for backticking functions
 - NG.hs contains operations based on the NG type
   most current API.
 - String.hs and Text.hs are based on the triply-nested lists, thus these are
   deprecated modules.
 - Pretty.hs is an output formatting module

### Parsers
The parsers party in this module!
 - Args.hs contains CLI argument handling
 - Body.hs contains parsers for the body of a document
 - Metadata.hs contains parsers for a document's metadata

### Quant
The math operations live here!
 - Count.hs houses operations for counting ngrams
 - Dispersion.hs contains statistical functions for calculating means and
   variances. The code in here is really old and quite inelegant.
 - Stats.hs contains statistical operations for use on language (e.g. MI)

### Utilities
Need some List and Tuple ops? Look no further! Not totally sure that Utilities
and Ops are all that different. This set of modules may be merged into ops.

Versioning Information
------------------------

Below are the definitions of versions for this project. The definition for the
next release and later versions constitutes the functionality goals for 
NGramCrackers. The programme is still quite unstable with major API changes
planned.

### Version 0.3.0 -- Definition
Version 0.3.0 is a total retooling of the backend API. NGramCrackers now feels
like a much different program, so I've overhauled the planned versions.

 - NG a, a record type to house ngrams and some of their associated properties.
   NGs are Functors and Monoids. The old API based around nested lists of Text
   ([[[T.Text]]]) worked, but I want to be able to have ngrams in the programme
   to be able to carry around information, such as frequency and MI, without
   each statistic having to be calculated each time. I am not sure it will save
   memory or make the programme faster, however. MI seems likely to be extremely
   costly in computing, especially for multiword strings. Similarly, if an ngram
   has its word length encoded, there is no need to break up the T.Text
   component and iterate through it to provide a word count.

 - Major, breaking changes to module naming and structure:
     - NGramCrackers.Ops.Infixes to hold any infixes
     - NGramCrackers.Ops.NG for operations on the new NG record type
     - NGramCrackers.Ops.Pretty for formatting output
     - NGramCrackers.Parsers.Body used to be NGramCrackers.Parsers.Paragraph
       but now reflects the module's actual function, which is to parse the
       bodies of input documents

 - Type synonyms implemented for:
     - SentColl -- collection of n-grams in a sentence
     - ParaColl -- collection of n-grams in a paragraph
     - DocCol -- collection of n-grams in a multiparagraph document
     - Count -- an integer representing counts of words (wish these could be
       restricted to the Natural numbers)
     - CrackerMap and CrackerSet for long type signatures that are a pain to
       type

 - Slightly more concise argument handling statements in a more readable format

 - More exposed functions for testing

 - Relicensure under the [AGPL v3](https://gnu.org/licenses/why-affero-gpl.html)

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

Future Versions
---
Planned releases. These are in a high degree of flux and will be tweaked as
needed for the forseeable future. Version 0.2.4 is untagged in the commit tree
because of poor planning on the part of yours truly. Version 0.2.5 has been
properly tagged in the tree. Sorry about that if you find this! <3 rm

### Version 0.3.x -- Planned Features
Features for future implementation. When version 0.3.0 is finished up, the next
steps will be figuring out exactly the order of implementation for the next
versions.

- Modification to NG type to include an MI field

- Bigram Mutual Information calculation CLI integration

- Test module with various types of text that are known to work with the
  current version of NGC

- Metadate parsing

- Combination of document and metadata parsing

- Intradocument Metadata analytics

- Trigram Mutual Information calculation and CLI integration

- More shell functions for running the programme in different ways, both for
  demonstration and profiling purposes

- Haddock generated documentation

- Vector-based sequences?

### Version 0.4.0 -- Planned Features
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
  - Mutual Information (MI) and Log Likelihood (LL) computations
  - T-scores
  - Other text stats
  - Number of differents files that the ngram occurs
   - Possible across genres
  
- Explicit corpus metadata parsing and analysis capabilities

- Fully specified data types for:
     
  - NGrams
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

- Testing suite

- Cabal-based installation of library!

- Plugable format parsing and analysis.

- Support for popular text formats and API output formats:
 - HTML
 - SGML
 - JSON-based API output
 - Markdown
  
- Literate Haskell documentation
