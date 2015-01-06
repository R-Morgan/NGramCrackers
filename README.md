NGramCrackers! A program for quantitative text analysis.
===

LICENCING INFO
---

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
While studying Applied Linguistics and working on my MA, I got into the study
of automated, corpus-based text analysis. Unfortunately, many of the most
well-known programmes in my AppLing circles are non-Free. For instance, the
language of implementation of MonoconcPro is not readily acertained, much less
the source code. Similarly, AntConc, which is provided gratis, does not ship 
with the source! If this has changed, I'd love to know. 

As a Free software user, programmer, and advocate, this was extremely 
frustrating because the progs simply don't do what I wish, and there was
no way to fix it to provide that sort of functionality to other users. Moreover, 
these programmes were designed with non-expert shell users in mind, so they 
feature GUIs that can get in the way of more advanced users. 

Thus, the idea of NGramCrackers was born: A GPLed, CLI for doing simple text
analysis. 

### But Python NLTK!

While I have played with the wonderful Python Natural Language Toolkit, I am not
a Pythonista. I was drawn to Haskell because of the possibility for excellent
performance from a high-level language. I've also been obsessed with the idea of
pure code and the verifiability that Haskell's strict typing provides since I 
first heard about the language in 2005.

Generally, I tell my friends who are keen to start programming to learn Python,
but I prefer writing Haskell.

### Won't a CLI make it in accessible to your target audience?

Initially, perhaps, but by waiting to develop a GUI, the text processing can
exist independently of the GUI. My own interest is in primarily in developing
the CLI because that is most useful to me personally. I interact with my system
almost exclusively through the terminal, so this interface will be the most
immediately useful.

The gaps this project is intended to fill:

  -- Simple commandline tool for text analysis, similar to Monoconc or Antconc
     in functionality. NGramCrackers falls in between limited-platform 
     proprietary binaries distributed to be consumed (MonoconcPro or Antconc) 
     and fully developed NLP APIs (e.g., NLTK).

  -- Tools for intermediate to advanced users of GNU/Linux or other *nixen.
     Windows functionality will depend on my ability to cross-compile to
     such a binary.
    
     I have met few programmers like me: non-professional, self-taught (with 
     community support!), hobbiest programmers. During the writing of my thesis,
     I was frustrated by what sort of tools were available: Consumer products 
     intended for non-programmers or full libraries for much better programmers 
     than I was at that time. NGramCrackers is both a CLI tool and Libre backend
     API, written in Haskell, of the same name.

     Corpus linguists need high quality, Free software analytic tools that are
     capable of handling 5+ million word corpora spread over many documents.
     Corpus linguists should not be bound to proprietary OSes because the best
     tools available are not cross-platform. Similarly, there needs to be an
     intermediary step from consumer-GUI frontends to fully functional text
     processing APIs.

  -- NGramCrackers is intended to provide easy outputting of usefully formatted
     (e.g., .csv, .tsv, etc.) data. No one should ever have to manually copy and
     paste output from a GUI to a spreadsheet programme to get a CSV to use in 
     an analysis conducted with R (or whatever other statistical package).

Versioning Information
------------------------

Below are the definitions of versions for this project. The definition for the
next release and later versions constitutes the functionality goals for 
NGramCrackers. The programme is still quite unstable with major API changes
planned.

### Version 0.1.1
  -- Modualised argument handling system for ease of future development. 
     Future modifications to argument handling can take place in a contained
     environment and can be reused in other applications

### Version 0.1.0 -- Release Date: 04.01.2015

Primary features:

  -- First public alpha release. The CL tool and the backend library will be
     changing a great deal.

  -- Very basic commandline argument parsing that enables the 
     specification of input and output files.

  -- Basic word count properties and type-token ratio functionality

  -- Separation of version, help, summary, etc. from myArgs, the argument
     record for the programme. getOpts is an abstraction to handle the use of
     cmdArgs before being passed into the optionHandler

Future Versions
---

### Version 0.2.0
  -- Integration of paragraph parsing function into commandline user interface
  -- Multi-paragraph Parsing

### Version 0.3.0 -- Definition
  -- Specific Data Types:
     -- NGrams (including 1-grams, i.e., words)
     -- Sentences
     -- Paragraph

### Version 0.4.0 -- Definition
  -- Multimode CLI options

### Version 1.0.0  -- Definition 4, Jan, 2015

My vision for 1.0.0 is a fully functional text analysis programme. This project
is in the early stages, so I may revise this standard substantially over the
coming months. Version 1.0.0 should include:

  -- Output options:
     
     -- CSV
     -- TSV
     -- Tabular output for human readability

  -- Concordancing capabilities

  -- N-gram extraction

     -- Frequencies
     -- MI and LL computations
     -- T-scores
     -- Other text stats
     -- Number of differents files that the ngram occurs
        -- Possible across genres
  
  -- Explicit corpus metadata parsing and analysis capabilities

  -- Fully specified data types for:
     
      -- Words
      -- Sentences
      -- Paragraphs
      -- Documents
      -- Metadata
      -- Corpora

  -- Descriptive test statistics:
     
     -- Corpora
     -- Documents
     -- Paragraphs
     -- Words

     -- Very useful to consider things like, mean/variance words per sent, 
        paragraph in a Document or Corpus. 

  -- CLI Argument Handling

  -- Testing Suite implemented in quickCheck

  -- Cabal-based installation of library!

