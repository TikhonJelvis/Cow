# Cow: Structural Version Control

Normal diff and merge tools treat programs as plain text. They are easily confused by superficial changes to the code and can't take advantage of the code's inherent structure for deeper analysis.

Language specific **semantic version control** tools can provide a high level of analysis but are tied to the syntax and semantics of specific languages. Tools that understand Java class and method declarations¹ can provide an improved diff and merge experience but don't easily generalize. You could extend the approach to languages with substantially similar models (like C♯) but working with radically distinct languages would require new logic.

Our tool offers a new middle ground: it takes advantage of the *structure* of the text without being tied to its semantics. By operating on **parse trees**, our algorithms can work for any language that we can parse while still giving more insight than a tool constrained to plain text.

## Architecture Overview

The current approach works as a pipeline of several distinct algorithms. Each step will produce an intermediate form with a clear interface, making it easy to plug additional logic in between any two steps.

  1. **Parsing**: the only language-specific part of the pipeline is producing the parse trees. In this context, a **parse tree** is a slightly lower-level representation than an abstract syntax tree: it's a tree that's only labeled at its leaves, with exactly one leaf node per token. Reading out the leaves in order complete recreates the input stream of tokens.
  
  Each language we support will probably need a custom parser, but this makes sense anyhow because the parsing behavior we need for diffing and merging is different from what a compiler requires:
  
    * We can make distinctions the compiler wouldn't like detecting statements grouped with blank lines.
    * We want to preserve as much lexical information as possible to recreate the program text—especially for merging.
    * Controlling how the code is parsed gives us fine control over the resulting diff, letting us selectively ignore things we don't care about. Think of this is a sophisticated, syntax-aware alternative to ignoring whitespace with normal diff.
  
  2. **Parse tree diff**: we find a minimal number of additions and deletions to go between two parse trees. The tree structure is used to consolidate multiple changes into one: if you changed most of the lines in a block of code, it's useful to think of that as a single action rather than a bunch of separate actions per line. The threshold for when to consolidate changes is based on a heuristic which will probably have to be tuned per-language.
  
  This diff is useful on its own to reflect the structure of your code and reduce some of the noise you can get with text-based diff algorithms. However, the main advantage here is for giving the diff some extra structure which can be taken advantage of in analysis passes after the tree diff.
  
  The current algorithm for calculating the needed changes is a slight modification of the [Wagner-Fischer] algorithm for normal string edit distance. It's based on dynamic programming and runs in O(n²) time and space where n is the total number of nodes in the tree (including internal nodes).
  
  3. **Substructure detection**: the next step identifies which blocks of code (ie subtrees) correspond between the two trees being compared, with some heuristic for determining when two subtrees are "close enough". This information is used to find blocks of code that were moved and can also be used for additional analysis by a plugin.
  
  It's useful to think of this as a graph problem where each subtree is a node with edges to each other subtree weighted based on some distance between them (perhaps based on the distances calculated in step 2). Given this graph, there are two possible approaches I'm considering:
  
    * *bipartite graph matching*: the earlier proof of concept found matching substructures by finding the best match between subtrees from the input and output and keeping all the resulting pairs that were close enough in weight. This approach is solid in common cases where you just move a block of code, but doesn't detect more complex transformations like copying a subtree into *two* places in the new tree.
    * *clustering*: the main alternative I'm considering now is some sort of clustering algorithm that tries to find all the subtrees that are "close enough". This will be able to detect more complex relationships in the code but will likely also depend more on the weights and heuristics used to identify clusters.
    
  4. **Merging**: the final (optional) step is merging and conflict resolution. One of the neat advantages to how the tree diff was designed for step 2 is that the resulting diff is a parse tree itself with some additional annotations. This allows us to find conflicts by doing a diff-of-diffs.
  
  Once we have a diff-of-diffs, we should be able to resolve more conflicts than a purely text-based system thanks to the high-level information we derived in the previous steps. For example, we can apply *both* a move *and* a modification to a block even if the changes physically overlap in the text.
  
  This step is the least-developed in the current system. The basic idea worked well in the old proof-of-concept but I've rethought many of the details of the previous steps without revisting merging. I'll put more thought and work towards merging once I get the previous few steps working satisfactorily.


## Progress

The current code is really rough: it's barely at a proof-of-concept stage, if that. It's entirely experimental and not really usable at all!

Most of the code comes from a few years ago and is purely exploratory in nature. It was the final project for CS164, the programming languages class at Berkeley. I worked on the project with Ankur Dave whose insights helped shaped much of the design.

I didn't really know what I wanted to accomplish so it's rough and hard to follow. I also never managed to get the diff algorithm working efficiently (partly because it was never well-specified), so never tested anything on non-trivial amounts of code.

I've started working on the project again recently. Thanks largely to collaboration with Dimitri DeFigueiredo I've come up with a significantly more concrete idea of what I want to accomplish in each part of the system, and I've figured out an efficient way to implement the parse tree diff algorithm based on [Wagner-Fischer].

Now I'm reimplementing the various pieces with my new specification in mind and working out all the details. I don't have much new code yet but at least I know where I'm going with it! The next step is debugging my new tree diff algorithm, writing some code for visualizing trees and writing up a draft of a paper about this to make all the ideas completely concrete, following Simon Peyton Jones's advice for doing research "paper first". (I guess this README is a rough draft of sorts, but it needs significant reworking!)
