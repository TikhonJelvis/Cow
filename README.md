# Cow: Semantic Version Control

Normal diff and merge tools treat programs as plain text. This means they are easily confused by superficial changes to the code.

Our goal is simple: we want to take advantage of a program's underlying structure to get a higher-level, more accurate diff and merge. We want to perform a "semantic" diff of the program rather than blindly comparing the text.

Our approach is also simple: instead of diffing the text directly, we parse the code and operate on the resulting ASTs. This is what allows us to perform sophisticated analysis and improve both diffing and merging. Apart from immediately improving diffing and merging, we also create a platform for semantic diff--we provide the tools needed to add more behavior based on diffs and merges of source trees. This is good for both improving the diff quality and implementing completely new capabilities.

## Progress

We currently have basic tree diff and merge support. We can also match substructures in two trees, find moves and accurately find conflicts. There is also some basic visualization, but no coherent UI.

The only language currently supported is JavaScript; however adding a new language is not particularly difficult. 

The main downside is performance: it is horrible. Small examples work fast enough, but the current tree diff implementation is in O(2^n). This is the main problem we're trying to fix right now.
