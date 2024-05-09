# Topic Modeling Metrics in R and RCpp

This is a small project that I have done when exploring the area of Topic Modeling for St. Olaf's Center for Interdisciplinary Research project, where I implement several popular metrics for evaluating topic model, especially when we train the model on a large corpus of text and using different topic model libraries. The metrics that were implemented are:

- Topic Coherence (measure whether "top" words of a topic would go together in a document):
    + $C_v$ coherence metrics (from "Exploring the Space of Topic Coherence Measures" by Roder et al.)
    + Coherence metrics from "Topic Modeling in Embedding Spaces" by Dieng et al.
- Topic Diversity (measure where the "top" words of different topics are different) from "Topic Modeling in Embedding Spaces" by Dieng et al.

This repo is still in development process and any suggestion would be great!
