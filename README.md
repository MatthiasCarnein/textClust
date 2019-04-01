# textClust

This R package implements an algorithm for topic discovery by clustering text streams as proposed in our paper:

> Carnein M., Assenmacher D. and Trautmann H. (2017), "Stream Clustering of Chat Messages with Applications to Twitch Streams", In Advances in Conceptual Modeling: ER 2017 Workshops AHA, MoBiD, MREBA, OntoCom, and QMMQ, Valencia, Spain, November 6--9, 2017, Proceedings. , pp. 79-88. Springer International Publishing.


The algorithm uses a widely popuar two-phase clustering approach where the stream is first summarised in real time. The result are many small preliminary clusters in the stream called 'micro-clusters'. Our micro-clusters maintain enough information to update them over time and also efficiently calculate the cosine similarity between them based on the Tf-idf vector of their texts. Upon request, the miro-clusters can be reclustered to generate the final result using any distance-based clustering algorithm such as hierarchical clustering. To keep the micro-clusters up-to-date our algorithm applies a fading strategy where micro-clusters that are not updated regularly loose relevance and are eventually removed.


## Installation

The easiest way to install the package is by using devtools:

```R
devtools::install_git("https://wiwi-gitlab.uni-muenster.de/stream/textClust")
```

Alternatively, the package can be build from source.


## Usage

Usage and interfaces are largely based on the R-package [stream](https://github.com/mhahsler/stream) with modifications for the analysis of text data

```R
library(textClust)
library(stream)

## define data stream
data = data.frame(text=sample(c("Main Topic", "Similar Topic", "Something Different"), size=1000, replace=T),stringsAsFactors=F)
stream = DSD_Memory(data) 

# Alternatively read data from file:
# stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")

## define text clustering algorithm
algorithm = DSC_textClust(r=.4, lambda=0.1, tgap=100, nmin=1, nmax=2, k=3, stopword=c(), minWeight=3, textCol=1)

## run the algorithm
update(algorithm, stream, n=1000)

## get micro clusters
get_centers(algorithm, "micro")

## get macro clusters
get_centers(algorithm, "macro")


## Assign new texts to existing clusters
data = data.frame(text=sample(c("Main Topic", "Something Different"), size=100, replace=T),stringsAsFactors=F)
get_assignment(algorithm, data)
```

The algorithm can also be evaluated using prequential (interleaved test-then-train) evaluation:

```R
evaluation = textClust::evaluate_cluster(algorithm, stream, measure=c("numMicroClusters", "purity"), n=1000, assign="micro", type="micro", assignMethod="nn", horizon=100)
```
