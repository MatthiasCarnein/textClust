# textClust

This R package implements an algorithm to cluster text streams as proposed in our paper:

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
## read text-stream from file
stream = DSD_ReadCSV("file.txt", sep = "\t", comment.char="", quote="")

## define text clustering algorithm
algorithm = DSC_textClust(r=.5, lambda=0.5, tgap=100, updateAll = F, nmin=1, nmax=1, k=10, verbose=F)

## run the algorithm
update(algorithm, stream, n=1000)

## plot the resulting micro-clusters using multi-dimensional scaling (MDS)
plot(algorithm, numRepresentatives = 2, type = "micro")

## or plot the final clusters
plot(algorithm, numRepresentatives = 2, type = "macro")
```

The algorithm can also be evaluated using prequential (also interleaved test-then-train) evaluation:

```R
evaluation = textClust::evaluate_cluster(algorithm, stream, measure=c("numMicroClusters", "purity"), n=500000, assign="micro", type="micro", assignMethod="nn", horizon=100)
```

## Data

The algorithm assumes that the data-file comes in the following form (without the header):

| Time | User | Text |
| -----| -----| ---- |
| 03/28/2017 14:59:48 | Albert Einstein |	The difference between stupidity and genius is that genius has its limits. |
| 03/28/2017 15:00:10 | Dennis |	Well said, Albert! |
| 03/28/2017 15:00:43 | Albert Einstein |	Thank you! |

In our paper, we used the implemented algorithm to cluster chat messages from the streaming platform Twitch. In general, however, any kind of text data can be analysed.
