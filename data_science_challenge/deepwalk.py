import numpy as np
import networkx as nx
from random import choice
from gensim.models import Word2Vec


############## Task 1
# Simulates a random walk of length "walk_length" starting from node "node"
def random_walk(G, node, walk_length):
	walk = [node]
	
	for i in range(walk_length-1):
		nbrs = list(G.neighbors(walk[-1]))
		
		#check if list is not empty
		if not len(nbrs) == 0:
			current_node = choice(nbrs)
			walk.append(current_node)

	walk = [str(node) for node in walk]
	return walk


############## Task 2
# Runs "num_walks" random walks from each node
def generate_walks(G, num_walks, walk_length):
	walks = []
	
	nodes = list(G.nodes())
	for i in range(num_walks):
		idx = np.random.permutation(len(nodes))
		for j in range(len(nodes)):
			node = nodes[idx[j]]
			walk = random_walk(G, node, walk_length)
			walks.append(walk)

	return walks

# Simulates walks and uses the Skipgram model to learn node representations
def deepwalk(G, num_walks, walk_length, n_dim):
    print("Generating walks")
    walks = generate_walks(G, num_walks, walk_length)

    print("Training word2vec")
    model = Word2Vec(size=n_dim, window=8, min_count=0, sg=1, workers=8)
    model.build_vocab(walks)
    model.train(walks, total_examples=model.corpus_count, epochs=5)

    return model