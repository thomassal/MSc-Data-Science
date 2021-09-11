import csv
import numpy as np
import networkx as nx

import torch
import torch.nn.functional as F

import torch_geometric
from torch_geometric.data import Data
from torch_geometric.nn import GCNConv, GATConv, GatedGraphConv, VGAE

from random import randint
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
from sklearn.linear_model import LogisticRegression


class GCNEncoder(torch.nn.Module):
    def __init__(self, input_dim, hidden_dim, output_dim):
        super(GCNEncoder, self).__init__()
        self.mp1 = GCNConv(input_dim, hidden_dim)
        self.mp2 = GCNConv(hidden_dim, output_dim)
        # self.fc = torch.nn.Linear(hidden_dim, output_dim)
        self.dropout = torch.nn.Dropout(0.0)
        self.relu = torch.nn.ReLU()

    def forward(self, x, edge_index):
        h1 = self.relu(self.mp1(x, edge_index))
        h1 = self.dropout(h1)
        h2 = self.mp2(h1, edge_index)

        return h2


class VariationalGCNEncoder(torch.nn.Module):
    def __init__(self, in_channels, out_channels):
        super(VariationalGCNEncoder, self).__init__()
        self.conv1 = GCNConv(in_channels, 2 * out_channels)
        self.conv_mu = GCNConv(2 * out_channels, out_channels)
        self.conv_logstd = GCNConv(2 * out_channels, out_channels)

    def forward(self, x, edge_index):
        x = self.conv1(x, edge_index).relu()
        return self.conv_mu(x, edge_index), self.conv_logstd(x, edge_index)


def loss_func(z, edge_index):
    y = list()
    y_pred = list()

    y.append(torch.ones(edge_index.size(1)).to(device))
    y_pred.append(torch.sum(torch.mul(z[edge_index[0, :], :], z[edge_index[1, :]]), dim=1))

    rand_indices = torch.randint(z.size(0), edge_index.size())
    y.append(torch.zeros(edge_index.size(1)).to(device))
    y_pred.append(torch.sum(torch.mul(z[rand_indices[0, :], :], z[rand_indices[1, :], :]), dim=1))

    y = torch.cat(y, dim=0)
    y_pred = torch.cat(y_pred, dim=0)

    loss = F.binary_cross_entropy_with_logits(y_pred, y)
    loss = loss + (1 / data.num_nodes) * model.kl_loss()  # new line

    return loss


G = nx.read_edgelist('edgelist.txt', delimiter=',', create_using=nx.Graph(), nodetype=int)
nodes = list(G.nodes())
edges = list(G.edges())
n = G.number_of_nodes()
m = G.number_of_edges()
print('Number of nodes:', n)
print('Number of edges:', m)

edge_index = np.zeros((2, 2 * m))
for i, edge in enumerate(edges):
    edge_index[0, 2 * i] = edge[0]
    edge_index[1, 2 * i] = edge[1]

    edge_index[0, 2 * i + 1] = edge[1]
    edge_index[1, 2 * i + 1] = edge[0]

edge_index = torch.tensor(edge_index, dtype=torch.long)

# Read the abstract of each paper
# abstracts = dict()
# with open('abstracts.txt', 'r') as f:
#     for line in f:
#         node, abstract = line.split('|--|')
#         abstracts[int(node)] = abstract
#
# # Map text to list of terms
# for node in abstracts:
#     abstracts[node] = abstracts[node].split()
#
# documents = [TaggedDocument(doc, [node]) for node, doc in abstracts.items()]
# model = Doc2Vec(documents, vector_size=32, window=10, dm=0, hs=1, min_count=20, workers=4)

# Create a matrix x that will contain the embeddings of all abstracts (one per row)
# You can obtain the embedding of the abstact of a node using: model.docvecs[node]

# your code here
x = np.load('doc_vecs.npy')

x = torch.tensor(x, dtype=torch.float)

data = Data(x=x, edge_index=edge_index)
print(data)

output_dim = 16
input_dim = 32
hidden_dim = 32
# model = GCNEncoder(input_dim, hidden_dim, output_dim)
model = VGAE(VariationalGCNEncoder(input_dim, output_dim))  # new line
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

model = model.to(device)
x = data.x.to(device)
edge_index = data.edge_index.to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=0.01)


def train():
    model.train()
    optimizer.zero_grad()
    z = model.encode(x, edge_index)
    loss = loss_func(z, edge_index)
    loss.backward()
    optimizer.step()
    return float(loss)


def test():
    model.eval()
    with torch.no_grad():
        z = model.encode(x, edge_index)
    return z


for epoch in range(1, 301):
    loss = train()
    if epoch % 50 == 0:
        print('Epoch: {:03d}, Loss: {:.4f}'.format(epoch, loss))

z = test()
z = z.detach().cpu().numpy()
np.save("vGAE.npy", z)
