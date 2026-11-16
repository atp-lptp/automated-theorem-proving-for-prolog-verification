% tree/1 and forest/1 are mutually recursive: a tree is a node with a forest,
% a forest is a list of trees.
tree(node(X,F)) :- forest(F).

forest([]).
forest([T|F]) :- tree(T), forest(F).
