# Sorting leaf nodes in a tree

[![Build Status](https://travis-ci.org/kmusienko/tree-sort-leaf-nodes.svg?branch=master)](https://travis-ci.org/kmusienko/tree-sort-leaf-nodes) 

This is an implementation of an algorithm that sorts leaf nodes in a tree and moves **extra leaf nodes**
from each node to the next node. If **extra leaf nodes** are present in the last node, they are deleted.

Definition of the **extra leaf nodes**:

If we start iterating over the leaf nodes of a some particular node and count sum of their values,
**extra leaf nodes** will be a right part of a leaf nodes starting from a node when the sum begins being more
than given constant **W**.