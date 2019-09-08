tree_preorder(nil) --> [].
tree_preorder(node(Left, Name, Right)) -->
    [Name],
    tree_preorder(Left),
    tree_preorder(Right).

tree_inorder(nil) --> [].
tree_inorder(node(Left, Name, Right)) -->
    tree_inorder(Left),
    [Name],
    tree_inorder(Right).
