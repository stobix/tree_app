{application, tree_app,
    [{description, "A little app to handle gb_trees."},
     {vsn,"0.9"},
     {modules,[tree_app,tree_srv,tree_sup]},
     {registered, [tree_srv]},
     {applications, [kernel,stdlib]},
     {mod, {tree_app, []}},
     {env, [ ]}
 ]}.
