
"user-home:darcs;asdf-system-connections;website;source;index.lml"
"user-home:darcs;cl-graph;website;source;index.lml"
"user-home:darcs;cl-containers;website;source;index.lml"
"user-home:darcs;cl-mathstats;website;source;index.lml"
"user-home:darcs;cl-variates;website;source;index.lml"
"user-home:darcs;metabang.bind;website;source;index.lml"
"user-home:darcs;metatilities;website;source;index.lml"
"user-home:darcs;moptilities;website;source;index.lml"
"user-home:darcs;tinaa;website;source;index.lml"




Add list of shared stuff (per system?) and use the shared version unless
  a project version exists

(with-new-file
  (s "Billy-Pilgrim:Users:gwking:darcs:metabang-site:website:source:about.lml
")
  (format s "~S"
          (net.html.parser::parse-html
           #P"Billy-Pilgrim:Users:gwking:darcs:metabang-site:website:source:about.html")))


ok - Prevent updates when source hasn't changed...
