(def fn
  (list 'mac (list 'clo [args body]
                   (list 'clo scope (list 'quote args)
                                    (list 'quote body)))))
(def defn
  (list 'mac (list 'clo [sym args body]
                   (list 'def sym
                         (list 'fn args body)))))

