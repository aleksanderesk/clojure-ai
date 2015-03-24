(ns deduce.utils)

(defn merge-quit
  "As special type of dictionary merge that can return false if one of its
  arguments is false"
  [m r]
  (if (and m r)
    (merge m r)
    false))
