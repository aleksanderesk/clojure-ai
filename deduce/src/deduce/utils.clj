(ns deduce.utils)

(defn ormap
  "Takes a function producing either true or false and a list of elements to
  apply the function to.
  Returns true if any of the results of the function applicatio are true,
  else returns nil."
  [func l]
  (some true? (map func l)))
