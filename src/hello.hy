(with [f (open "/data/hello.txt")]
  (-> f
    .read
    .strip
    print ) )
