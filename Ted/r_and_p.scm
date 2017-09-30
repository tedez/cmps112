(let ((p (open-input-file "00-hello-world.sbir")))
  (let f ((x (read p)))
    (if (eof-object? x)
        (begin
          (close-input-port p)
          '())
        (cons x (f (read p))))))
