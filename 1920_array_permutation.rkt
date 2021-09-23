(define (build-array nums)
   (map (lambda (x) (list-ref nums x)) nums))
