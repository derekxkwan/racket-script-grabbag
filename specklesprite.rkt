#lang racket

(require racket/draw)

(define cur-num-frames 5)
(define cur-num-colors 3)
(define cur-sq-dim 24)
(define cur-row-sz 4)
(define out-name "out.png")

 
(define (make-colors num-colors)
  (let ([color-gen
	 (lambda ()
           (apply (lambda (x y z)
		    (make-object color% x y z))
		  (map (lambda (a) (random 255)) (range 3))
		  ))])
    (map (lambda (b) (color-gen)) (range num-colors))
    )
  )


(define (get-topleft cur-idx num-rows cur-sz)
  (let ([cur-row (modulo cur-idx num-rows)]
	[cur-col (floor (/ cur-idx num-rows))]
	)
    (map (lambda (x) (* cur-sz x)) (list cur-row cur-col))
    )
  )

(define (generate-sprites num-frames num-colors cur-outname sq-dim num-rows)
  (let* ([subsq-sz (/ sq-dim num-rows)]
	[target (make-bitmap (* sq-dim num-frames) sq-dim)]
	[cur-dc (new bitmap-dc% [bitmap target])]
	[cur-colors (make-colors num-colors)]
	[cur-pens (map (lambda (x) (new pen% [color x])) cur-colors)]
	[cur-brushes (map (lambda (x) (new brush% [color x]))
			  cur-colors)]
	)
    (for ([cur-frame (range num-frames)])
      (for ([cur-idx (range (* num-rows num-rows))])
	(let* ([cur-x-offset (* cur-frame sq-dim)]
	       [cur-sel (random num-colors)]
	       [cur-brush (list-ref cur-brushes cur-sel)]
	       [cur-pen (list-ref cur-pens cur-sel)]
	       [cur-topleft (get-topleft cur-idx num-rows subsq-sz)]
	       )
	  (send cur-dc set-pen cur-pen)
	  (send cur-dc set-brush cur-brush)
	  (send cur-dc draw-rectangle
		(+ cur-x-offset (list-ref cur-topleft 0))
		(list-ref cur-topleft 1)
		subsq-sz
		subsq-sz
		)
	  )
	)
      )
    (send target save-file cur-outname 'png)
    )
  )




(define (batch-gen num-iter)
  (let ([cur-outs (map (lambda (x) (string-append "out-" (number->string x) ".png"))
		       (range num-iter))])
    (map (lambda (x)
	 (generate-sprites cur-num-frames cur-num-colors x cur-sq-dim cur-row-sz)
	 ) cur-outs)
    )
  )

(let ([cur-num ((compose string->number vector-ref) (current-command-line-arguments) 0)])
  (batch-gen cur-num))
