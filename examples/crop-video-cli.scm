(define (help)
  (displayNl)
  (displayNl "crop-video-cli.scm: A thin, overengineered wrapper around ffmpeg to crop videos. Just a demo for the language features.")
  (displayNl "usage: aether run examples/crop-video-cli.scm -- -i ./input -o ./output --start 00:00:05 --end 00:02:30")
  (displayNl)
  (displayNl "options:")
  (displayNl "  -i <input-file-path>     Path to input video file")
  (displayNl "  -o <output-file-path>    Path to output video file (will get overwritten if it exists)")
  (displayNl "  --start <time>           Start cropping at specified time (defaults to start of video)")
  (displayNl "  --end <time>             Stop cropping at specified time (defaults to end of video)"))

(record Config
  .in-file
  .out-file
  .start-time
  .end-time)

(define (crop-video config)
  (set result (! ffmpeg -y
     -i ,(.in-file config)
     ,@(get-time-args config)
     -c:v copy
     ,(.out-file config)))
  
  (displayNl result))

(define (get-time-args config)
  (define start-time
    (if (nil? (.start-time config))
      "00:00:00"
      (.start-time config)))

  (define end-time-args
    (if (nil? (.end-time config))
      #nil
      (list "-to" (.end-time config))))

  '(-ss ,start-time ,@end-time-args))

(define help-args '("-h" "--help" "help"))
(define (parse-args args cfg)
  (cond
    [ (empty? args)                      cfg ]
    [ (contains? (car args) help-args)   (progn (help) #nil) ]
    [ (= "--start" (car args))           (parse-args (drop 2 args) (set@.start-time (cadr args) cfg)) ]
    [ (= "--end" (car args))             (parse-args (drop 2 args) (set@.end-time (cadr args) cfg)) ]
    [ (= "-i" (car args))                (parse-args (drop 2 args) (set@.in-file (cadr args) cfg)) ]
    [ (= "-o" (car args))                (parse-args (drop 2 args) (set@.out-file (cadr args) cfg)) ]
    [ else                               (error! (quote 'invalid-arguments) (string "Invalid argument: " (car args))) ]))

(define args (get-args))

(define (main)
  (define config (parse-args args (Config #nil #nil #nil #nil)))

  (unless (nil? config)
    (when (|| (nil? (.in-file config)) (nil? (.out-file config)))
      (error! (quote 'invalid-arguments) "You need to specify both in/out file paths with -i and -o flags"))

    (displayNl "Cropping " (.in-file config) " into " (.out-file config) "...")
    (crop-video config)
    (displayNl "done")))

(expand [error] (try (main)))
(unless (nil? error)
  (help)
  (displayNl)
  (displayNl (:error/message error)))

