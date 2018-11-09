#lang racket

;import required packages
(require data-science-master)
(require plot)
(require math)
(require json)
(require srfi/19)
(require racket/stream)
(require racket/system)




;;START THE PROGRAM WITH PROMPTING THE USER FOR COUNTRY CODE
(display "Enter country code eg UK")
(newline)
(define countrycode (read))
(display (string-append "Loading Tweets..."))


;DEFINE THE REFQUIRED FUNCTOIONS BEFORE ACTUALLY FETCHING TWEETS

;Function to reed tweets into an array
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

;Function to remove URLs, remove punctuation, and remove spaces from each tweet.
(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x))) #:websafe? #t))
       lst))

;Fetch the tweets
(define tweets (string->jsexpr
                ;OPTION 1 - ORIGINAL Json Example
                (with-input-from-file "trump_tweets.json" (λ () (json-lines->json-array))) ;This loads the orginal trump tweets

                ;OPTION 2 - loading hardcoded UK value
                ;(with-output-to-string (lambda ()(system "twurl /1.1/tweets/search/30day/dev.json?query=DailyMonitor search api&place_country=UK&fromDate=201710090000&toDate=201711090000""))); This load user required tweets

                ;OPTION 3 - loading user value
                ;(define countrycode (read))
                ;(with-output-to-string (lambda ()(system (string-append "twurl /1.1/tweets/search/30day/dev.json?query=DailyMonitor search api&place_country=" countrycode "&fromDate=201710090000&toDate=201711090000"")))); This load user required tweets


                ))
;Display tweets in console to be sure they loaoded. This step may be skipped
(display (jsexpr->string tweets))

;Remove unnecessary details, keep jut=st tweets and source
(define t
  (let ([tmp (map (λ (x) (list (hash-ref x 'text))) tweets)]) ;; improve to use streams
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)

    ))

;organize the tweets
(define joined-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 t "")) )


;START ANALYSIS following previous assignment
(define words (document->tokens joined-tweets #:sort? #t))

(define sentiment (list->sentiment words #:lexicon 'nrc))

(take sentiment 5)

(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))