#lang typed/racket

;; display and handle the game for a local human player competing against an "AI"
;; run game between 2 "AI"s 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/state)
(require PillWars/World/typed/handlers-for-local-nav)
(require PillWars/AI/typed/strategy-1)
(require PillWars/World/typed/constants)
(require typed/2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
(: main/AI {->* (String) [State] [Listof String]})
;; a turn-based game betweeen a human player and an AI; the optional state can make it deterministic 
(define (main/AI my-name [state0 #false])
  (define start-with (or state0 (create-state (~a my-name))))
  (define end-with
    (big-bang (create-interactive start-with) : Interactive 
      [on-tick   (enable AI #;disable: HUMAN (ai-strategy strategy-1))]
      [to-draw   (strip (draw-state BG))]
      [on-mouse  (enable HUMAN #;disable: AI act-on-button-down {x : Integer} {y : Integer} {me : String})]
      [on-key    (enable HUMAN #;disable: AI navigate-by-key {ke : String})]
      [name      (~a my-name)]
      [stop-when (strip game-over?) (strip (draw-state-with-winners BG))]))
  (interactive-winners end-with))

;; the next one is added for head-less profiling; derived from the above w/ attempt to make it similar
(: 2AIs {->* () [Interactive] [Listof String]})
(define (2AIs [interactive0 #false])
  (define state++    (create-state (~a AI2)))
  (define start-with (or interactive0 (create-interactive state++)))
  (define end-with
    (big-bang/nodraw start-with : Interactive 
                     [on-tick       (enable AI #;disable: AI2 (ai-strategy strategy-1))]
                     [on-tick-other (enable AI2 #;disable: AI (ai-strategy strategy-1))]
                     [stop-when     (strip game-over?)]))
  (set! *start-interactive start-with)
  (interactive-winners end-with))

(define-syntax-rule (big-bang/nodraw state0 : T [on-tick th] [on-other-tick th-other] [stop-when sw?])
  (let loop : T ([state : T state0] [handle : (Pairof (T -> T) (T -> T))  (cons th th-other)])
    (cond
      [(sw? state) state]
      [else (match-define (cons f h) handle)
            (define state++ (f state))
            (loop state++ (cons h f))])))
    
;; ---------------------------------------------------------------------------------------------------
;; making a `big-bang` world a two-player game, with one of them an AI
;; ASSUME the AI can't make the mistake of dropping out
;; CONSEQUENCE if the Human player drops out, we notice because the AI player is first. 

(struct interactive [{whose-turn : Symbol} {state : State}] #:prefab #:type-name Interactive)
#; {type Interactive = [interactive Tag State]}
#; {type Tag         = .. gensymed symbol .. }
;; INVARIANT `whose-turn` and the first player in `state` must be in sync
#; {type [Handler X] = (State Any ... -> X)}

(define AI (gensym "AI"))
(define AI2 (gensym "AI"))
(define HUMAN (gensym "HUMAN"))

(: create-interactive {State -> Interactive})
;; add an AI player and set it up to go first 
(define (create-interactive state)
  (interactive AI (add-fighter-to-front (~a AI) state)))

(require (for-syntax syntax/parse))
; (: enable {-> Symbol Symbol [Handler State State] (-> Interactive Any * Interactive)})
(define-syntax (enable stx)
  (syntax-parse stx
    [(_ tag other handler {rem (~literal :) Rem} ...)
     #'(λ ({i : Interactive} {rem : Rem} ...)
         (match-define [interactive whose state0] i)
         (cond
           [(only-one-player state0)
            (define state++ (handler state0 rem ...))
            (interactive AI (cast state++ State))]
           [(equal? whose other) i]
           [else
            (define state++ (handler state0 rem ...))
            (if (boolean? state++)
                i
                (interactive other (swap-two-players state++)))]))]))

(: only-one-player {State -> Boolean})
(define (only-one-player state0)
  (define players (state-fighters state0))
  (and (cons? players) (empty? (rest players))))

(: strip (∀ (X) {-> (State -> X) (-> Interactive X)}))
(define [(strip handler) i]
  (match-define [interactive whose state] i)
  (handler state))

(: interactive-winners {Interactive -> [Listof String]})
(define (interactive-winners i)
  (match-define [interactive whose state] i)
  (winners state))

;; ---------------------------------------------------------------------------------------------------
(define *start-interactive (create-interactive (plain-state)))

(module+ seed ;; run only once; re-run if ,,/Resources/n-test-{in|out}pyt.rktd are lost
  (require PillWars/typed/test-aux)

  (: create-and-save-tests (-> Natural Void))
  (define (create-and-save-tests n)
    (for ([tst n])
      (define result [2AIs])
      (write-test-pair tst *start-interactive result)))
  
  (: write-test-pair (-> Natural Interactive [Listof String] Void))
  (define (write-test-pair tst input expected)
    (with-output-to-file (file-name tst in) (λ () (write input)) #:exists 'replace)
    (with-output-to-file (file-name tst out) (λ () (write expected)) #:exists 'replace))
    
  (time (create-and-save-tests 10)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (provide run)
  
  (require typed/rackunit)

  (: run (-> [Listof Interactive] [Listof [Listof String]] (-> Void)))
  (define (run input* expect*)
    (lambda ()
      (for ([i input*] [e expect*])
        (check-equal? [2AIs i] e)))))

;; ---------------------------------------------------------------------------------------------------
(module+ deep
  (provide deep-cast-interactive)

  (require (submod PillWars/Common/typed/state deep))
  
  (: deep-cast-interactive (-> Any Interactive))
  (define (deep-cast-interactive i)
    (match-define [interactive tag s] (cast i Interactive))
    (interactive tag (deep-cast-state s))))

(module+ examples-deep
  (provide input* expect*)

  (require (submod ".." deep))
  (require PillWars/typed/test-aux)
  (define PREFIX "../")
  
  (: read-from (-> String Any))
  (define (read-from fname)
    (with-input-from-file (~a PREFIX dir fname) read))
  
  (define input*  (map (λ ({i : String}) (deep-cast-interactive (read-from i))) (get-files in)))
  (define expect* (map (λ ({e : String}) (cast (read-from e) [Listof String])) (get-files out))))

(module+ perf-deep
  (require (submod ".." examples-deep))
  (require (submod ".." test))
  (time [(run input* expect*)]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples-io

  (provide input* expect*)

  (require PillWars/typed/test-aux)
  (define PREFIX "../")
  
  (: read-from (-> String Any))
  (define (read-from fname)
    (with-input-from-file (~a PREFIX dir fname) read))
  
  (define input*  (map (λ ({i : String}) (cast (read-from i) Interactive)) (get-files in)))
  (define expect* (map (λ ({e : String}) (cast (read-from e) [Listof String])) (get-files out))))

(module+ perf-io
  (require (submod ".." examples-io))
  (require (submod ".." test))
  (time [(run input* expect*)]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples-local

  (provide input* expect*)
  
  (define i0 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 168+327i 7.0+10.0i 0 "AI113424" default-image) #s(fighter 356+117i 8.0+6.0i 0 "AI113425" default-image)) (#s((red pill 2) 761+1i 5 2/5) #s((red pill 2) 344+240i 3 1/10) #s((red pill 2) 226+488i 3 1/5) #s((red pill 2) 474+277i 1 3/10) #s((red pill 2) 555+41i 5 2/5) #s((red pill 2) 983+303i 1 3/10) #s((red pill 2) 560+708i 2 1/2) #s((red pill 2) 204+264i 3 3/10) #s((red pill 2) 1175+773i 2 3/10) #s((red pill 2) 46+374i 4 1/2) #s((red pill 2) 1+1i 5 2/5) #s((red pill 2) 114+227i 4 1/5) #s((red pill 2) 254+625i 5 1/5) #s((red pill 2) 114+501i 4 1/10) #s((red pill 2) 301+241i 2 1/10) #s((red pill 2) 1070+128i 3 3/10) #s((red pill 2) 846+786i 5 3/10) #s((red pill 2) 959+525i 1 1/10) #s((red pill 2) 745+643i 5 1/5) #s((red pill 2) 790+592i 5 1/2) #s((blue pill 2) 710+445i 13) #s((blue pill 2) 27+645i 9) #s((blue pill 2) 1140+481i 8) #s((blue pill 2) 1171+268i 14) #s((blue pill 2) 1106+252i 9) #s((blue pill 2) 540+615i 11) #s((blue pill 2) 954+390i 10) #s((blue pill 2) 973+123i 11) #s((blue pill 2) 1060+597i 7) #s((blue pill 2) 688+574i 6)))))
  (define e0 '("AI113424: 99" "AI113425: 23"))

  (define i1 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 396+216i 10.0+2.0i 0 "AI113424" default-image) #s(fighter 28+572i 7.0+1.0i 0 "AI113425" default-image)) (#s((red pill 2) 58+300i 5 3/10) #s((red pill 2) 153+360i 4 1/10) #s((red pill 2) 833+10i 4 2/5) #s((red pill 2) 1082+747i 3 1/5) #s((red pill 2) 702+526i 1 2/5) #s((red pill 2) 583+251i 1 1/10) #s((red pill 2) 833+187i 5 1/2) #s((red pill 2) 512+650i 2 2/5) #s((red pill 2) 222+763i 4 1/5) #s((red pill 2) 494+256i 3 2/5) #s((red pill 2) 215+296i 4 2/5) #s((red pill 2) 648+662i 5 1/10) #s((red pill 2) 959+586i 4 3/10) #s((red pill 2) 719+313i 5 1/5) #s((red pill 2) 665+548i 5 1/5) #s((red pill 2) 285+305i 5 1/2) #s((red pill 2) 965+130i 4 3/10) #s((red pill 2) 299+53i 3 1/2) #s((red pill 2) 250+773i 1 3/10) #s((red pill 2) 119+74i 1 3/10) #s((blue pill 2) 578+675i 8) #s((blue pill 2) 146+716i 7) #s((blue pill 2) 680+537i 11) #s((blue pill 2) 93+284i 9) #s((blue pill 2) 1107+571i 8) #s((blue pill 2) 263+406i 9) #s((blue pill 2) 742+504i 10) #s((blue pill 2) 30+778i 13) #s((blue pill 2) 875+335i 8) #s((blue pill 2) 1163+416i 10)))))
  (define e1 '("AI113424: 97" "AI113425: 26"))

  (define i2 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 997+737i 6.0+7.0i 0 "AI113424" default-image) #s(fighter 1024+32i 2.0+6.0i 0 "AI113425" default-image)) (#s((red pill 2) 370+331i 4 3/10) #s((red pill 2) 160+250i 1 1/5) #s((red pill 2) 437+152i 3 3/10) #s((red pill 2) 647+362i 4 1/5) #s((red pill 2) 128+379i 3 2/5) #s((red pill 2) 884+264i 3 1/5) #s((red pill 2) 941+172i 2 1/10) #s((red pill 2) 903+701i 1 1/10) #s((red pill 2) 573+782i 1 1/5) #s((red pill 2) 641+791i 1 2/5) #s((red pill 2) 1171+592i 4 2/5) #s((red pill 2) 245+82i 4 1/10) #s((red pill 2) 936+290i 1 2/5) #s((red pill 2) 915+6i 2 3/10) #s((red pill 2) 436+319i 4 1/5) #s((red pill 2) 490+446i 3 2/5) #s((red pill 2) 403+756i 4 2/5) #s((red pill 2) 1049+624i 5 1/10) #s((red pill 2) 455+761i 4 3/10) #s((red pill 2) 970+36i 2 2/5) #s((blue pill 2) 998+745i 14) #s((blue pill 2) 907+154i 11) #s((blue pill 2) 583+48i 5) #s((blue pill 2) 121+189i 9) #s((blue pill 2) 229+139i 5) #s((blue pill 2) 1023+383i 8) #s((blue pill 2) 214+59i 8) #s((blue pill 2) 602+232i 12) #s((blue pill 2) 907+658i 5) #s((blue pill 2) 2+353i 13)))))
  (define e2 '("AI113424: 109" "AI113425: 14"))

  (define i3 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 763+239i 5.0+10.0i 0 "AI113424" default-image) #s(fighter 445+314i 4.0+9.0i 0 "AI113425" default-image)) (#s((red pill 2) 1074+694i 4 3/10) #s((red pill 2) 994+19i 3 1/5) #s((red pill 2) 140+471i 1 3/10) #s((red pill 2) 1032+140i 4 3/10) #s((red pill 2) 981+77i 2 1/5) #s((red pill 2) 1096+700i 3 2/5) #s((red pill 2) 57+511i 5 1/2) #s((red pill 2) 318+47i 1 1/5) #s((red pill 2) 809+83i 5 1/5) #s((red pill 2) 230+19i 4 1/2) #s((red pill 2) 230+225i 5 1/10) #s((red pill 2) 907+288i 4 1/10) #s((red pill 2) 590+359i 1 1/10) #s((red pill 2) 456+547i 1 1/5) #s((red pill 2) 842+787i 5 1/2) #s((red pill 2) 173+398i 5 3/10) #s((red pill 2) 180+638i 4 1/10) #s((red pill 2) 294+593i 4 1/5) #s((red pill 2) 220+624i 4 1/10) #s((red pill 2) 934+192i 2 1/2) #s((blue pill 2) 955+787i 10) #s((blue pill 2) 498+556i 13) #s((blue pill 2) 761+536i 6) #s((blue pill 2) 321+282i 12) #s((blue pill 2) 572+453i 14) #s((blue pill 2) 315+542i 5) #s((blue pill 2) 468+11i 8) #s((blue pill 2) 491+138i 11) #s((blue pill 2) 74+231i 13) #s((blue pill 2) 320+655i 7)))))
  (define e3 '("AI113424: 109" "AI113425: 57"))

  (define i4 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 226+794i 10.0+8.0i 0 "AI113424" default-image) #s(fighter 671+164i 10.0+5.0i 0 "AI113425" default-image)) (#s((red pill 2) 798+558i 2 2/5) #s((red pill 2) 1166+642i 3 1/10) #s((red pill 2) 19+550i 4 1/5) #s((red pill 2) 271+758i 5 1/10) #s((red pill 2) 132+614i 3 1/5) #s((red pill 2) 311+19i 4 2/5) #s((red pill 2) 623+519i 4 3/10) #s((red pill 2) 977+732i 4 1/5) #s((red pill 2) 1068+103i 5 1/2) #s((red pill 2) 268+296i 5 1/5) #s((red pill 2) 639+756i 3 3/10) #s((red pill 2) 1027+415i 5 1/5) #s((red pill 2) 887+563i 4 1/5) #s((red pill 2) 586+398i 5 1/2) #s((red pill 2) 892+654i 4 1/10) #s((red pill 2) 818+491i 4 3/10) #s((red pill 2) 1037+530i 3 1/5) #s((red pill 2) 924+635i 5 1/2) #s((red pill 2) 582+606i 2 1/2) #s((red pill 2) 918+95i 3 1/5) #s((blue pill 2) 833+301i 9) #s((blue pill 2) 435+317i 13) #s((blue pill 2) 256+475i 8) #s((blue pill 2) 898+36i 6) #s((blue pill 2) 1053+711i 12) #s((blue pill 2) 417+173i 9) #s((blue pill 2) 678+619i 10) #s((blue pill 2) 698+276i 11) #s((blue pill 2) 15+615i 8) #s((blue pill 2) 276+385i 8)))))
  (define e4 '("AI113424: 115" "AI113425: 43"))

  (define i5 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 54+11i 6.0+5.0i 0 "AI113424" default-image) #s(fighter 22+157i 4.0+5.0i 0 "AI113425" default-image)) (#s((red pill 2) 796+606i 3 1/5) #s((red pill 2) 71+608i 2 1/10) #s((red pill 2) 1142+141i 5 1/2) #s((red pill 2) 855+531i 1 2/5) #s((red pill 2) 612+445i 4 1/10) #s((red pill 2) 1005+420i 3 1/10) #s((red pill 2) 633+56i 5 1/10) #s((red pill 2) 359+514i 5 1/10) #s((red pill 2) 942+307i 2 1/5) #s((red pill 2) 601+369i 2 3/10) #s((red pill 2) 834+37i 5 1/10) #s((red pill 2) 953+414i 5 1/10) #s((red pill 2) 406+783i 2 3/10) #s((red pill 2) 1129+267i 3 3/10) #s((red pill 2) 859+554i 2 1/10) #s((red pill 2) 545+761i 2 1/5) #s((red pill 2) 240+65i 3 2/5) #s((red pill 2) 1055+154i 2 3/10) #s((red pill 2) 250+293i 4 3/10) #s((red pill 2) 894+43i 2 2/5) #s((blue pill 2) 12+303i 6) #s((blue pill 2) 154+602i 9) #s((blue pill 2) 246+719i 8) #s((blue pill 2) 786+123i 12) #s((blue pill 2) 741+436i 12) #s((blue pill 2) 346+568i 9) #s((blue pill 2) 1034+369i 9) #s((blue pill 2) 770+186i 5) #s((blue pill 2) 233+197i 5) #s((blue pill 2) 1123+627i 12)))))
  (define e5 '("AI113425: 80" "AI113424: 52"))

  (define i6 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 124+662i 7.0+1.0i 0 "AI113424" default-image) #s(fighter 590+433i 1.0+4.0i 0 "AI113425" default-image)) (#s((red pill 2) 1002+118i 4 3/10) #s((red pill 2) 595+641i 1 2/5) #s((red pill 2) 1153+510i 4 1/2) #s((red pill 2) 1098+239i 1 1/2) #s((red pill 2) 299+177i 1 1/10) #s((red pill 2) 129+678i 3 1/5) #s((red pill 2) 720+83i 4 3/10) #s((red pill 2) 507+649i 5 1/5) #s((red pill 2) 739+503i 2 1/2) #s((red pill 2) 824+244i 3 3/10) #s((red pill 2) 243+661i 5 1/10) #s((red pill 2) 371+768i 3 3/10) #s((red pill 2) 908+278i 2 3/10) #s((red pill 2) 508+76i 1 2/5) #s((red pill 2) 1027+373i 2 3/10) #s((red pill 2) 236+673i 3 1/10) #s((red pill 2) 975+55i 4 1/5) #s((red pill 2) 1103+80i 3 3/10) #s((red pill 2) 1043+475i 3 3/10) #s((red pill 2) 724+122i 3 3/10) #s((blue pill 2) 661+156i 7) #s((blue pill 2) 672+783i 11) #s((blue pill 2) 333+492i 5) #s((blue pill 2) 753+290i 5) #s((blue pill 2) 400+329i 8) #s((blue pill 2) 761+662i 11) #s((blue pill 2) 75+121i 11) #s((blue pill 2) 142+253i 5) #s((blue pill 2) 279+474i 14) #s((blue pill 2) 786+699i 6)))))
  (define e6 '("AI113424: 107" "AI113425: 25"))

  (define i7 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 296+108i 4.0+9.0i 0 "AI113424" default-image) #s(fighter 1130+490i 7.0+8.0i 0 "AI113425" default-image)) (#s((red pill 2) 607+750i 4 3/10) #s((red pill 2) 615+574i 1 3/10) #s((red pill 2) 755+681i 2 1/5) #s((red pill 2) 23+592i 1 1/5) #s((red pill 2) 409+569i 5 1/5) #s((red pill 2) 534+650i 3 1/10) #s((red pill 2) 130+267i 4 1/5) #s((red pill 2) 33+456i 1 1/10) #s((red pill 2) 348+492i 2 1/2) #s((red pill 2) 70+187i 5 1/2) #s((red pill 2) 778+31i 1 1/10) #s((red pill 2) 386+358i 4 1/2) #s((red pill 2) 725+593i 4 1/5) #s((red pill 2) 137+245i 5 3/10) #s((red pill 2) 80+557i 5 1/10) #s((red pill 2) 928+743i 4 2/5) #s((red pill 2) 866+415i 1 3/10) #s((red pill 2) 360+528i 5 3/10) #s((red pill 2) 805+221i 4 3/10) #s((red pill 2) 844+272i 2 3/10) #s((blue pill 2) 767+740i 12) #s((blue pill 2) 894+165i 8) #s((blue pill 2) 880+15i 8) #s((blue pill 2) 677+136i 6) #s((blue pill 2) 167+59i 7) #s((blue pill 2) 1077+95i 6) #s((blue pill 2) 1090+325i 13) #s((blue pill 2) 709+72i 5) #s((blue pill 2) 775+620i 11) #s((blue pill 2) 924+504i 6)))))
  (define e7 '("AI113424: 83" "AI113425: 59"))
  
  (define i8 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 192+317i 8.0+4.0i 0 "AI113424" default-image) #s(fighter 171+712i 2.0+4.0i 0 "AI113425" default-image)) (#s((red pill 2) 100+446i 5 3/10) #s((red pill 2) 276+789i 3 2/5) #s((red pill 2) 771+491i 5 1/5) #s((red pill 2) 1129+60i 3 1/5) #s((red pill 2) 1169+792i 5 3/10) #s((red pill 2) 310+528i 1 2/5) #s((red pill 2) 1155+133i 5 1/2) #s((red pill 2) 186+462i 5 3/10) #s((red pill 2) 199+423i 4 1/10) #s((red pill 2) 149+713i 4 1/2) #s((red pill 2) 396+342i 2 1/10) #s((red pill 2) 371+418i 5 2/5) #s((red pill 2) 221+548i 4 2/5) #s((red pill 2) 888+387i 3 1/2) #s((red pill 2) 480+675i 1 3/10) #s((red pill 2) 154+351i 5 3/10) #s((red pill 2) 514+426i 4 1/5) #s((red pill 2) 117+470i 1 1/10) #s((red pill 2) 781+457i 1 3/10) #s((red pill 2) 1124+697i 1 1/5) #s((blue pill 2) 586+561i 12) #s((blue pill 2) 738+313i 8) #s((blue pill 2) 393+66i 13) #s((blue pill 2) 393+282i 12) #s((blue pill 2) 526+753i 12) #s((blue pill 2) 384+529i 9) #s((blue pill 2) 82+483i 8) #s((blue pill 2) 629+628i 9) #s((blue pill 2) 868+23i 8) #s((blue pill 2) 617+314i 14)))))
  (define e8 '("AI113424: 121" "AI113425: 51"))

  (define i9 : Interactive
    #s(interactive AI113424 #s(state (#s(fighter 402+193i 1.0+6.0i 0 "AI113424" default-image) #s(fighter 113+799i 4.0+9.0i 0 "AI113425" default-image)) (#s((red pill 2) 1009+25i 5 3/10) #s((red pill 2) 813+397i 1 1/10) #s((red pill 2) 859+659i 1 1/10) #s((red pill 2) 213+272i 4 2/5) #s((red pill 2) 589+198i 5 1/5) #s((red pill 2) 706+216i 5 2/5) #s((red pill 2) 280+244i 2 2/5) #s((red pill 2) 998+31i 4 3/10) #s((red pill 2) 289+330i 5 3/10) #s((red pill 2) 540+790i 4 1/5) #s((red pill 2) 714+383i 2 1/10) #s((red pill 2) 434+736i 4 1/2) #s((red pill 2) 821+578i 5 1/2) #s((red pill 2) 1001+223i 1 1/2) #s((red pill 2) 1166+245i 3 1/2) #s((red pill 2) 413+737i 3 1/2) #s((red pill 2) 1135+126i 2 1/10) #s((red pill 2) 430+768i 4 3/10) #s((red pill 2) 1006+726i 1 1/10) #s((red pill 2) 277+558i 2 3/10) #s((blue pill 2) 200+389i 11) #s((blue pill 2) 492+66i 10) #s((blue pill 2) 822+632i 12) #s((blue pill 2) 902+224i 14) #s((blue pill 2) 438+654i 9) #s((blue pill 2) 953+776i 14) #s((blue pill 2) 482+38i 12) #s((blue pill 2) 513+145i 5) #s((blue pill 2) 646+615i 11) #s((blue pill 2) 1040+515i 8)))))
  (define e9 '("AI113425: 89" "AI113424: 80"))

  (define input*  (list i0 i1 i2 i3 i4 i5 i6 i7 i8 i9))
  (define expect* (list e0 e1 e2 e3 e4 e5 e6 e7 e8 e9)))

(module+ perf-local
  (require (submod ".." examples-local))
  (require (submod ".." test))
  (time [(run input* expect*)]))