; scql (c) 2019-2025 Alex Semotan
; scql is released under the MIT license.
(use extras
     srfi-1
     srfi-13)
(define-syntax scql
  (er-macro-transformer
   (lambda 
       (exp rename compare)
     (define last-command #f)
     (define (throw-exception msg)
       (abort (make-property-condition 'exn 'message msg)))
     (if (= (length exp) 1)
	 (throw-exception 
	  "Invalid Query Syntax: A level command is required (i,e: select, create, update or delete ,etc..)"))
     (let ((top-command (cadr exp))
	   (exp (cddr exp))
	   (str ""))
       (define (parse-symbol v)
	 (let*
	     ((is-defined   (eval `(condition-case ((lambda () ,v #t)) 
				     [(exn) #f])))
	      (procedure-name (if (and is-defined (not (string? v)))
				  (eval 
				   `(condition-case 
					((lambda () (car (string->symbol (procedure-information ,v))))) 
				      [(exn) #f ]
				      [var () (string->symbol 
					       (car (procedure-information ,v)))]))
				  #f))
	      (v (if procedure-name procedure-name v))
              (is-symbol    (if is-defined (eval `(symbol? ,v)) #f))
              (is-number    (if (and (not is-symbol) is-defined) (eval `(number? ,v)) #f))
              (is-string    (if (and (not is-number) is-defined) (eval `(string? ,v)) #f))
              (is-list      (if (and (not is-string) is-defined) (eval `(list?  ,v)) #f)))
	   (cond 
	    (is-string
	     (set! v (eval v)))
	    (is-list ;variable referencing a list
					;prevent a situation where we get caught in an endless loop
	     (let 
		 ((evaluated-parent (eval v))) ;evaluated `parent` values
	       (map 
		(lambda (x) 
		  (let 
		      ((evaluated-child (eval `(condition-case ((lambda() ,x)) [(exn) #f]))))
		    (if (equal? evaluated-child evaluated-parent)
			(let ((x (symbol->string x)))
			  (throw-exception 
			   (sprintf 
			    "Invalid Query Syntax: A circular reference was detected. ~A ~A ~A ~A ~A ~A "
			    "The scheme list named `" x "` contains a reference to itself."
			    "If you meant to define the literal \"" x 
			    "\", enclose it in quotes."))))))
		evaluated-parent)
	       (set! v (map parse-symbol evaluated-parent)))) ;we got past the errors, process list
	    ((list? v) ;literal list
	     (if (eq? (car v) 'quote)
		 (set! v (parse-symbol (car (cdr v))))
		 (set! v (map parse-symbol v))))
	    ((string? v) ;literal string
	     (void)) ;just return v, catch the `cond` statement, so it doesnt go to else
	    (is-number
	     (set! v (number->string (eval v))))
	    (procedure-name
	     (set! v procedure-name))
	    (else
	     (if (symbol? v)
		 (set! v (symbol->string  v))
		 (throw-exception 
		  (sprintf "Invalid Query Syntax: Value ~A is not of an expected type." v)))))
	   v))
       (define command-tree '())
       (define (descend-command-tree #!key
				     (required-previous-command `())
				     (override-current-command `())
				     (override-command-tree `())
				     (use-null-command-tree #f))
	 (let* ((c (if use-null-command-tree
		       '()
		       (if (= (length override-command-tree) 0)
			   command-tree
			   override-command-tree)))
		(c (if override-current-command
		       (append override-current-command c)
		       c)))
	   (if (> (length c) 2)
	       (begin
		 (if (and
		      (not use-null-command-tree)
		      (eq? override-command-tree '()))
		     (set! command-tree (cddr command-tree))
		     (void))
		 `((,(car c) ,(cadr c))
		   ,(cddr c)
		   ,required-previous-command))
	       `((,(car c) ,(cadr c))
		 ()
		 ,required-previous-command))))
       (define (process-join join on)
	 (let 
	     ((processed-str ""))
	   (if
	    (and  
	     (list? join)
	     (list? on)
	     (= (* (length on) 2)
		(length join)))
	    (begin
	      (define (p-o on-list p-str)
		(let 
		    ((first (equal? p-str "")))
		  (if
		   first 
		   (if 
		    (not (= (remainder (- (length on-list) 3) 4) 0))
		    (throw-exception 
		     (sprintf 
		      "Invalid on clause list passed, first clause requires 3 elements ~A ~A"
		      "(<operator> <col> <val>[...]), Subsequent additions to that clause require"
		      "4 elements (<logical operator and/or/etc.> <operator> <col> <val> [...])"))))
		  (if (>= (length on-list) (if first 3 4))
		      (begin 
			(set! p-str 
			  (sprintf 
			   "~A ~A ~A ~A ~A"
			   p-str 
			   (if first "on" (car on-list)) 
			   (if first (cadr on-list) (caddr on-list)) 
			   (if first (car on-list) (cadr on-list))
			   (if first (caddr on-list) (cadddr on-list))))
			(p-o ((if first cdddr cddddr) on-list) p-str))
		      p-str)))
	      (define (p-j join-list p-str)
		(if (>= (length join-list) 2)
		    (begin 
		      (set! p-str 
			(sprintf 
			 "~A ~A join ~A ~A" 
			 p-str (car join-list) (cadr join-list) (p-o (car on) "")))
		      (set! on (cdr on))
		      (p-j (cddr join-list) p-str))
		    p-str))
	      (p-j join ""))
	    (throw-exception 
	     (sprintf "Invalid Query Syntax: You must pass join clause as list, and on clause ~A"
		      "as nested list with one entry per join clause defining the terms of the join.")))))
       (define (process-clause type clause processed-str)
	 (let 
	     ((first (equal? processed-str ""))
	      (type  (if (string? type) type (symbol->string type))))
	   (if first 
	       (if (not (= (remainder (- (length clause) 3) 4) 0))
		   (throw-exception 
                    (sprintf 
		     "Invalid `~A` ~A ~A ~A"
		     type " clause list passed, first clause requires 3 elements (<operator> <col> <val> [...]),"
		     "Subsequent additions to that clause require 4 elements (<logical operator and/or/etc.>"
		     "<operator> <col> <val> [...])"))))
	   (if (or (and first (>= (length clause) 3))
		   (and (not first) (>= (length clause) 4)))
	       (begin 
		 (set! processed-str 
		   (sprintf 
                    " ~A ~A ~A ~A"
                    (if first type (sprintf "~A ~A" processed-str (car clause)))
                    ((if first cadr caddr) clause) 
                    ((if first car cadr) clause) 
                    ((if first caddr cadddr) clause)))
		 (process-clause type 
				 ((if first cdddr cddddr) clause) 
				 processed-str))
	       processed-str)))
       (define (process-order-by order-by processed-str)
	 (let 
	     ((first (equal? processed-str ""))
              (order-by-len (length order-by)))
	   (if (and (> order-by-len 0))
	       (begin 
		 (if (and 
                      first
                      (not (even? (length order-by))))
		     (throw-exception 
                      (sprintf 
		       "Invalid Query Syntax: `Order-by` clause requires a string or a list of even length defining ~A "
		       "pairs: the first element being the column name, the second being the order (asc or desc)"))
		     (set! processed-str 
		       (sprintf "~A ~A ~A ~A " processed-str (if first "" ",") (car order-by) (car (cdr order-by)))))
		 (process-order-by (cddr order-by) 
				   processed-str))
	       (if (not first)
		   processed-str
		   ""))))
       (define (process-insert-cols  cols)
	 (sprintf "insert into ~A (~A) " (car cols) (string-join (cdr cols) ",")))
       (define (process-update-cols vals processed-str)
	 (let 
	     ((first (equal? processed-str "")))
	   (if (>= (length vals) 2)
	       (begin (set! processed-str (sprintf "~A~A ~A = ~A"
						   processed-str  (if first "" ",")
						   (car vals) (cadr vals)))
                      (process-update-cols (cddr vals) processed-str))
	       (if (not first)
		   (sprintf "set ~A" processed-str)
		   processed-str))))
       (define (construct-sel-query distinct)
	 (set! command-tree
	   '(from fr as as join jo group-by
		  gr-by where wh having ha
		  limit lim order-by or-by
		  insert ins))
	 (let* 
	     ((nested (equal? (substring (symbol->string top-command) 0 2) "->"))
	      (cols   
	       (begin 
		 (if (<= (length exp) 1)
		     (throw-exception
		      "Invalid Query Syntax: column(s) definition is required."))
		 (let 
		     ((a (parse-symbol (car exp))))
		   (if
		    (not (or (list? a) (string? a)))
		    (throw-exception 
		     "Invalid Query Syntax: column(s) definition is of an invalid type."))
		   (if
		    (>= (length exp) 1)
		    (set! exp (cdr exp))
		    (throw-exception 
		     "Invalid Query Syntax: `From` clause expected after column(s) definition."))
		   a)))
	      (processed-results 
	       (map 
                (lambda (args)
                  (process-param current-command:       (car args)
                                 allowed-next-commands: (cadr args)
                                 allowed-last-commands: (caddr args)
                                 return-checks:         (cadddr args)))
                `((,@(descend-command-tree) 
		   (list? string?))
		  (,@(descend-command-tree required-previous-command: '(from)
					   override-command-tree: '(join jo on))
		   (list?)) 
		  (,@(descend-command-tree
		      required-previous-command: '(join)
		      override-current-command: '(on on)
		      override-command-tree:
		      '(as group-by gr-by
			   where wh
			   having ha
			   limit lim
			   order-by or-by
			   insert ins))
		   (list?))
		  (,@(descend-command-tree
		      override-current-command: '(where wh)
		      override-command-tree:
		      '(as group-by gr-by
			   having ha
			   limit lim
			   order-by or-by
			   insert ins))
		   (list? string?))
		  (,@(descend-command-tree
		      override-current-command: '(group-by gr-by)
		      override-command-tree:
		      '(as having ha
			   limit lim
			   order-by
			   or-by
			   insert ins))
		   (list? string?))
		  (,@(descend-command-tree
		      override-current-command: '(having ha)
		      override-command-tree:
		      '(as group-by gr-by
			   limit lim
			   order-by or-by
			   insert ins))
		   (list? string?))
		  (,@(descend-command-tree
		      override-current-command: '(order-by or-by)
		      override-command-tree: '(limit lim insert ins))
		   (list? string?))
		  (,@(descend-command-tree
		      override-current-command: '(limit lim)
		      override-command-tree: '(as insert ins))
		   (list? string?))
		  (,@(descend-command-tree
		      override-current-command: '(as as)
		      override-command-tree: '(insert ins))
		   (string?))
		  (,@(descend-command-tree
		      override-current-command: '(insert ins)
		      use-null-command-tree: #t)
		   (list? string?)))))
	      (tables (car processed-results))
	      (join (cadr processed-results))
	      (on  (caddr processed-results))
	      (where-clause    (cadddr processed-results))
	      (group-by        (car (cddddr processed-results)))
	      (having-clause (cadr (cddddr processed-results)))
	      (order-by (caddr (cddddr processed-results)))
	      (limit    (cadddr (cddddr processed-results)))
	      (as       (cadddr (cddddr (cdr processed-results))))
	      (insert   (cadddr (cddddr (cddr processed-results)))))
           (if (not tables)
               (throw-exception
                (sprintf "~A ~A"
                         "Invalid Query Syntax:"
                         "`From` clause malformed.")))
	   (sprintf 
	    "~A~Aselect ~A ~A from ~A~A~A~A~A~A~A~A~A"
	    (if nested "(" "")
	    (if (list? insert) 
		(if (>= (length insert) 2)
		    (process-insert-cols insert)
		    (throw-exception 
		     (sprintf 
		      "~A ~A ~A "
		      "Invalid Query Syntax: `Select Insert` clause must contain at"
		      "least 2 elements, the first being the table name, the second and"
		      "so forth being column names.")))
		(if (and (string? insert) (> (string-length insert) 0)) 
		    (sprintf "~A ~A " "insert into" insert) 
		    ""))
            (if distinct "distinct" "")
	    (if (list? cols) (string-join cols ",") cols)
	    (if (list? tables) (string-join tables ",") tables)
	    (if (and (list? join) (list? on)) (process-join join on) "")
	    (if (list? where-clause) (process-clause 'where where-clause "")
		(if (and (string? where-clause) (> (string-length where-clause) 0))
		    (sprintf "~A ~A" "where" where-clause)
		    ""))
	    (if (list? group-by)
		(if (> (length group-by) 0) 
		    (sprintf " group by ~A " (string-join group-by ",")) 
		    "")
		(if (and (string? group-by) 
			 (> (string-length group-by) 0)) 
		    (sprintf " group by ~A " 
			     group-by) 
		    ""))
	    (if (list? having-clause)     
		(process-clause 'having having-clause "")
		(if (and (string? having-clause)
			 (> (string-length having-clause) 0))
		    (sprintf " ~A ~A " "having" having-clause)
		    ""))
	    (if (list? order-by) 
		(sprintf " ~A ~A " "order by" (process-order-by order-by ""))
		(if (and (string? order-by) (> (string-length order-by) 0))
		    (sprintf " ~A ~A " "order by" order-by) ""))
	    (if (list? limit) 
		(sprintf " ~A ~A " "limit" (string-join limit ","))
		(if (and (string? limit) (> (string-length limit) 0))
		    (sprintf " ~A ~A " "limit" limit) ""))
	    (if nested ")" "")
            (if (string? as)
                (if (> (string-length as) 0)
                    (sprintf " ~A ~A " "as" as)
                    "")
                ""))))
       (define (construct-upd-query)
	 (set! command-tree
	   '(where wh
		   having ha
		   limit lim
		   order-by or-by))
	 (let* 
	     ((vals  
	       (begin 
		 (if
		  (< (length exp) 1)
		  (throw-exception
		   "Invalid Query Syntax: Values definition is required."))
		 (let 
		     ((a (parse-symbol (car exp))))
		   (if
		    (not (or (list? a) (string? a)))
		    (throw-exception
		     "Invalid Query Syntax: Values definition is of an invalid type.")
		    (if
		     (and (list? a)
			  (not (even? (sub1 (length a)))))
		     (throw-exception
		      (sprintf "Invalid Query Syntax: Values definition elements ~A ~A ~A "
			       "following first element which is table"
			       "definition must be a list of even length with each 2"
			       "values representing a column/value pair."))))
		   (if (>= (length exp) 1)
		       (set! exp (cdr exp))
		       (throw-exception "Invalid Query Syntax: Values definition expected after table definition."))
		   a)))
	      (processed-results 
	       (map 
		(lambda (args)
		  (process-param current-command: (car args)
				 allowed-next-commands: (cadr args)
				 allowed-last-commands: (caddr args)
				 return-checks: (cadddr args)))
		`((,@(descend-command-tree
		      override-current-command: '(join jo)
		      override-command-tree: '(on))
		   (list?))
		  (,@(descend-command-tree
		      override-current-command: '(on on)
		      override-command-tree:
		      '(as where wh having ha
			   limit lim order-by
			   or-by insert ins)
		      required-previous-command: '(join))
		   (list?))
		  (,@(descend-command-tree)
		   (list? string?))
		  (,@(descend-command-tree)
		   (list? string?))
		  (,@(descend-command-tree)
		   (list? string?))
		  (,@(descend-command-tree)
		   (list? string?)))))
	      (join  (car processed-results))
	      (on    (cadr processed-results))
	      (where-clause (caddr processed-results))
	      (having-clause (cadddr processed-results))
	      (limit    (car (cddddr processed-results)))
	      (order-by (cadr (cddddr processed-results))))
           (if (not (list? vals))
               (throw-exception
                "Invalid Query Syntax: Parameters for update must be passed as a list."))
           (if (not (and
                     (odd? (length vals))
                     (> (length vals) 1)))
               (throw-exception
                (sprintf "~A ~A"
                         "Invalid Query Syntax: Table and Column(s) list must contain table name"
                         "and one or more value pairs.")))
	   (sprintf 
            "update ~A~A~A~A~A~A"
            (sprintf " ~A ~A " (car vals) (process-update-cols (cdr vals) ""))
            (if (and (list? join) (list? on))
                (process-join join on)
                "")
            (if (list? where-clause) 
                (process-clause 'where where-clause "")
                (if (and (string? where-clause) (> (string-length where-clause) 0))
                    (sprintf " ~A ~A " "where" where-clause)
                    ""))
            (if (list? having-clause) 
                (process-clause 'having having-clause "")
                (if (and (string? having-clause) (> (string-length having-clause) 0))
                    (sprintf " ~A ~A " "having" having-clause)
                    ""))
            (if (list? order-by) 
                (sprintf " ~A ~A " "order by" (process-order-by order-by ""))
                (if (and (string? order-by) (> (string-length order-by) 0))
                    (sprintf " ~A ~A " "order by" order-by)
                    ""))
            (if (list? limit) 
                (sprintf " ~A ~A " "limit" (string-join limit ","))
                (if (and (string? limit) (> (string-length limit) 0))
                    (sprintf " ~A ~A "  "limit" limit)
                    "")))))
       (define (construct-del-query)
	 (set! command-tree '(from fr where wh limit lim order-by or-by))
	 (let* 
	     ((processed-results 
	       (map 
                (lambda (args)
                  (process-param current-command: (car args)
                                 allowed-next-commands: (cadr args)
                                 allowed-last-commands: (caddr args)
                                 return-checks: (cadddr args)))
                `((,@(descend-command-tree) (string?))
		  (,@(descend-command-tree) (list? string?))
		  (,@(descend-command-tree) (list? string?))
		  (,@(descend-command-tree) (list? string?)))))
              (table         (car processed-results))
              (where-clause  (cadr processed-results))
              (limit         (caddr processed-results))
	      (order-by      (cadddr processed-results)))
	   (sprintf 
            "delete from ~A~A~A~A"
            table
            (if (list? where-clause) 
                (process-clause 'where where-clause "")
                (if (and (string? where-clause) (> (string-length where-clause) 0))
                    (sprintf " ~A ~A " "where" where-clause)
                    ""))
            (if (list? order-by) 
                (sprintf " ~A ~A " "order by" (process-order-by order-by ""))
                (if (and (string? order-by) (> (string-length order-by) 0))
                    (sprintf " ~A ~A " "order by" order-by)
                    ""))
            (if (list? limit) 
                (sprintf " ~A ~A " "limit" (string-join limit ","))
                (if (and (string? limit)
			 (> (string-length limit) 0))
                    (sprintf " ~A ~A " "limit" limit)
                    "")))))
       (define (construct-ins-query)
	 (let* 
	     ((pairs    
	       (begin 
		 (if (<= (length exp) 0)
		     (throw-exception "Invalid Query Syntax: Column(s) definition is required."))
		 (let 
		     ((a (parse-symbol (car exp))))
		   (if (not (or (list? a) (string? a)))
		       (throw-exception 
                        "Invalid Query Syntax: Column(s) definition is of an invalid type."))
		   (if (>= (length exp) 1)
		       (set! exp (cdr exp))
		       (throw-exception 
                        "Invalid Query Syntax: Column(s) definition expected in insert statement."))
		   a)))
              (table (car pairs))
              (pairs (cdr pairs))
              (flds '())
              (vals '()))
	   (if (not (even? (length pairs)))
	       (throw-exception 
                (sprintf 
		 "Invalid Query Syntax: `Insert` clause excpects a list with ~A ~A ~A "
		 "the first element defining the table name and all subsequent elements"
		 "of the list being a group of associated field and value pairs"
		 "(e.g, (scql ins (table1 fld1 \"FOO\" fld2 \"BAR\" fld3 122)))")))
	   (define (get-flds-and-vals pairs)
	     (if (>= (length pairs) 2)
		 (begin (set! flds (append flds (list (car pairs))))
                        (set! vals (append vals (list (cadr pairs))))
                        (get-flds-and-vals (cddr pairs)))))
	   (get-flds-and-vals pairs)
	   (sprintf "~A values(~A)" (process-insert-cols (append (list table) flds)) (string-join vals ","))))
       (define (construct-cre-tmp-query distinct)
	 (let* ((table-name 
		 (if (>= (length exp) 1)
		     (parse-symbol (car exp))
		     (throw-exception 
                      (sprintf "Invalid Query Syntax: `create-tmp` clause expects a string or symbol ~A "
                               "defining name of table name")))))
	   (set! exp (cdr exp))
	   (sprintf "~A ~A ~A" "create temporary table" table-name (construct-sel-query distinct))))
       
       (define (process-param #!key 
			      current-command ;list of command currently being processed it's abbreviations
			      allowed-next-commands ;list of acceptable upcoming commands and their abbreviations
			      allowed-last-commands
			      return-checks)
					;deal with stray unchecked clutter after main clause
	 (if (and (= (length exp) 1 )
		  (not (member (car exp) current-command)))
	     (throw-exception 
              "Invalid Query Syntax: There was an invalid symbol where the end of the query was expected."))
	 (if (> (length exp) 0 )
	     (let((a (parse-symbol (car exp)))
		  (command-title (car current-command)))
	       (if (not (string? a))
		   (throw-exception 
                    (sprintf 
		     "Invalid Syntax Error: Unexpected data type expecting ~A or ~A " 
		     (string-join current-command ",")
		     (string-join allowed-next-commands ",")))
		   (set! a (string-downcase a)))
	       (if (member a (map symbol->string current-command)) ;does curent sql command match given clause?
		   (set! a 
		     (begin 
		       (if (< (length exp) 2) ;command given without clause
			   (throw-exception 
                            (sprintf "Invalid Query Syntax: Argument to `~A` clause not found. "
                                     (symbol->string command-title))))
		       (let ((b (parse-symbol (car (cdr exp))))) ;get argument coming after command
			 (set! exp (cddr exp))
			 (if (not (fold 
                                   (lambda (new old) 
                                     (set! new (eval `(begin ,new)))
                                     (or old (new b)))
                                   #f
                                   return-checks)) ;run all the return checks
			     (throw-exception 
                              (sprintf 
			       "~A ~A ~A ~A"
			       "Invalid Query Syntax: Argument to "
			       "failed to meet one of the following required type checks: "
			       (car current-command)
			       (string-join (map symbol->string return-checks) ","))))
					;check that the next command is expected 
			 (define exp-len (length exp))
			 (if 
                          (> exp-len 1)
                          (let ((a  (string-downcase (parse-symbol (car exp)))))
                            (if (>= exp-len 2)
                                (if (not (and (or (eq? allowed-last-commands '())
                                                  (member last-command allowed-last-commands))
                                              (fold (lambda (new old) 
                                                      (or old (and (eq? new (string->symbol a)))))
                                                    #f
                                                    allowed-next-commands)))
                                    (throw-exception 
				     (sprintf 
				      "~A ~A ~A ~A ~A"
				      "Invalid Query Syntax: Expected one of the following:"
				      (string-join (map symbol->string allowed-next-commands) ",")
				      "After"
				      (car current-command)
				      (if (> (length allowed-last-commands) 0)
					  (sprintf 
					   " And one of the following: ~A preceding ~A."
					   (string-join (map symbol->string allowed-last-commands) ",")
					   (car current-command))
					  ".")))))
                            (set! last-command command-title)))
			 b)))
		   (set! a #f))
	       a)
	     ""))
       (if (string? top-command)
	   (set! top-command (string->symbol top-command)))
       (cond ((or (eq? top-command 'sel)
		  (eq? top-command 'select)
		  (eq? top-command '->sel)
		  (eq? top-command '->select))
              (construct-sel-query #f))
             ((or (eq? top-command 'sel-distinct)
                  (eq? top-command 'select-distinct)
                  (eq? top-command '->sel-distinct)
                  (eq? top-command '->select-distinct))
              (construct-sel-query #t))
	     ((or (eq? top-command 'cr-tmp)
		  (eq? top-command 'create-tmp))
              (construct-cre-tmp-query #f))
     	     ((or (eq? top-command 'cr-tmp-distinct)
		  (eq? top-command 'create-tmp-with-distinct-rows))
              (construct-cre-tmp-query #t))
	     ((or (eq? top-command 'upd)
		  (eq? top-command 'update))
              (construct-upd-query))
	     ((or (eq? top-command 'del)
		  (eq? top-command 'delete))
              (construct-del-query))                          
	     ((or (eq? top-command 'ins)
		  (eq? top-command 'insert))
              (construct-ins-query))
             (else
              (throw-exception
               (sprintf
                "~A ~A ~A ~A"
                "Error Unexpected top level command encountered, "
                top-command
                ".Expected select, select-distinct, create-tmp, update, delete or "
                "insert, or their respective abbreviations."))))))))
