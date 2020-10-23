package main

/*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************/

const
   NAMELENG = 20;      (* Maximum length of a name *)
   MAXINPUT = 500;     (* Maximum length of an input *)
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';

type Name string

type EXP interface {
	func Apply()
}

type VALEXP {
	NUMBER
}
func (VALEXP) Apply() {}

type VAREXP {
	varble int
}
func (VAREXP) Apply() {}

type APEXP {
	optr int
	args EXPLIST
}
func (APEXP) Apply() {}

type
   NUMBER = integer;

//	a NAME is an index in printNames
      
   BUILTINOP = (IFOP, WHILEOP, SETOP, BEGINOP, PLUSOP, MINUSOP, TIMESOP, DIVOP, EQOP, LTOP, GTOP, PRINTOP);
   VALUEOP = PLUSOP .. PRINTOP;
   CONTROLOP = IFOP .. BEGINOP;

   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;
   FUNDEF = ^FUNDEFREC;

   EXPTYPE = (APEXP);

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: NUMBER;
               tail: VALUELIST
            end;

   NAMELISTREC = record
               head: int;
               tail: NAMELIST
            end;

type ENV struct {
	vars	NAMELIST
	values	VALUELIST
}

   FUNDEFREC = record
               funname: int;
               formals: NAMELIST;
               body: EXP;
               nextfundef: FUNDEF
            end;

var fundefs			FUNDEF
var globalEnv		*ENV
var userinput		[]byte
var inputleng		int
var pos				int
var printNames		[]string
var numNames		int
var numBuiltins		int
var quittingtime	bool

/*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************/

// lengthVL - return length of VALUELIST vl
func lengthVL(vl VALUELIST) (i int) {
	for vl != nil {
		i += 1
		vl = vl.tail
	}
	return
}

// lengthNL - return length of NAMELIST nl
func lengthNL(nl NAMELIST) (i int) {
	for nl != nil {
		i += 1
		nl = nl.tail
	}
	return
}

/*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************/

// fetchFun - get function definition of fname from fundefs
func fetchFun(fname int) (r FUNDEF) {
	found := false
	r = fundefs
	for r != nil && !found {
		if f.funname == fname {
			found = true
		} else {
			r := r.nextfundef
		}
    }
	return
}

// newFunDef - add new function fname w/ parameters nl, body e
func newFunDef(fname int, nl NAMELIST, e EXP) {
	f := fetchFun(fname)
	if f == nil {
		f = &FUNDEF { nextfundef: fundefs }
		fundefs = f
	}
	f.funname = fname
	f.formals = nl
	f.body = e
}

// initNames - place all pre-defined names into printNames
func initNames() {
	fundefs = nil
	printNames = []string {
		"", "if", "while", "set",
		"begin", "+", "-", "*",
		"/", "=", "<", ">",
		"print",
	}
   numNames = len(printNames)
   numBuiltins = numNames
}

// install - insert new name into printNames
func install(nm string) (r int) {
	found := false
	for (r <= numNames) && !found {
		if nm == printNames[r] {
			found = true
		} else {
			r += 1
		}
	}
	if !found {
		numNames = r
		printNames[r] = nm
	}
	return
}

// primOp - translate optr to corresponding BUILTINOP
func primOp(optr int) (r BUILTINOP) {

	r = IFOP	// N.B. IFOP is first value in BUILTINOPS
	for i := 1; i < optr - 1; op = succ(op) {}
	return
}

/*****************************************************************
 *                        INPUT                                  *
 *****************************************************************/

// isDelim - check if c is a delimiter
func isDelim(c char) bool {
	return c in ['(', ')', ' ', COMMENTCHAR]
}

// skipblanks - return next non-blank position in userinput
func skipblanks(p int) int {
	for userinput[p] = ' ' {
		p += 1
	}
	return p
}

// matches - check if string nm matches userinput[s .. s+leng]
func matches(s int, leng int, nm string) (ok bool) {
	ok = true
	for i := 1; ok && i <= leng; {
		if userinput[s] <> nm[i] {
			ok = false
		}
    	i += 1
		s += 1
	}
	if !isDelim(userinput[s]) {
		ok = false
	}
	return
}

// reader - read char's into userinput; be sure input not blank
func reader() {
	//	readInput - read char's into userinput
	func readInput() {
	   var c char

		// nextchar - read next char - filter tabs and comments
		func nextchar(c char) {
			read(c)
			switch c {
			case '\t':
				c = ' '
			case COMMENTCHAR:
				for !eoln do {
					read(c);
					c = ' '
				}
			}
		}

		// readParens - read char's, ignoring newlines, to matching ')'
		func readParens() {
			var c char
			parencnt := 1	// '(' just read
			for parencnt != 0 {
	            if eoln {
            		write(PROMPT2)
				}
	            nextchar(c)
	            pos += 1
	            if pos = MAXINPUT {
					writeln('User input too long');
					goto 99
				}
	            userinput[pos] = c
	            switch c {
	            case '(':
					parencnt += 1
	            case ')':
		            parencnt -= 1
	            }
			}
		}

		write(PROMPT)
		pos = 0
		for !eoln {
			pos += 1
			if pos == MAXINPUT {
				writeln('User input too long')
				goto 99
			}
			nextchar(c)
			userinput[pos] = c
			if userinput[pos] = '(' {
				readParens
			}
		}
		inputleng = pos
		userinput[pos + 1] = COMMENTCHAR	//	sentinel
	}

	// ignore blank lines
	for pos > inputleng {
       readInput()
       pos = skipblanks(1)
	}
}

// parseName - return (installed) Name starting at userinput[pos]
func parseName() Name {
	n := ""
	for pos <= inputleng && !isDelim(userinput[pos]) {
         n += userinput[pos]
         pos += 1
	}
	if len(n) = 0 {
		writeln('Error: expected name, instead read: ', userinput[pos]);
		goto 99
	}
	pos = skipblanks(pos)
	return install(nm)
}

// isNumber - check if a number begins at pos
func isNumber(pos int) bool {

	// isDigits - check if sequence of digits begins at pos
   func isDigits(pos int) (ok bool) {
		if userinput[pos] in ['0'..'9'] {
			ok = true
			for userinput[pos] in ['0'..'9'] {
				pos += 1
				if !isDelim(userinput[pos]) {
					ok = false
				}
			}
		}
		return
	}

	return isDigits(pos) || ((userinput[pos] = '-') && isDigits(pos+1))
}

// parseVal - return number starting at userinput[pos]
func parseVal() NUMBER {
	n := 0
	sign := 1
	if userinput[pos] = '-' {
		sign = -1
		pos += 1
	}
	for userinput[pos] in ['0'..'9'] {
		n = 10 * n + (ord(userinput[pos]) - ord('0'))
		pos += 1
	}
	pos = skipblanks(pos)	// skip blanks after number
	return n * sign
}

// parseExp - return EXP starting at userinput[pos]
func parseExp() (r EXP) {
	switch {
	case userinput[pos] = '(':
		pos = skipblanks(pos + 1)	// skip '( ..'
		r = &APEXP{ parseName(), parseEL() }
	case isNumber(pos):
		r = &VALEXP{parseVal())	// VALEXP
	default:
		r = &VAREXP{ parseName() } // VAREXP
	}
	return
}

// parseEL - return EXPLIST starting at userinput[pos]
func parseEL() (r EXPLIST) {
	if userinput[pos] = ')' {
		pos = skipblanks(pos + 1) // skip ') ..'
	} else {
		r = &EXPLIST{ parseExp(), parseEL() }
	}
	return
}

// parseNL - return NAMELIST starting at userinput[pos]
func parseNL() (r NAMELIST) {
	if userinput[pos] = ')' {
		pos = skipblanks(pos + 1) // skip ') ..'
	} else {
		r = mkNamelist(parseName(), parseNL())
	}
	return
}

// parseDef - parse function definition at userinput[pos]
func parseDef() int {
	pos = skipblanks(pos + 1)	// skip '( ..'
	pos = skipblanks(pos + 6)	// skip 'define ..'
	fname := parseName()
	pos = skipblanks(pos + 1)	// skip '( ..'
	nl := parseNL()
	e := parseExp()
	pos = skipblanks(pos + 1)	// skip ') ..'
	newFunDef(fname, nl, e)
	return fname
}

/*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************/

// bindVar - bind variable nm to value n in environment rho
func bindVar(nm int, n NUMBER, rho *ENV) {
	rho.vars = &NAMELIST{ nm, rho.vars }
	rho.values = &VALUELIST{ n, rho.values }
}

// findVar - look up nm in rho                                   *)
func findVar(nm int, rho *ENV) VALUELIST {
	found := false
	nl := rho.vars
	vl := rho.values
	for nl != nil && !found {
		if nl.head == nm {
			found = true
		} else {
			nl = nl.tail
			vl = vl.tail
		}
	}
	return vl
}

// assign - assign value n to variable nm in rho
func assign(nm int, n NUMBER, rho *ENV) {
	findVar(nm, rho).head = n
}

// fetch - return number bound to nm in rho
func fetch(nm int, rho *ENV) NUMBER {
   return findVar(nm, rho).head
}

// isBound - check if nm is bound in rho
func isBound(nm int, rho *ENV) bool {
   return findVar(nm, rho) != nil
}

(*****************************************************************
 *                     NUMBERS                                   *
 *****************************************************************)

// prValue - print number n
func prValue(n NUMBER) {
	fmt.Print(n:1)
}

// isTrueVal - return true if n is a true (non-zero) value
func isTrueVal(n NUMBER) bool {
   return n != 0
}

// applyValueOp - apply VALUEOP op to arguments in VALUELIST vl
func applyValueOp(op VALUEOP, vl VALUELIST) (r NUMBER) {
	var n1, n2 NUMBER

	// arity - return number of arguments expected by op             *)
	func arity(op VALUEOP) (r int) {
		if op in [PLUSOP .. GTOP] {
			r = 2
		} else {
			r = 1
		}
		return
	}

	if arity(op) != lengthVL(vl) {
		fmt.Println("Wrong number of arguments to %v\n", ord(op) + 1)
		goto 99
	}
	n1 := vl.head	// 1st actual
	if arity(op) = 2 {
		n2 = vl.tail.head	// 2nd actual
	}

	switch op.(type) {
	case PLUSOP:
		r = n1 + n2
	case MINUSOP:
		r = n1 - n2
	case TIMESOP:
		r = n1 * n2
	case DIVOP:
		r = n1 div n2
	case EQOP:
		if n1 = n2 {
			r = 1
		} else {
			r = 0
		}
	case LTOP:
		if n1 < n2 {
			r = 1
		} else {
			r = 0
		}
	case GTOP:
		if n1 > n2 {
			r = 1
		} else {
			r = 0
		}
	PRINTOP:
		prValue(n1)
		fmt.Println()
		r = n1
	}
	return
}

/*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************/

// eval - return value of expression e in local environment rho
func eval(e *EXP, rho *ENV) (r NUMBER) {
	var op BUILTINOP

	// evalList - evaluate each expression in el
	func evalList(el EXPLIST) VALUELIST {
		var h NUMBER
		var t VALUELIST

		if el == nil {
			evalList = nil
		} else {
			h = eval(el.head, rho)
			t = evalList(el.tail)
			evalList = &VALUELIST{ h, t }
		}
	}

// applyUserFun - look up definition of nm and apply to actuals
	func applyUserFun(nm int, actuals VALUELIST) NUMBER {
		if f := fetchFun(nm); f == nil {
			fmt.Println("Undefined function: %v\n", nm)
			goto 99
		}

		if lengthNL(f.formals) != lengthVL(f.actuals) {
			fmt.Println("Wrong number of arguments to: %v\n", nm)
			goto 99
		}
		return eval(body, &ENV(f.formals, f.actuals))
	}

// applyCtrlOp - apply CONTROLOP op to args in rho
	function applyCtrlOp(op CONTROLOP, args EXPLIST) (r NUMBER) {
		switch op {
		case IFOP:
			if isTrueVal(eval(args.head, rho)) {
				r = eval(args.tail.head, rho)
			} else {
				r = eval(args.tail.tail.head, rho)
			}
		case WHILEOP:
			for r = eval(args.head, rho); isTrueVal(n) {
				r = eval(args.tail.head, rho)
				r = eval(args.head, rho)
			}
		case SETOP:
			r = eval(args.tail.head, rho)
			if isBound(args.head.varble, rho) {
				assign(args.head.varble, r, rho)
			} else if isBound(args.head.varble, globalEnv) {
				assign(args.head.varble, r, globalEnv)
			} else {
				bindVar(args.head.varble, r, globalEnv)
			}
		case BEGINOP: 
			for args.tail != nil {
				r = eval(args.head, rho)
				args = args.tail
			}
			r = eval(args.head, rho)
		}
		return
	}

	switch e := e.(type) {
	case VALEXP:
		r = e.num
	case VAREXP:
		if isBound(e.varble, rho) {
			r = fetch(e.varble, rho)
		} else if isBound(e.varble, globalEnv) {
			r = fetch(e.varble, globalEnv)
		} else {
			fmt.Printf("Undefined variable: %v\n", e.varble)
			goto 99
		}
	case APEXP: 
		if e.optr > numBuiltins {
			r = applyUserFun(e.optr, evalList(e.args))
		} else {
			op = primOp(e.optr)
		}
		if op in [IFOP .. BEGINOP] {
			r = applyCtrlOp(op, e.args)
		} else {
			r = applyValueOp(op, evalList(e.args))
		}
	}
	return
}

(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

func main() {
	initNames()
	globalEnv := &ENV{}

	for quittingtime := false; !quittingtime; {
		func() {
			reader()
			if matches(pos, 4, 'quit                ') {
				quittingtime = true
			} else if (userinput[pos] = '(') && matches(skipblanks(pos + 1), 6, 'define              ') {
				fmt.Println(parseDef())
			} else {
				prValue(eval(parseExp(), &ENV))
				fmt.Println()
				fmt.Println()
			}
		}()
	}
}