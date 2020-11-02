package main

import "bufio"
import "fmt"
import "os"

const PROMPT = '-> '
const PROMPT2 = '> '
const COMMENTCHAR = ';'
const LPARENCHAR = "("
const RPARENCHAR = ")"

type Name string

type Expression interface {
	func Apply()
}

type Value int
func (v Value) Apply() {}

type Variable int
func (v Variable) Apply() {}

type Application struct {
	Op int
	ExpressionList
}
func (a Application) Apply() {}

const (
	IFOP = iota
	WHILEOP
	SETOP
	BEGINOP
	PLUSOP
	MINUSOP
	TIMESOP
	DIVOP
	EQOP
	LTOP
	GTOP
	PRINTOP
)

// CONTROLOP = IFOP .. BEGINOP;
type ControlOp int
func (c ControlOp) Apply() {}

// VALUEOP = PLUSOP .. PRINTOP;
type ValueOp int
func (v ValueOp) Apply() {}

type ExpressionList struct {
	*Expression
	*ExpressionList
}

// evalList - evaluate each expression in el
func (el *ExpressionList) Eval(rho *Environment) (r *ValueList) {
	if el != nil {
		r = &ValueList {
			eval(el.Expression, rho),
			evalList(el.ExpressionList),
		}
	}
	return
}


type ValueList struct {
	Value
	*ValueList
}

func (v *ValueList) Len() (i int) {
	for v != nil {
		i += 1
		v = v.ValueList
	}
	return
}

type NameList struct {
	head	int
	*NameList
}

func (n *NameList) Len() (i int) {
	for n != nil {
		i += 1
		n = n.ValueList
	}
	return
}

type Environment struct {
	*NameList
	*ValueList
}

type Function struct {
	Name		int
	*NameList
	*Expression
	*Function
}

var Functions		*Function
var globalEnv		*Environment
var userinput		[]string

var ProgramInput	bufio.Reader

var pos				int

type Builtin struct {
	Name	string
	Arity	int
}

var Builtins		[]Builtin

func init() {
	ProgramInput = bufio.NewReader(os.Stdin)
	Builtins = []Builtin {
		Builtin{ "", 0 },
		Builtin{ "if", 1 },
		Builtin{ "while", 1 },
		Builtin{ "set", 1 },
		Builtin{ "begin", 1 },
		Builtin{ "+", 2 },
		Builtin{ "-", 2 },
		Builtin{ "*", 2 },
		Builtin{ "/", 2 },
		Builtin{ "=", 2 },
		Builtin{ "<", 2 },
		Builtin{ ">", 2 },
		Builtin{ "print", 1 },
	}
}

/*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************/

// fetchFun - get function definition of fname from Functions
func fetchFun(fname int) (r *Function) {
	found := false
	r = Functions
	for r != nil && !found {
		if f.Name == fname {
			found = true
		} else {
			r := r.Function
		}
    }
	return
}

// newFunDef - add new function fname w/ parameters nl, Expression e
func newFunDef(fname int, nl *NameList, e *Expression) {
	f := fetchFun(fname)
	if f == nil {
		f = &Function{ Function: Functions }
		Functions = f
	}
	f.Name = fname
	f.NameList = nl
	f.Expression = e
}

// install - insert new name into Builtins
func install(nm string) (r int) {
	var found bool
	for i, n := range Builtins {
		if n == nm {
			found = true
			r = i
			break
		}
	}
	if !found {
		r = len(Builtins)
		Builtins = append(Builtins, Builtin{ nm, 0 })
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
func isDelim(c char) (ok bool) {
	switch c {
	case '(', ')', ' ', COMMENTCHAR:
		ok = true
	}
	return
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
            		fmt.Print(PROMPT2)
				}
	            nextchar(c)
	            userinput += c
	            switch c {
	            case LPARENCHAR:
					parencnt += 1
	            case RPARENCHAR:
		            parencnt -= 1
	            }
			}
		}

		fmt.Print(PROMPT)
		pos = 0


		line, e := ProgramInput.ReadBytes('\n')
		if e != nil && e != io.EOF {
			panic(e)
		}
		sline := strings.TrimSpace(string(line))

		for _, c := range sline {
			switch c {
			case "\t":
			case COMMENTCHAR:
				break
			case LPARENCHAR:
				userinput += c
				readParens()
			default:
				userinput += c
			}
		}
		userinput += COMMENTCHAR	//	sentinel
	}

	// ignore blank lines
	for len(userinput) == 0 {
       readInput()
       pos = skipblanks(1)
	}
}

// parseName - return (installed) Name starting at userinput[pos]
func parseName() Name {
	n := ""
	for pos <= len(userinput) && !isDelim(userinput[pos]) {
         n += userinput[pos]
         pos += 1
	}
	if len(n) = 0 {
		fmt.Printf('Error: expected name, instead read: %v\n', userinput[pos]);
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
func parseVal() int {
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

// parseExp - return Expression starting at userinput[pos]
func parseExp() (r *Expression) {
	switch {
	case userinput[pos] = '(':
		pos = skipblanks(pos + 1)	// skip '( ..'
		r = &Application {
			parseName(),
			parseEL(),
		}
	case isNumber(pos):
		r = Value(parseVal())
	default:
		r = Variable(parseName())
	}
	return
}

// parseEL - return ExpressionList starting at userinput[pos]
func parseEL() (r *ExpressionList) {
	if userinput[pos] = ')' {
		pos = skipblanks(pos + 1) // skip ') ..'
	} else {
		r = &ExpressionList{ parseExp(), parseEL() }
	}
	return
}

// parseNL - return NameList starting at userinput[pos]
func parseNL() (r *NameList) {
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
func bindVar(nm, n int, rho *Environment) {
	rho.NameList = &NameList{ nm, rho.NameList }
	rho.ValueList = &ValueList{ n, rho.ValueList }
}

// findVar - look up nm in rho                                   *)
func findVar(nm int, rho *Environment) *ValueList {
	found := false
	nl := rho.NameList
	vl := rho.ValueList
	for nl != nil && !found {
		if nl.head == nm {
			found = true
		} else {
			nl = nl.NameList
			vl = vl.ValueList
		}
	}
	return vl
}

// assign - assign value n to variable nm in rho
func assign(nm int, n Value, rho *Environment) {
	findVar(nm, rho).Value = n
}

// fetch - return number bound to nm in rho
func fetch(nm int, rho *Environment) Value {
   return findVar(nm, rho).Value
}

// isBound - check if nm is bound in rho
func isBound(nm int, rho *Environment) bool {
   return findVar(nm, rho) != nil
}

(*****************************************************************
 *                     NUMBERS                                   *
 *****************************************************************)

// prValue - print number n
func prValue(n int) {
	fmt.Print(n:1)
}

// isTrueVal - return true if n is a true (non-zero) value
func isTrueVal(n int) bool {
   return n != 0
}

// applyValueOp - apply VALUEOP op to arguments in *ValueList vl
func applyValueOp(op VALUEOP, vl *ValueList) (r int) {
	if op.Arity != vl.Len() {
		fmt.Println("Wrong number of arguments to %v\n", ord(op) + 1)
		goto 99
	}
	n1 := vl.Value	// 1st actual
	n2 := 0

	if op.Aarity = 2 {
		n2 = vl.ValueList.Value	// 2nd actual
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
func eval(e *Expression, rho *Environment) (r int) {
	var op BUILTINOP

// applyUserFun - look up definition of nm and apply to actuals
	applyUserFun := func(nm int, actuals *ValueList) int {
		f := fetchFun(nm)

		if f == nil {
			fmt.Println("Undefined function: %v\n", nm)
			goto 99
		}

		if f.NameList.Len() != f.actuals.Len() {
			fmt.Println("Wrong number of arguments to: %v\n", nm)
			goto 99
		}
		return eval(f.Expression, &Environment(f.NameList, f.actuals))
	}

// applyCtrlOp - apply CONTROLOP op to args in rho
	applyCtrlOp := func(op CONTROLOP, args *ExpressionList) (r int) {
		switch op {
		case IFOP:
			if isTrueVal(eval(args.Expression, rho)) {
				r = eval(args.ExpressionList.Expression, rho)
			} else {
				r = eval(args.ExpressionList.ExpressionList.Expression, rho)
			}
		case WHILEOP:
			for r = eval(args.Expression, rho); isTrueVal(n) {
				r = eval(args.ExpressionList.Expression, rho)
				r = eval(args.Expression, rho)
			}
		case SETOP:
			v := args.Expression.(Variable)
			r = eval(args.ExpressionList.Expression, rho)
			if isBound(v, rho) {
				assign(v, r, rho)
			} else if isBound(v, globalEnv) {
				assign(v, r, globalEnv)
			} else {
				bindVar(v, r, globalEnv)
			}
		case BEGINOP: 
			for args.ExpressionList != nil {
				r = eval(args.Expression, rho)
				args = args.ExpressionList
			}
			r = eval(args.Expression, rho)
		}
		return
	}

	switch e := e.(type) {
	case Value:
		r = e.(int)
	case Variable:
		if isBound(e, rho) {
			r = fetch(e, rho)
		} else if isBound(e, globalEnv) {
			r = fetch(e, globalEnv)
		} else {
			fmt.Printf("Undefined variable: %v\n", e)
			goto 99
		}
	case Application: 
		if e.Op > len(Builtins) {
			r = applyUserFun(e.Op, e.ExpressionList.Eval(rho))
		} else {
			op = primOp(e.Op)
		}
		if op in [IFOP .. BEGINOP] {
			r = applyCtrlOp(op, e.ExpressionList)
		} else {
			r = applyValueOp(op, e.ExpressionList.Eval(rho))
		}
	}
	return
}

(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

func main() {
	globalEnv = &Environment{}

	for quittingtime := false; !quittingtime; {
		func() {
			reader()
			if matches(pos, 4, 'quit                ') {
				quittingtime = true
			} else if (userinput[pos] = '(') && matches(skipblanks(pos + 1), 6, 'define              ') {
				fmt.Println(parseDef())
			} else {
				fmt.Printf("%v\n\n", prValue(eval(parseExp(), &Environment)))
			}
		}()
	}
}