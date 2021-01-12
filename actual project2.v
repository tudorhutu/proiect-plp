(* Since the variable names are now strings, we need to import the required libraries *)
Require Import Strings.String.
Local Open Scope string_scope.
Local Open Scope list_scope.
Scheme Equality for string.

(* ErrorNat encapsulates the constructor error_nat 
   which is useful in the case of arithmetic operations like division by 0*)
Inductive ErrorNat :=
  | error_nat : ErrorNat
  | num : nat -> ErrorNat.

Inductive ErrorBool :=
  | error_bool : ErrorBool
  | boolean : bool -> ErrorBool.

Inductive ErrorString :=
  | error_string : ErrorString
  | str : string -> ErrorString.

Coercion num: nat >-> ErrorNat.
Coercion boolean: bool >-> ErrorBool.
Coercion str: string >-> ErrorString.

(* Section for AExp *)
Inductive AExp :=
  | avar: string -> AExp (* Var ~> string *)
  | anum: ErrorNat -> AExp
  | aplus: AExp -> AExp -> AExp
  | asub: AExp -> AExp -> AExp
  | amul: AExp -> AExp -> AExp (* Multiplication *)
  | adiv: AExp -> AExp -> AExp (* Division *)
  | amod: AExp -> AExp -> AExp. (* Modulo *)

(* Section for BExp *)
Inductive BExp :=
  | berror
  | btrue
  | bfalse
  | bvar: string -> BExp
  | blt : AExp -> AExp -> BExp
  | bnot : BExp -> BExp
  | band : BExp -> BExp -> BExp
  | bor : BExp -> BExp -> BExp.

(* Section for SExp *)(*pending name*)
Inductive SExp :=
  | stvar: string -> SExp.


(* Section for Statements *)
Inductive Stmt :=
  | nat_decl: string -> AExp -> Stmt (* Declaration Stmt for variables of type nat *)
  | bool_decl: string -> BExp -> Stmt (* Declaration Stmt for variables of type bool *)
  | str_decl: string -> SExp -> Stmt (* Declaration Stmt for variables of type string *)
  | nat_assign : string -> AExp -> Stmt (* Assignment Stmt for variables of type nat *)
  | bool_assign : string -> BExp -> Stmt (* Assignment Stmt for variables of type bool *)
  | str_assign : string -> SExp -> Stmt (* Assignment Stmt for variables of type string *)
  | sequence : Stmt -> Stmt -> Stmt
  | while : BExp -> Stmt -> Stmt
  | ifthenelse : BExp -> Stmt -> Stmt -> Stmt
  | ifthen : BExp -> Stmt -> Stmt.

(* Section for notations *)

(* Notations used for arithmetic operations *)
Notation "A +' B" := (aplus A B)(at level 50, left associativity).
Notation "A ++'" := (aplus A 1)(at level 50, left associativity).
Notation "A -' B" := (asub A B)(at level 50, left associativity).
Notation "A *' B" := (amul A B)(at level 48, left associativity).
Notation "A /' B" := (adiv A B)(at level 48, left associativity).
Notation "A %' B" := (amod A B)(at level 45, left associativity).

(* Notations used for boolean operations *)
Notation "A <' B" := (blt A B) (at level 70).
Notation "!' A" := (bnot A)(at level 51, left associativity).
Notation "A &&' B" := (band A B)(at level 52, left associativity).
Notation "A ||' B" := (bor A B)(at level 53, left associativity).

(* Notations for Statements *)
Notation "X :n= A" := (nat_assign X A)(at level 90).
Notation "X :b= A" := (bool_assign X A)(at level 90).
Notation "X :s= A" := (str_assign X A)(at level 90).
Notation "'iNat' X ::= A" := (nat_decl X A)(at level 90).
Notation "'iBool' X ::= A" := (bool_decl X A)(at level 90).
Notation "'iStr' X ::= A" := (str_decl X A)(at level 90).
Notation "S1 ;; S2" := (sequence S1 S2) (at level 93, right associativity).
Notation "'fors' ( A ~ B ~ C ) { S }" := (A ;; while B ( S ;; C )) (at level 97).

(* Reserved Notations *)
Reserved Notation "A =[ S ]=> N" (at level 60).
Reserved Notation "B ={ S }=> B'" (at level 70).
Reserved Notation "B ={ S }=> B'" (at level 70).
Reserved Notation "S -{ Sigma }-> Sigma'" (at level 60).

(* Coercions for numerical and string constants and variables *)
Coercion anum: ErrorNat >-> AExp.
Coercion avar: string >-> AExp. (* Var ~> string *)
Coercion stvar: string >-> SExp.

Definition while_stmt :=
    iNat "i" ::= 0 ;;
    iNat "sum" ::= 0 ;;
    iStr "s" ::= "test";;
    while 
        ("i" <' 6) 
        (
           "sum" :n= "sum" +' "i" ;;
           "i" :n= "i" ++' 
        ).

Check while_stmt.
