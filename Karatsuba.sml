(* 
Name - Pratik Karia
Entry Number - 2019MCS2568
Assignment Title - Factorial Using Karatsuba Algorithm
*)

exception Invalid_Input_exception;
(*---------------------------------------------Helper Functions--------------------------------------------*)

(* Takes input a number and reverses the number 
Signature - val reverse_num = fn : int * int -> int
Eg) Input = 123, sum = 0

    reverse_num(123,0) = reverse_num(12,0*10 + 3) = reverse_num(1,3*10 +  2) = reverse_num(0,32*10 + 1) 
    So, sum = 321 which is returned
*)
fun reverse_num(0,sum) = sum
|   reverse_num(a,sum) = reverse_num(a div 10, sum*10 + (a mod 10));

(*------------------------------------------------------------------*)


(* Takes input a list and returns a list[out] which is reversed 
Signature  - val reverse_list = fn : 'a list * 'a list -> 'a list 
Eg) Input = [1,2,3,4] , out = []
    reverse_list(1::[2,3,4] , [])-> reverse_list(2::[3,4],1::[])->reverse_list(3::[4],2::1::[]) -> reverse_list([4::[],3::2::1::[]) -> reverse_list([],[4::3::2::1])
    So, out = [4,3,2,1] which is returned
*)
fun reverse_list([],out) = out
|   reverse_list(h::T,out) = reverse_list(T,h::out);
(*------------------------------------------------------------------*)


(* Takes input a list and returns the length of the list 
Signature - val len_list = fn : 'a list -> int
Eg) Input = [1,2,3,4]
    len_list(1::[2,3,4]) = 1+ len_list(2:[3,4]) = 1+1+ len_list(3::[4]) = 1+1+1+ len_list(4::[]) = 1+1+1+1+0 = 4
    So, 4 is returned
*)
fun len_list([]) = 0
|   len_list(h::T) = 1+len_list(T);
(*------------------------------------------------------------------*)


(* Takes input a list as input and the length of the list and prepends 0s till the length of the list is divisible by 4
Signature - val prepend = fn : int list * int -> int list
Eg) L = [1] , len = 1
    prepend([1],1) -> prepend(0::[1],2) -> prepend(0::0::[1],3) -> prepend(0::0::0::[1],4) -> if condition satisfies and we return L
    So, L = [0,0,0,1] which is returned
*)
fun prepend(L, len ) = if len mod 4=0 then L else prepend(0::L,len+1);
(*------------------------------------------------------------------*)


(* Takes iput a list of digits, out(blank list) , count(initial = 1), temp(initial = 0)
Signature - val out_list = fn : int list * int list * int * int -> int list
Eg) out_list(1::[2,3,4,5,6],[],1,0) -> out_list(2::[3,4,5,6],[],2,0*10+1) -> out_list(3::[4,5,6],[],3,1*10+2) -> out_list(4::[5,6],[],4,12*10+3)
    -> out_list(5::[6],1234::[],5,0) -> out_list(6::[],1234::[],6,0*10+5) -> out_list([],1234::[],7,5*10+6) -> (56::1234::[])
    So, [56,1234] is returned which is later reversed during its usage to get [1234,56]
*)
fun out_list([],out,count,temp) = if temp>0 then temp::out else out
|   out_list(h::T,out,count,temp) = if (count mod 4)=0 andalso count>0 then out_list(T,(temp*10+h)::out,count+1,0) else out_list(T,out,count+1, (temp*10 + h));
(*------------------------------------------------------------------*)

(* Takes input a number and finds number of digits in the number (num -> number, count-> number of digits(initially sent as 0)
Signature - val find_num_digits = fn : int * int -> int
Eg) find_num_digits(1234,0) -> find_num_digits(123,1) -> find_num_digits(12,2) -> find_num_digits(1,3) -> find_num_digits(0,4) -> 4 is returned
    So, 4 is returned as output of this function
*)
fun find_num_digits(num:int,count) = if num=0 then count else find_num_digits((num div 10),count+1);
(*------------------------------------------------------------------*)


(* Takes 2 lists of integers as inputs and makes 2 lists of same size by prepending 0s to it and returns out1,out2 which are initially []
Signature - val make_list_equal = fn : int list * int list * int list * int list -> int list * int list
Eg) make_list_equal(12::[34,56],48::[],[],[])-> make_list_equal(34::[56],[],12::[],48::[]) ->make_list_equal(56::[],[],34::12::[],0::48::[])
    -> make_list_equal([],[],56::34::12::[],0::0::48::[])
    
    So, out1 = [56,34,12],out2 = [0,0,48] is returned which is later reversed during its usage
*)
fun make_list_equal([],[],out1,out2) = (out1,out2)
|   make_list_equal(h::T,[],out1,out2) = make_list_equal(T,[],h::out1,0::out2)
|   make_list_equal([],h::T,out1,out2) = make_list_equal([],T,0::out1,h::out2)
|   make_list_equal(h::T,h1::T1,out1,out2) = make_list_equal(T,T1, h::out1,h1::out2);
(*------------------------------------------------------------------*)


(*
1) find_higher(L1,L2,out1,out2,0) -> The handler function used to return the integer list that represents a greater number (always takes integer list
	 			    of equal length using make list equal function.
Signature - val find_higher = fn : int list * int list * int list * int list * int -> int list * int list	 			    
2) find_higher_wrapper(x1,x2) -> The function that calls find_higher and returns necessary outputs.
Signature - val find_higher_wrapper = fn : int list * int list -> int list * int list

Eg) find_higher([12,34],[0,48],[],[],0) -> find_higher([34::[],48::[],12::[],0::[],1)-> find_higher([],[],[34,12],[48,0],_) -> returns reverse of out1=[34,12] and
    out2 = [48,0] where 1st one (out1 here) is the greater list and second one is smaller. 
*)

fun find_higher([],[],out1,out2,_) = (reverse_list(out1,[]),reverse_list(out2,[]))
|   find_higher(h::T,h1::T1,out1,out2,1) = find_higher(T,T1,h::out1,h1::out2,1)
|   find_higher(h::T,h1::T1,out1,out2,0) = if h=h1 then find_higher(T,T1,h::out1,h1::out2,0)
					   else
						if h>h1 then find_higher(T,T1,h::out1,h1::out2,1)
						else find_higher(T1,T,h1::out1,h::out2,1);

fun find_higher_wrapper(x1,x2) = find_higher(x1,x2,[],[],0);
(*------------------------------------------------------------------*)


(* Takes a list and splits it into 2 parts where len is 1st first part and T is second part.
Signature - val splitList = fn : 'a list * int * 'a list -> 'a list * 'a list
Eg) splitList([1,2,3,4],2,[]) -> splitList([2,3,4],1,[1]) -> splitList([3,4],0,[2,1]) -> reverse([2,1]),[3,4] is returned 
    So, [1,2] and [3,4] are returned.
*)

fun splitList(T,0,out) = (reverse_list(out,[]),T) 
|splitList(h::T,len,out) = splitList(T,len-1,h::out);
(*------------------------------------------------------------------*)
(* Takes a integer list of integers as input and removes all the zeros from beginning of list 
Signature - val remove_extra = fn : int list * int list * int -> int list
Eg) remove_extra([0,0,0,0,46,1234],[],0)-> remove_extra([0,0,0,46,1234],[],0)->remove_extra([0,0,46,1234],[],0) -> remove_extra([0,46,1234],[],0)
    -> remove_extra([0,46,1234],[],0) -> remove_extra([46,1234],[],0) -> remove_extra([1234],[46],1) -> remove_extra([],[1234,46],1) and reverse
       of ([1234,46]) -> ([46,1234]) is returned
*)
fun   remove_extra([],out,flag) = if null(out) then [0] else reverse_list(out,[])
|     remove_extra(h::T,out,flag)= if flag=0 then if h=0 then remove_extra(T,out,0) else remove_extra(T,h::out,1) else remove_extra(T,h::out,1);
(*------------------------------------------------------------------*)


(* This function is used in Karatsuba Algorithm to do multiplication z2 with B^(2*m) and z0 with B^(m). To multiply 10^m with a number all that is 
   needed to be done is append the number with m zeros on the right hand side.
1) pow_handler(a,m) ->  Appends m zeros at end of a. a is the reversed list of the original required list.
Signature - val pow_handler = fn : int list * int -> int list
2) pow(a,m) ->  Power function to call the handler
Signature - val pow = fn : int list * int -> int list 
Eg) Suppose we need [34,12]*(10^4)^2. (here [34,12] is actually [0034,0012] as it is base 10^4. B=10^4)
pow_handler([12,34],2)-> pow_handler([0,12,34],1) -> pow_handler([0,0,12,34],0) so reverse([0,0,12,34]) -> [34,12,0,0] is returned 
which actually represents the number [0034,0012,0000,0000]
*)
fun pow_handler(a,0) = reverse_list(a,[])
|   pow_handler(a,m) = pow_handler(0::a,m-1);

fun pow(a,m) = pow_handler(reverse_list(a,[]),m);
(*------------------------------------------------------------------*)
(*
1) add_handler -> Handler function to compute sum of 2 integer lists.
Signature - val add_handler = fn : int list * int list * int -> int list
2) add -> Main add function that calls the handler function
Signature - val add = fn : int list * int list -> int list
Eg) Suppose we want to add [12,23] (number - 120023)  with [13,14] (number = 130014) we send [23,12] and [14,13] in add_handler
   add_handler([23,12],[14,13],0) -> 37::([12],[13],0) [Here as base is 10000 so we get a carry only if sum of 2 elements on integer list is >=10000] 
   -> 25::37::add_handler([],[],0) -> [25,37] which is reversed at the calling function add to get the result [37,25] which represents the number 
   370025.
*)

fun add_handler([],_,carry) = if carry=1 then [1] else []
|   add_handler(_,[],carry) = if carry=1 then [1] else []
|   add_handler(h1::T1,h2::T2,carry) = if (h1+h2+carry)>=10000 then (h1+h2+carry-10000)::add_handler(T1,T2,1) else (h1+h2+carry)::add_handler(T1,T2,0);

fun add(x1,x2) = let
			val (a,b) = find_higher_wrapper(make_list_equal(reverse_list(x1,[]),reverse_list(x2,[]),[],[]))
		in
			reverse_list(add_handler(reverse_list(a,[]),reverse_list(b,[]),0),[])
		end;
(*------------------------------------------------------------------*)


(*
1) subtract_handler - Handler function to compute different of 2 integer lists where first list is always greater than second list.
                      This is done by find_higher_wrapper function called in subtract.
Signature - val subtract_handler = fn : int list * int list * int -> int list                 
2) subtract - Main subtract function that calls the handler function
val subtract = fn : int list * int list -> int list
Eg) Suppose we want to subtract 340012 from 100010 then we pass [12,34] and [10,10] (reversed list) in subtract function that calls the handler
subtract_handler([34,12],[10,10],0) -> 24::subtract_handler([12],[10],0) -> 2::24::subtract_handler([],[],0) -> [2,24] which is reversed at the
calling function subtract to get the result [24,2] which represents the number 240002.
*)

fun subtract_handler([],_,_) = []
fun subtract_handler(_,[],_) = []
| subtract_handler(h1::l1,h2::l2,borrow) = if (h1-h2-borrow)>=0 then (h1-h2-borrow)::subtract_handler(l1,l2,0) else (h1-h2-borrow+10000)::subtract_handler(l1,l2,1);

fun subtract(x1,x2) = let
			val (a,b) = find_higher_wrapper(make_list_equal(reverse_list(x1,[]),reverse_list(x2,[]),[],[]))
			
		      in
			reverse_list(subtract_handler(reverse_list(a,[]),reverse_list(b,[]),0),[])
		      end;
		   
(*------------------------------------------------------------------*)


(* Takes input a list of integers that represent a number and decrements it by 1 and returns a list. This function is uses the subtract function 
   and is used in factorial function.
   Signature - val decrement = fn : int list -> int list
   Eg) decrement([1,998]) -> [1,997] is returned by the function
  *)

fun decrement(x) = subtract(x,[1]);		
(*------------------------------------------------------------------*)


(* Takes 2 list and returns 1 if subtraction of both is positive else returns -1. It checks this by just checking which of the 2 integer list represents
   a higher number.
   Signature - val find_sign = fn : int list * int list * int -> int [initially sign = 0]
   Eg) find_sign([1,12,34],[1,10,32],0) -> find_sign([12,34],[10,32]) -> find_sign([],[],1) -> returns sign = 1 as output
   - This function is used in Karatsuba Algorithm
*)

fun find_sign ([],[],sign) = sign
|   find_sign(T,[],sign) = find_sign([],[],1)
|   find_sign([],T,sign) = find_sign([],[],~1)
|   find_sign (h1::T,h2::T1,sign) = if h1=h2 then find_sign(T,T1,sign)
				   else if h1>h2 then find_sign([],[],1)
				   else  find_sign([],[],~1);
(*------------------------------------------------------------------*)



(* Takes a number as input and converts it into list of digits
Signature - val find_dig = fn : int * int list -> int list
Eg) find_dig(1234,[]) = find_dig(123,[4]) ->find_dig(12,[3,4]) -> find_dig(1,[2,3,4]) -> find_dig([],[1,2,3,4]).
    So [1,2,3,4] is returned to calling function.
*)
fun find_dig(0,out) = out
|  find_dig(a,out) = find_dig(a div 10, (a mod 10)::out);
(*------------------------------------------------------------------*)

(* Takes a list of digits as input and length of that list and appends 0 till the length of list is divisible by 4 and we are taking base as 10^4 (0-9999).
   Signature - val append_zeros = fn : int list * int -> int list
   Eg) append_zeros([1,2,3],3)->append_zeros([0,1,2,3],4) -> returns [0,1,2,3] as output
*)
fun append_zeros(b,len) = if len mod 4 = 0 then b else append_zeros(0::b,len+1);
(*------------------------------------------------------------------*)


(* Takes a number >=10000 and divides it in groups of integers each <10000.
Signature - val divi = fn : int -> int list
Eg) divi(123456) -> Returns [12,3456]
*)
fun divi(a) = let
		val b = find_dig(a,[])
		val c = append_zeros(b,len_list(b))
	      in 
		reverse_list(out_list(c,[],1,0),[])
		end;
(*------------------------------------------------------------------*)
(* Takes input a character list and returns 1 if one of them is not a number else returns 0 if it a valid list
Signature - 

*)
fun check_for_exception([]) = 0
|   check_for_exception([#"a"]) = raise Invalid_Input_exception
|   check_for_exception(h::T) = if Char.ord(h)>=48 andalso Char.ord(h)<=57 then check_for_exception(T) else check_for_exception([#"a"]);


(*------------------------------------------------------------------*)


(* Takes input a character list and and removes the zeros occuring at the start of character list
Signature - val output_trim = fn : char list * int -> char list
Eg) output_trim([#"0",#"0",#"1",#"2"],0) -> [#"1",#"2"]
*)
fun output_trim([],_)=[]
|   output_trim(h::T,f) = if f>1 then h::output_trim(T,1) else if f=1 then h::output_trim(T,1) else if h= #"0" then output_trim(T,0) else h::output_trim(T,1)
(*------------------------------------------------------------------*)


(*
1) convert(a,out) -> Takes an integer as input and converts it to string. It is used in toString Function
Signature - val convert = fn : int * char list -> string
2) toString(L) -> Given an integer list it returns a string representing the integers in integer list
Signature - val toString = fn : int list -> string
Eg) toString([12,34,56]) -> "001200340056" is returned. Furthur output_trim function is used to remove first 2 zeros that are extra by exploding the list
    and giving it to the output_trim function and then imploding the output of output_trim function.
*)

fun convert(0,out) = implode(out)
|   convert(a,out) = convert(a div 10, Char.chr((a mod 10)+48)::out);


fun toString([]) = ""
|   toString(h::T) = if h>=1000 then convert(h,[])^toString(T) else if h>=100 then "0"^convert(h,[])^toString(T) else if h>=10 then "00"^convert(h,[])^toString(T) else if h>0 then  "000"^convert(h,[])^toString(T) else "0000"^toString(T);
(*------------------------------------------------------------------*)


(* Takes input a string and converts it to list of integers where each integer is of base 10^4
   Signature - val fromString = fn : string -> int list
   Eg) fromList("12345") -> [1,2345] -> Output which is returned to the calling function
*)
fun  	fromString("") = []
|       fromString(L) = let
				val a =map (fn x=>x-48) (map Char.ord (explode(L)))
				val prepended = prepend(a,len_list(a))
			   in
			   	reverse_list(out_list(prepended,[],1,0),[])
			   end;

(*---------------------------------------------Karatsuba Algorithm--------------------------------------------*)



(* This is the implementation of karatsuba algorithm using helper functions. It is implement in the following way:
1) karatsuba L1 L2 - It recursively implements karatsuba algorithm
Signature - val karatsuba = fn : int list -> int list -> int list
Eg) karatsuba [12,12] [13,13] -> [156,312,156] . 
   The actual multiplication that is happening is 120012*130013 = 15603120156
*)
fun karatsuba [a] [b] = if (a*b)>=10000 then divi(a*b) else [a*b]
|   karatsuba L1 L2  = let
						val (list1,list2) = make_list_equal(reverse_list(L1,[]),reverse_list(L2,[]),[],[])
						val max = len_list(list1)
						val ceil = (max+1) div 2
						val m = max - ceil;
						val (a1,b1)=  splitList(list1, m , [])
						val (c1,d1)=  splitList(list2, m , [])
						val (a,b) = make_list_equal(reverse_list(a1,[]),reverse_list(b1,[]),[],[])
						val (c,d) = make_list_equal(reverse_list(c1,[]),reverse_list(d1,[]),[],[])
						val temp1 = remove_extra(karatsuba a c,[],0)
						val temp2 = remove_extra(karatsuba b d,[],0)
						val temp3 = subtract(a,b)
						val temp4 = subtract(c,d)
						val temp5 = remove_extra(karatsuba temp3 temp4,[],0)
						val sign1 = find_sign(b,a,0)
				 		val sign2 = find_sign(c,d,0)
						val z0 = remove_extra(temp2,[],0)
						val z2 = remove_extra(temp1,[],0)
						val temp_sign = sign1*sign2
						val z1 = if temp_sign=(~1) then remove_extra(subtract(add(temp1,temp2),temp5), [], 0) else remove_extra(add(add(temp1,temp2),temp5),[],0)
					in
						
						remove_extra(add(add(pow(z2,2*ceil), z0), pow(z1,ceil)),[],0)

					end;



(*---------------------------------------------Factorial Implementation--------------------------------------------*)



(* This is an implementation of factorial using Karatsuba Algorithm:-
1) factorial_handler(L) - Takes an integer list and passes it to karatsuba function along with recursive factorial_handler function call with 
   decremented value of L as parameter
   Signature - val factorial_handler = fn : int list -> int list
2) factorial - The main function of the program that is run with input as string. This function calls fromString to convert String to integer list
   then it calls factorial handler to get the factorial of input number and finally converts the output of factorial handler to string and returns
   it as output.
   Signature - val factorial = fn : string -> string
   
   Eg) factorial("5") = "120"
*)
fun  factorial_handler([1]) = [1]
|    factorial_handler(n)=let
				val prev_fact = factorial_handler(decrement(n))
			  in
				karatsuba n prev_fact
			  end;


fun   factorial(n) = let
				val check = if check_for_exception(explode(n)) = 1 then 1 else 0
				val inp = fromString(n)
				val fact = factorial_handler(inp)
				val output = toString(fact)
			   in
				implode(output_trim(explode(output),0))
			   end;
		
(*------------------------------------------------------------------*)
