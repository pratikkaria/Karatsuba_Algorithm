
fun intToChar(a) = if a = 1 then #"1"
              else if a = 2 then #"2"
               else if a = 3 then #"3"
               else if a = 4 then #"4"
               else if a = 5 then #"5"
               else if a = 6 then #"6"
               else if a = 7 then #"7"
               else if a = 8 then #"8"
               else if a = 9 then #"9"
               else #"0";

fun reverse_num(0,sum) = sum
|   reverse_num(a,sum) = reverse_num(a div 10, sum*10 + (a mod 10));


fun reverse_list([],out) = out
|   reverse_list(h::T,out) = reverse_list(T,h::out);



					     
fun len_list([]) = 0
|   len_list(h::T) = 1+len_list(T);

fun prepend(L, l ) = if l mod 4=0 then L else prepend(0::L,l+1);


fun out_list([],out,count,temp) = out
|   out_list(h::T,out,count,temp) = if (count mod 4)=0 andalso count>0 then out_list(T,(temp*10+h)::out,count+1,0) else out_list(T,out,count+1, (temp*10 + h));


fun  	StringtoDigit("") = []
|       StringtoDigit(L) = let
				val a =map (fn x=>x-48) (map Char.ord (explode(L)))
				val prepended = prepend(a,len_list(a))
			   in
			   	reverse_list(out_list(prepended,[],1,0),[])
			   end;
		       	      


			

fun   handler_Digit_to_String([],out) = out 
|     handler_Digit_to_String(a::b,out) = let
				   val ap = intToChar(a)
		       	       in 
		     		   handler_Digit_to_String(b,ap::out)
		       	       end;



		       	       
fun DigittoString([]) = ""
|   DigittoString(L) = implode(rev(handler_Digit_to_String(L,[])));




fun find_num_digits(num:int,count) = if num=0 then count else find_num_digits((num div 10),count+1);


fun find_power([],pow) = pow 
|  find_power(h::T,pow) = if find_num_digits(h,0)>pow then find_power(T,find_num_digits(h,0)) else find_power(T,pow);


fun pow_handler(a,0) = reverse_list(a,[])
|   pow_handler(a,m) = pow_handler(0::a,m-1);

fun pow(a,m) = pow_handler(reverse_list(a,[]),m);

fun splitList(T,0,out) = (reverse_list(out,[]),T) 
|splitList(h::T,len,out) = splitList(T,len-1,h::out);



fun len_list([]) = 0
|   len_list(h::T) = 1+len_list(T);



fun add_base_104([],[],carry,out) = if carry=1 then 1::out else out
|   add_base_104(h1::T1,h2::T2,carry,out) = let
						val temp = if(h1+h2+carry>=10000) then (h1+h2+carry - 10000) else (h1+h2+carry)
						val newc = if(h1+h2+carry >=10000) then 1 else 0
					    in
						add_base_104(T1,T2,newc,temp::out)
					    end;


fun add1([],[],carry) = if carry=1 then [1] else []
|   add1(h1::T1,h2::T2,carry) = if (h1+h2+carry)>=10000 then (h1+h2+carry-10000)::add1(T1,T2,1) else (h1+h2+carry)::add1(T1,T2,0);


fun make_list_equal([],[],out1,out2) = (out1,out2)
|   make_list_equal(h::T,[],out1,out2) = make_list_equal(T,[],h::out1,0::out2)
|   make_list_equal([],h::T,out1,out2) = make_list_equal([],T,0::out1,h::out2)
|   make_list_equal(h::T,h1::T1,out1,out2) = make_list_equal(T,T1, h::out1,h1::out2);



fun find_higher([],[],out1,out2,_) = (reverse_list(out1,[]),reverse_list(out2,[]))
|   find_higher(h::T,h1::T1,out1,out2,1) = find_higher(T,T1,h::out1,h1::out2,1)
|   find_higher(h::T,h1::T1,out1,out2,0) = if h=h1 then find_higher(T,T1,h::out1,h1::out2,0)
					   else
						if h>h1 then find_higher(T,T1,h::out1,h1::out2,1)
						else find_higher(T1,T,h1::out1,h::out2,1);

fun find_higher_wrapper(x1,x2) = find_higher(x1,x2,[],[],0);

					

fun subtract1([],_,borrow) = []
| subtract1(h1::l1,h2::l2,borrow) = if (h1-h2-borrow)>=0 then (h1-h2-borrow)::subtract1(l1,l2,0) else (h1-h2-borrow+10000)::subtract1(l1,l2,1);


fun add(x1,x2) = let
			val (a,b) = find_higher_wrapper(make_list_equal(reverse_list(x1,[]),reverse_list(x2,[]),[],[]))
		in
			reverse_list(add1(reverse_list(a,[]),reverse_list(b,[]),0),[])
		end;

fun subtract(x1,x2) = let
			val (a,b) = find_higher_wrapper(make_list_equal(reverse_list(x1,[]),reverse_list(x2,[]),[],[]))
			
		      in
			reverse_list(subtract1(reverse_list(a,[]),reverse_list(b,[]),0),[])
		      end;


fun decrement(x) = subtract(x,[1]);

						
									
fun find_values(L,flag) = if(flag=1) then hd(L) else find_values(tl(L),1);

fun find_sign ([],[],sign) = sign
|   find_sign (h1::T,h2::T1,sign) = if h1=h2 then find_sign(T,T1,sign)
				   else if h1>h2 then find_sign([],[],2)
				   else  find_sign([],[],1);



fun find_sign ([],[],sign) = sign
|   find_sign(T,[],sign) = find_sign([],[],1)
|   find_sign([],T,sign) = find_sign([],[],~1)
|   find_sign (h1::T,h2::T1,sign) = if h1=h2 then find_sign(T,T1,sign)
				   else if h1>h2 then find_sign([],[],1)
				   else  find_sign([],[],~1);


		 

fun out_list([],out,count,temp) = out
|   out_list(h::T,out,count,temp) = if (count mod 4)=0 andalso count>0 then out_list(T,(temp*10+h)::out,(count+1) mod 4,0) else out_list(T,out,(count+1), (temp*10 + h));

fun find_dig(0,out) = out
|  find_dig(a,out) = find_dig(a div 10, (a mod 10)::out);

fun append_zeros(b,len) = if len mod 4 = 0 then b else append_zeros(0::b,len+1);

fun divi(a) = let
		val b = find_dig(a,[])
		val c = append_zeros(b,len_list(b))
	      in 
		reverse_list(out_list(c,[],1,0),[])
		end;


fun   remove_extra([],out,flag) = if null(out) then [0] else reverse_list(out,[])
|     remove_extra(h::T,out,flag)= if flag=0 then if h=0 then remove_extra(T,out,0) else remove_extra(T,h::out,1) else remove_extra(T,h::out,1);

fun printList([]) = ()
|   printList([a]) = if a > 0 then (
		print(Int.toString(a));
		print("\n");
		printList([])
	) else printList([])
|   printList(h::T) = if h>0 then (
		print(Int.toString(h));
		print(" ");
		printList(T)
	) else printList(T);

fun karatsuba_handler([a],[b]) = if (a*b)>=10000 then divi(a*b) else [a*b]
|   karatsuba_handler(L1,L2) = let
						val (list1,list2) = make_list_equal(reverse_list(L1,[]),reverse_list(L2,[]),[],[])
						val max = len_list(list1);
						val ceil = Real.ceil(Real.fromInt(len_list(list1))/2.0)
						val m = max - ceil;
						val (a1,b1)=  splitList(list1, m , [])
						val (c1,d1)=  splitList(list2, m , [])

						val (a,b) = make_list_equal(reverse_list(a1,[]),reverse_list(b1,[]),[],[])
						val (c,d) = make_list_equal(reverse_list(c1,[]),reverse_list(d1,[]),[],[])
						val temp1 = remove_extra(karatsuba_handler(a,c),[],0)
						val temp2 = remove_extra(karatsuba_handler(b,d),[],0)
						val temp3 = subtract(a,b)
						val temp4 = subtract(c,d)
						val temp5 = remove_extra(karatsuba_handler(temp3,temp4),[],0)
						val sign1 = find_sign(b,a,0)
				 		val sign2 = find_sign(c,d,0)
						val z0 = remove_extra(temp2,[],0)
						val z2 = remove_extra(temp1,[],0)
						val temp_sign = sign1*sign2
						val z1 = if temp_sign=(~1) then remove_extra(subtract(add(temp1,temp2),temp5), [], 0) else remove_extra(add(add(temp1,temp2),temp5),[],0)
						(*val useless = (printList(z0); 2+3)*)
					in
						
						remove_extra(add(add(pow(z2,2*ceil), z0), pow(z1,ceil)),[],0)

					end;
fun  factorial_handler([1]) = [1]
|    factorial_handler(n)=  karatsuba_handler(n,factorial_handler(decrement(n)));



				
fun convert(0,out) = implode(out)
|   convert(a,out) = convert(a div 10, Char.chr((a mod 10)+48)::out);


fun output_trim([],_)=[]
|   output_trim(h::T,0) = if h= #"0" then output_trim(T,0) else h::output_trim(T,1)
|   output_trim(h::T,1) = h::output_trim(T,1);

fun toString([]) = ""
|   toString(h::T) = if h>=1000 then convert(h,[])^toString(T) else if h>=100 then "0"^convert(h,[])^toString(T) else if h>=10 then "00"^convert(h,[])^toString(T) else if h>0 then  "000"^convert(h,[])^toString(T) else "0000"^toString(T);


fun factorial(n) = let
				val inp = StringtoDigit(n)
				val fact = factorial_handler(inp)
				val output = toString(fact)
			   in
				implode(output_trim(explode(output),0))
			   end;
