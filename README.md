# scql

A Macro to Write Select/Update/Delete/Insert/Create Temp Table MySQL queries directly in Scheme. It allows me to write the SQL queries I most often use in a quick fashion, without losing syntax highlighting and with compile time checking of missing or malformed clauses. Only parameterized queries will work, as SQL strings are generated at compilation.

I'm **not** necessarily recommending anybody actually use it, it's still rough.


#to include  

(include "scql.scm")

# select

(scql sel col fr tab)

"select col from tab  ;"

(scql sel (col1 col2 col3) fr tab)

"select col1,col2,col3 from tab  ;"

(scql sel (col1 col2 col3) fr (tab1 tab2 tab3))

"select col1,col2,col3 from tab1,tab2,tab3  ;"

(scql sel col1 fr tab1 wh (= tab1.id ?))

"select col1 from tab1   where tab1.id = ? ;"

(scql sel col1 fr tab1 wh (= tab1.id 10 and > tab1.number ?)) 

"select col1 from tab1     where tab1.id = 10  and  tab1.number > ? ;"

(scql sel (tab1.id tab1.col1 tab2.id) fr tab1 join (left tab2) on ((= tab2.tab1_id tab1.id)))

"select tab1.id,tab1.col1,tab2.id from tab1  left join tab2  on tab2.tab1_id = tab1.id   ;"

(scql sel (tab1.id tab1.col1 tab2.id) fr tab1 join (left tab2) on ((= tab2.tab1_id tab1.id and > tab2.number ?)))

"select tab1.id,tab1.col1,tab2.id from tab1  left join tab2  on tab2.tab1_id = tab1.id  and tab2.number > ?   ;"

(define _cols "col1,col2,col3,col4 as num, col5") 

(scql sel _cols fr table)

"select col1,col2,col3,col4 as num, col5 from table  ;"

(define _tabs '(tab1 tab2 tab3 tab4 tab5)) 

(scql sel col fr _tabs)

"select col from tab1,tab2,tab3,tab4,tab5  ;"

(define sub-query1 (scql ->sel "SUM(tab2.aggregated_number)" fr tab2 wh (= tab2.id tab1.tab2_id) lim 1 as aggregated_number)) 

(scql sel (col1 col2 col3 col4 sub-query1) fr tab1 wh (> aggregated_number 100))

"select col1,col2,col3,col4,(select SUM(tab2.aggregated_number) from tab2   where tab2.id = tab1.tab2_id limit 1 )as aggregated_number  from tab1   where aggregated_number > 100 ;"

# insert select

(scql sel (col1 col2 col3 col4) fr tab1 lim (10 100) ins (tab2 col1 col2 col3 col4))

"insert into tab2 (col1,col2,col3,col4) select col1,col2,col3,col4 from tab1  limit 10,100 ;"

# update
(scql upd (table col1 val1 col2 val2 col3 val3) wh (>= table.id 10 and = table.include 1))

"update table  set   col1 = val1 , col2 = val2 , col3 = val3     where table.id >= 10  and  table.include = 1 ;"

# delete

(scql del fr table wh (= id 10))

"delete from table where id = 10 ;"

# insert

(scql ins (table col1 val1 col2 val2 col3 val3))

"insert into table (col1,col2,col3)  values(val1,val2,val3);"

# create temp table
(scql cr-tmp tmp-table (col1 col2 col3 col4) fr tbl1 jo (left tbl2) on ( (= tbl1.id tbl2.id and > tbl1.value 1000)))

"create temporary table tmp-table select col1,col2,col3,col4 from tbl1  left join tbl2  on tbl1.id = tbl2.id  and tbl1.value > 1000   ; "

