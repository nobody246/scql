# scql

I wrote this to check out what I can do with macros, and write MySQL queries in Chicken Scheme. This was before I heard about ssql (http://wiki.call-cc.org/eggref/4/ssql), which (**I think**) does something similar but I haven't tried it yet.

# syntax abbreviations
select-distinct, select, from , join, where, having, group-by , order-by, limit, insert, delete commands can respectively be abbreviated to
sel-distinct ,sel, fr, jo, wh, ha, gr-by or-by, lim, ins, del


# to use

**(include "scql.scm")**

# select

**(scql sel col fr tab)**

"select col from tab;"

**(scql sel (col1 col2 col3) fr tab)**

"select col1,col2,col3 from tab;"

**(scql sel (col1 col2 col3) fr (tab1 tab2 tab3))**

"select col1,col2,col3 from tab1,tab2,tab3;"

**(scql sel col1 fr tab1 wh (= tab1.id ?))**

"select col1 from tab1 where tab1.id = ?;"

**(scql sel col1 fr tab1 wh (= tab1.id 10 and > tab1.number ?))**

"select col1 from tab1  where tab1.id = 10 and tab1.number > ?;"

**(scql sel (tab1.id tab1.col1 tab2.id) fr tab1 join (left tab2) on ((= tab2.tab1_id tab1.id)))**

"select tab1.id,tab1.col1,tab2.id from tab1  left join tab2  on tab2.tab1_id = tab1.id  ;"

**(scql sel (tab1.id tab1.col1 tab2.id) fr tab1 join (left tab2) on ((= tab2.tab1_id tab1.id and > tab2.number ?)))**

"select tab1.id,tab1.col1,tab2.id from tab1  left join tab2  on tab2.tab1_id = tab1.id  and tab2.number > ?  ;"

**(scql sel (tab1.id tab1.col1 tab2.id) fr tab1 jo (left tab2 right tab3) on ((= tab2.tab1_id tab1.id and > tab2.number tab1.number)(= tab3.tab2_id tab2_id)) wh (= tab3.id ? and > tab2.number 100) gr-by (tab1.fname tab2.id_number) or-by (tab1.lname asc) limit (1 10000))**

"select tab1.id,tab1.col1,tab2.id from tab1 left join tab2  on tab2.tab1_id = tab1.id and tab2.number > tab1.number right join tab3  on tab3.tab2_id = tab2_id  where tab3.id = ? and tab2.number > 100 group by tab1.fname,tab2.id_number order by   tab1.lname asc  limit 1,10000 ;"

**(define _cols "col4 as num, col5")**

**(scql sel _cols fr table)**

"select col4 as num, col5 from table;"

**(define _tabs '(tab1 tab2 tab3 tab4 tab5))**

**(scql sel col fr _tabs)**

"select col from tab1,tab2,tab3,tab4,tab5;"

**(define sub-query1 (scql ->sel "SUM(tab2.aggregated_number)" fr tab2 wh (= tab2.id tab1.tab2_id) lim 1 as aggregated_number))**

**(scql sel (col1 col2 col3 col4 sub-query1) fr tab1 wh (> aggregated_number 100))**

"select col1,col2,col3,col4,(select SUM(tab2.aggregated_number) from tab2 where tab2.id = tab1.tab2_id limit 1 ) as aggregated_number  from tab1 where aggregated_number > 100;"



# insert select

**(scql sel (col1 col2 col3 col4) fr tab1 lim (10 100) ins (tab2 col1 col2 col3 col4))**

"insert into tab2 (col1,col2,col3,col4) select col1,col2,col3,col4 from tab1 limit 10,100 ;"

# update
**(scql upd (table col1 val1 col2 val2 col3 val3) wh (>= table.id 10 and = table.include 1))**

"update table set  col1 = val1, col2 = val2, col3 = val3  where table.id >= 10 and table.include = 1;"

# delete

**(scql del fr table wh (= id 10))**

"delete from table where id = 10;"

# insert

**(scql ins (table col1 val1 col2 val2 col3 val3))**

"insert into table (col1,col2,col3)  values(val1,val2,val3);"

# create temp table
**(scql cr-tmp tmp-table (col1 col2 col3 col4) fr tbl1 jo (left tbl2) on ( (= tbl1.id tbl2.id and > tbl1.value 1000)))**

"create temporary table tmp-table select col1,col2,col3,col4 from tbl1 left join tbl2  on tbl1.id = tbl2.id and tbl1.value > 1000;"



**Also to note:**
You can pass lists, strings, or numbers where applicable, literally or as variables. Procedures passed in scql clause will reference the symbol of that procedure's name and eventually convert to a string E.g, 
**(scql sel print from list)** 

will evaluate to 

**"select print from list;"**,

but


**(define x '(print non-procedure-symbol-name))**
**(define y '(list something-or-another))**
**(scql sel x from y)**

will evaluate to 

**"select print,non-procedure-symbol-name from list,something-or-another;"**.


# select distinct


**(scql sel-distinct (col1 (scql ->sel-distinct a fr b limit 1)) fr c)**


**"select distinct col1,(select distinct a from b limit 1 ) from c;"**
