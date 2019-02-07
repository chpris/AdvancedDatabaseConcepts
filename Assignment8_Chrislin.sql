CREATE DATABASE w6;
\c w6


\! echo "Q1 - PowerSet"


create table A(x int primary key);
insert into A values(1),(2),(3),(4);

create table SS(s int[]);

CREATE EXTENSION intarray;

/*
Recursive function superSetsOfSet() - 
Building subsets from table A using the given input
For example in a set A with 1,2,3,4
if input is 1
step 1: 1 is combined with each of 2,3,4
step 2: After 1 combines with 2 iteratively calls the function to combine with left elements 3,4 -> (1,2,3), (1,2,4) and repeat
*/

CREATE OR REPLACE FUNCTION superSetsOfSet(p int[])
returns setof SS
AS $$
DECLARE
	setA integer[];
	setDiff integer[];
	setI integer[];
	nSS integer;
	nP integer;
	nA integer;
	i integer;
	d int[];
BEGIN
	setA := (select array(select distinct x from A) as setA);
	--setU := select ARRAY(select unnest(x) from SS union select unnest(p)));
	setDiff := (select ARRAY(select unnest(setA) except select unnest(p)));
	nSS:= (select count(*) from SS);
	nP:= array_length(p,1);
	nA:= (select count(*) from A);
	IF (p<@ setA) THEN
		IF nSS = 0 THEN
			insert into SS VALUES (sort(p));
		end if;
		FOREACH i IN array setDiff
		LOOP
			setI := (select s from SS where s <@ (p || array[i]) and (p || array[i])<@ s);
			--RAISE NOTICE '%', array_length(setI,1);
			IF setI IS NULL THEN
				insert into SS VALUES (sort(p || array[i]));
				IF array_length(p || array[i],1) != nA THEN
					d:=superSetsOfSet((p || array[i])::int[]);
				end if;
			end if;
		END LOOP;
	END IF;
	--return True;
END;
$$ language plpgsql;

select * from superSetsOfSet('{}'::int[]);
SELECT * FROM SS;




----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



\! echo "Q2 - Connected by even and odd length"




create table Graph(source int, target int);
insert into Graph values (1,2),(2,3),(3,1);
create table ANC(s int, t int, l int);

/*
new_ANC_pairs(s integer, t integer, l integer):
Creates all such pairs that have ancestral child relations. A length attribute is given to each pair representing the path length between each pair.
Length gets incremented with the increase in edges connecting two nodes (ancestor and child).
*/

CREATE OR REPLACE FUNCTION new_ANC_pairs()
RETURNS TABLE (s integer, t integer, l integer) 
AS $$
select d.* from
((SELECT s, target, mod(l+1,2) FROM ANC, Graph WHERE t = source)
EXCEPT
(SELECT s, t, l FROM ANC))d;
$$ language sql;

/*
connectedByEvenLengthPath():
Inserts the edges from graph table into ANC with length = 1
Inserts all the new ancestor descendant pairs into ANC
Returns the pairs connected with even length
*/

\! echo "Q2 - Even length pairs"
CREATE OR REPLACE FUNCTION connectedByEvenLengthPath()
RETURNS TABLE (s integer, t integer) as
$$
DECLARE
	vertices int[];
	v int;
BEGIN
	vertices:= (SELECT ARRAY(select distinct source from Graph union select distinct target from Graph));
	DROP TABLE IF EXISTS ANC;
	CREATE TABLE ANC(s integer, t integer,l int);
	INSERT INTO ANC (SELECT source,target, 1 FROM Graph);
	
	foreach v in array vertices
	loop
		INSERT INTO ANC values (v,v,0);
	end loop;
	WHILE EXISTS (SELECT * FROM new_ANC_pairs())
	LOOP
		INSERT INTO ANC SELECT * FROM new_ANC_pairs();
	END LOOP;
	--return QUERY select * from ANC;
	return QUERY select distinct a.s, a.t from ANC a WHERE a.l=0;
END;
$$ language plpgsql;

/*
connectedByEvenOddPath():
Inserts the edges from graph table into ANC with length = 1
Inserts all the new ancestor descendant pairs into ANC
Returns the pairs connected with odd length
*/


select * from connectedByEvenLengthPath();

\! echo "Q2 - Odd length pairs"

CREATE OR REPLACE FUNCTION connectedByOddLengthPath()
RETURNS TABLE (s integer, t integer) as
$$
DECLARE
	vertices int[];
	v int;
BEGIN
	vertices:= (SELECT ARRAY(select distinct source from Graph union select distinct target from Graph));
	DROP TABLE IF EXISTS ANC;
	CREATE TABLE ANC(s integer, t integer,l int);
	INSERT INTO ANC (SELECT source,target, 1 FROM Graph);
	
	foreach v in array vertices
	loop
		INSERT INTO ANC values (v,v,0);
	end loop;
	WHILE EXISTS (SELECT * FROM new_ANC_pairs())
	LOOP
		INSERT INTO ANC SELECT * FROM new_ANC_pairs();
	END LOOP;
	--return QUERY select * from ANC;
	return QUERY select distinct a.s, a.t from ANC a WHERE a.l=1;
END;
$$ language plpgsql;

select * from connectedByOddLengthPath();


Drop table Graph;



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




\! echo "Q3 - Topographical Sort"



create table graphs(P int, C int);

insert into graphs values (1,2),(1,3),(2,3),(2,4),(3,7),(7,4),(4,5),(4,6),(7,6);
--insert into graphs values (5,0),(5,2),(2,3),(4,0),(4,1),(3,1);
--(4,6),(6,13);


create table sorted(v int);
create table queue(i int,v int);

/*
sort2(z int)
> Create an entry in sorted table for z node (sorted table keeps account of visited node)
> Select one of its adjacent vertex and call sort function recursively
> If one of the visited node does not have adjacent node, add it to queue table along with an index (# of vertices - # number of entries in queue)
> Once a nodes adjacent vertices are explored, add the vertex to queue table
*/

CREATE OR REPLACE FUNCTION sort2(z int)
RETURNS integer as
$$
DECLARE
	i int;
	ver int[];
	d int;
	x int;
	c int;
begin
	insert into sorted values(z);
	ver:= (select array (select graphs.C from graphs where graphs.P=z));
	c:= (select count(*) from (select distinct graphs.P from graphs union select distinct graphs.C from graphs)c);
	foreach i in array ver
	loop
		if not exists (select * from sorted where v = i) then
			d:= sort2(i);
		end if;
	end loop;
	x:= (select count(*) from queue);
	insert into queue values (c-(x),z);
	return x; 
end;
$$ language plpgsql;

/*
topologicalSort():
Set the first vertex as the vertex with no incoming degree and apply topographical sort
Display the queue table ordered by index
*/

CREATE OR REPLACE FUNCTION topologicalSort()
RETURNS setof queue as
$$
DECLARE
	f_node int[];
	q int;
	ver int[];
	j int;
	p int;
BEGIN
	p:= 0;
	f_node:= (select array (select distinct graphs.P from graphs except select distinct graphs.C from graphs));
	--c:= (select count(*) from (select distinct graphs.P from graphs union select distinct graphs.C from graphs)c);
	foreach j in array f_node
	loop
		p:= (select * from sort2(j));
	end loop;
	return query select * from queue order by i;
END;
$$ language plpgsql;


select * from topologicalSort();
--select * from queue order by i;



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





\! echo "Q4-Graph Coloring"

create table gp(S int, D int);
insert into gp values (1,2),(1,4),(2,3),(3,4),(4,5),(5,1),(3,5);
create table colors(color text[]);
insert into colors values ('{"red","blue","green"}');
create table vcolor(v int , c text);

/* is_safe(k int, co text) - to check if any of the adjacent vertices of "k" has color "co"
*/

CREATE OR REPLACE FUNCTION is_safe(k int, co text)
RETURNS boolean AS $$
DECLARE
	i int;
	n int;
	ver int[];
begin
	i:= 1;
	ver:= (select array (select distinct gp.S from gp union select distinct gp.D from gp) as vertices);
	n:= (select count(*) from (select distinct gp.S from gp union select distinct gp.D from gp)c);
	while i<=n
	loop
		if exists (select 1 from gp where (S = k and D = ver[i]) or (S=ver[i] and D=k)) then
			if exists (select 1 from vcolor where v = ver[i] and c = co) then
				return False;
			end if;
		end if;
		i:= i+1;
	end loop;
	return True;
END;
$$ language plpgsql;


/* threeColorable() - 
select first vertex from graph and check if it is safe to assign first color from colors to the node.
Repeat for other vertices. Assign a different color if the adjacent vertex has the similar color
Store the visited nodes and colors in table vcolor
In the end if # entries in v color table is equal to number of vertices, graph is 3 colorable
*/

CREATE OR REPLACE FUNCTION threeColorable()
RETURNS boolean AS $$
DECLARE
	ver int[];
	n int;
	c int;
	m int;
	colrs text[];
	d boolean;
	v int;
	col text;
	k int;
	vc int;
	t boolean;
begin
	ver:= (select array (select distinct gp.S from gp union select distinct gp.D from gp) as vertices);
	n:= (select count(*) from (select distinct gp.S from gp union select distinct gp.D from gp)c);
	c:= 1;
	colrs:= (select * from colors);
	k:= ((select count(*) from vcolor)+1);
	m:= (select count(*) from (select unnest(color) from colors)c);
	while c <= m
	loop
		v:= (ver[k]);
		col:= (select color[c] from colors);
		if is_safe(v,col) and not exists (select 1 from vcolor x where x.v = ver[k]) then
			insert into vcolor values(v, col);
			if (k+1 <= n) then
				d:= (threeColorable());
			else
				exit;
			end if;
		end if;
	c=c+1;
	end loop;
	vc:= (select count(*) from vcolor);
	t:= (vc=n);
 	return t;

END;
$$ language plpgsql;


select * from threeColorable();



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





\! echo "Q5-Hamiltonian Cycle"

/* hamilcycle() - Recursive algorithm to check the hamiltonian cycle path. 
Nodes are inserted to table Path with their repective positions to form a hamiltonian cycle
Function returns true if a hamiltonial cycle is possible from the starting vertex ver 

Algorithm: For the vertex v, create an entry in path table with position 1
check the adjacent vertices
Insert one of the first adjacent with an incremented position
Repeat until position of last entry in path table = # of total vertices
Return True
*/

create table ham(S int, D int);

insert into ham values (2,3),(3,4),(4,5),(5,1),(4,2),(2,5),(5,3),(1,3);

--insert into ham values (1,2),(2,3),(3,4),(4,5),(5,2),(5,2);

create table path(v int, no int);
create table p (h int, y int);
create table z (h int, y int);
CREATE OR REPLACE FUNCTION hamilcycle(ver int, num int)
RETURNS integer AS $$
DECLARE
	i int;
	n int;
	vers int[];
	d int;
	v1 int;
	f int;
begin
	n:= (select count(*) from (select distinct ham.S from ham union select distinct ham.D from ham)c);
	insert into path values (ver,num);
	v1:= (select v from path limit 1);
	vers:= (select array (select ham.D from ham where ham.S = ver));

	if array_length(vers,1)=0 then 
		return 0;	
	ELSIF num = n and exists (select 1 from ham where ham.S = ver and ham.D = v1) then
		return 1;		
	else
	 	f:= 0;
		foreach i in array vers
		loop
			if not exists (select v from path where v = i) then
				d:= hamilcycle(i, num+1);
				f:= 1;

				if d = 1 then
					RETURN 1; 
					exit;
				else
					delete from path where no = num+1;
				END IF;
			else
				continue;
			end if;
		end loop;
		return 0;
	end if;
END;
$$ language plpgsql;

/* Hamiltonian() - For loop that iterates over each vertex as first node and finds if hamiltonian cycle is possible from the respective vertex
Return true if possible from even one vertex of the graph. */

CREATE OR REPLACE FUNCTION Hamiltonian()
RETURNS boolean AS $$
DECLARE
	ver int[];
	n int;
	t int;
	v int;
	x int;
begin
	x:= 0;
	ver:= (select array (select distinct ham.S from ham union select distinct ham.D from ham) as vertices);
	--n:= (select count(*) from (select distinct ham.S from ham union select distinct ham.D from ham)c);
	foreach v in array ver
	loop
		t:= (select * from hamilcycle(v,1));
		if t=1 then
			x:= 1;
			exit;
		else
			Delete from path;
			continue;
		end if;
	end loop;
	if x=1 then
		return True;
	else 
		RETURN False;
	end if;
	return t;

END;
$$ language plpgsql;



select * from Hamiltonian();






----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




\! echo "Q6 - tfrequentset"




create table document(doc int, words text[]);
create table tset(word text[]);
--insert into tset values ('words');

insert into document values  (1, '{"A","B","C"}');
insert into document values  (2, '{"B","C","D"}');
insert into document values  (3, '{"A","E"}');
insert into document values  (4, '{"B","B","A","D"}');
insert into document values  (5, '{"E","F"}');
insert into document values  (5, '{"A","D","G"}');
insert into document values  (5, '{"C","B","A"}');
insert into document values  (5, '{"B","A"}');

\! echo "Displaying input"
select * from document;

/* sorting elements in an array */

CREATE FUNCTION sortingk(words text[])
     RETURNS text[] AS
     $$
          SELECT array(select distinct c.x from (select * from unnest(words) x order by unnest(words))c);
     $$  LANGUAGE SQL;




create table SS1 (word text[]);


/* function to create power set of words in input*/


CREaTE OR REPLACE FUNCTION powerSet(p text[])
returns integer
AS $$
DECLARE
	setA text[];
	setDiff text[];
	setI text[];
	nSS integer;
	nP integer;
	nA integer;
	i text;
	d int;
BEGIN
	setA:= (select array(select distinct unnest(words) from document));
	--setU := select ARRAY(select unnest(x) from SS union select unnest(p)));
	setDiff := (select ARRAY(select unnest(setA) except select unnest(p)));
	nSS:= (select count(*) from SS1);
	nP:= array_length(p,1);
	nA:= array_length(setA,1);
	IF (p<@ setA) THEN
		IF nSS = 0 THEN
			insert into SS1 (select * from sortingk(p));
		end if;
		FOREACH i IN array sortingk(setDiff)
		LOOP
			setI := (select word from SS1 where word <@ (p || array[i]) and (p || array[i])<@ word);
			--RAISE NOTICE '%', array_length(setI,1);
			IF setI IS NULL THEN
				insert into SS1 (select * from sortingk(p || array[i]));
				IF array_length(p || array[i],1) != nA THEN
					d:=powerSet((p || array[i])::text[]);
				end if;
			end if;
		END LOOP;
	END IF;
	return 1;
END;
$$ language plpgsql;

/* function to find all such sets from powerset that occus atleast t times in the input*/

CREATE OR REPLACE FUNCTION frequentSets(t int)
returns setof tset as
$$
DECLARE
	all_words text[];
	c int;
	we RECORD;
	d int[];
	e text[];
	x int;
BEGIN
	all_words:= (select array(select distinct unnest(words) from document));
	x:= (Select * from powerSet('{}'::text[]));
	for we in (select * from SS1)
	loop
		d:= (select array (SELECT doc FROM document d WHERE we.word <@ d.words));
		if array_length(d,1) >= t then
			insert into tset values (we.word);
		end if;
	end loop;
	return QUERY select * from tset;
END;
$$ language plpgsql;

\! echo "t = 2"
select * from frequentSets(2);





----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


\! echo "Q7 - KMeans"



create table dataSet(p int, x float(40), y float(40));

insert into dataSet values (1,2.3,3.4);
insert into dataSet values (3,3.3,6.4);
insert into dataSet values (5,2.1,-9.8);
insert into dataSet values (7,10.1,15.8);
insert into dataSet values (9,-2.1,-9.8);
insert into dataSet values (11,-10.1,15.8);
insert into dataSet values (13,-2.3,-3.4);
insert into dataSet values (15,-3.3,-6.4);
insert into dataSet values (17,-2.1,50.8);
insert into dataSet values (19,-10.1,10.8);
insert into dataSet values (21,35.1,1.8);
insert into dataSet values (23,0.1,-9.8);
select * from dataSet;

/* function to calculate distance */

CREATE FUNCTION distance(x1 FLOAT, y1 FLOAT, x2 FLOAT, y2 FLOAT)
     RETURNS FLOAT AS
     $$
          SELECT sqrt(power(x1-x2,2)+power(y1-y2,2));
     $$  LANGUAGE SQL;

create table labelling(p int, x float, y float, clus_num int);

/* kMeans(# of clusters(k), # of iterations(iter_num)) 
> Randomly assign k points in the data set as k centroid coordinates
> Repeat the following for 100 iterations:
> For each point in dataset, assign a cluster label such that it has minium distance from the respective cluster
> For eah cluster, find the average x and y coordinates and update the new x and y in the centroids table

> Throw the new values of centroid table
*/

\! echo "Stopping the loop by giving a max limit of iteration: 100"

CREATE OR REPLACE FUNCTION kMeans(k int, iter_num int)
returns TABLE (x float, y float) as
$$
DECLARE
	d RECORD;
	dist float;
	c int;
	updated_x float;
	updated_y float;
	means int[];
	iteration int;
	r int;
BEGIN
	iteration:= 1;
	r = 0;
	CREATE EXTENSION IF NOT EXISTS tsm_system_rows;
	create table centroids as select ds.x, ds.y, row_number() over () as clus_num from dataSet ds TABLESAMPLE SYSTEM_ROWS(k);
	while (iteration < iter_num)
	loop
		for d in SELECT * From dataSet
		loop
			insert into labelling select distinct c.p, c.x, c.y, c.clus_num from (select d.p, d.x, d.y, c.clus_num, distance(d.x,d.y,c.x,c.y) dist from centroids c)c inner join (select d.p, min(distance(d.x,d.y,c.x,c.y)) dista from centroids c group by d.p)x on c.p = x.p and c.dist = x.dista;--where distance(d.x,d.y,c.x,c.y) = (select min(distance(d.x,d.y,c.x,c.y)) from centroids c);
		end loop;
		means = (select array(select distinct clus_num from labelling));
		foreach c in array means
		loop
			updated_x = (select sum(l.x)/count(l.x) from labelling l where clus_num = c);
			updated_y = (select sum(l.y)/count(l.y) from labelling l where clus_num = c);
			update centroids set x = updated_x where clus_num = c;
			update centroids set y = updated_y where clus_num = c;
		end loop;
		delete from labelling;
		iteration = iteration+1;
	end loop;
	return QUERY select ce.x,ce.y from centroids ce;
END;
$$ language plpgsql;

\! echo "centroids"

select * from kMeans(3,100);







----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



\! echo "Q8 aggregatedWeight of parts"


create table partSubpart(pid int ,sid int,quantity int, primary key(pid,sid));
create table basicPart(pid int primary key,weight int);
insert into partSubpart values(1, 2, 4);
insert into partSubpart values(1, 3, 1);
insert into partSubpart values(3, 4, 1);
insert into partSubpart values(3, 5, 2);
insert into partSubpart values(3, 6, 3);
insert into partSubpart values(6, 7, 2);
insert into partSubpart values(6, 8, 3);
insert into basicPart values(2,5);
insert into basicPart values(4,50);
insert into basicPart values(5,3);
insert into basicPart values(7,6);
insert into basicPart values(8,10);

create table weights (wt int);
select * from partSubpart;


/* aggregatedWeight(p integer)
> if p is basic part, get the weight from basicPart table
> if p is not basic part, iteratively call the function for each of its basic and subpart and multiple qty of parts with its cost 
*/

CREATE OR REPLACE FUNCTION aggregatedWeight(p integer)
returns int as
$$
DECLARE
	s int;
	w int;
	sids int[];
	wt int;
	qt int;
	res int;
	p_wt int;
BEGIN
	if exists (select x.pid from partSubpart x where x.pid = p) then
		sids:= (select array(select distinct ps.sid from partSubpart ps where ps.pid = p));
		foreach s in array sids
		loop
			if s in (select distinct pid from basicPart) then
				wt = (select weight from basicPart where pid = s);
				qt = (select q.quantity from partSubpart q where q.sid = s);
				if exists (select 1 from partSubpart x where x.sid = p) then
					p_wt = (select x.quantity from partSubpart x where x.sid = p);
					insert into weights values (wt*qt*p_wt);
				else
					insert into weights values (wt*qt);
				end if;
			else
				w:= aggregatedWeight(s);
			end if;
		end loop;
	else
		insert into weights (select weight from basicPart where pid = p);
	end if;
	res:= (select sum(y.wt) from weights y);
	return res;
END;
$$ language plpgsql;

\! echo "weight of pid - 1"
select * from aggregatedweight(1);

create table part_wt(pid int, weight int);


/* showing weight for each pid */

CREATE OR REPLACE FUNCTION agg_weight()
RETURNS TABLE (part int, weight int) AS
$$
DECLARE
	i int;
	wt int;
	parts int[];
begin
	parts:= (select array(select pid from partSubpart union select pid from basicPart));
	foreach i in array parts
	loop
		delete from weights;
		wt:= (select * from aggregatedweight(i));
		insert into part_wt values(i, wt);
	end loop;
	return query (select * from part_wt w order by w.pid);
end;
$$ LANGUAGE plpgSQL;

select * from agg_weight();





----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


\! echo "Q9 Dijkstra"



create table graph(source int, target int, weight int);
/*
insert into graph values (0,1,2);
insert into graph values (1,0,2);
insert into graph values (0,4,10);
insert into graph values (4,0,10);
insert into graph values (1,3,3);
insert into graph values (3,1,3);
insert into graph values (1,4,7);
insert into graph values (4,1,7);
insert into graph values (2,3,4);
insert into graph values (3,2,4);
insert into graph values (3,4,5);
insert into graph values (4,3,5);
insert into graph values (4,2,6);
insert into graph values (2,4,6);*/

insert into graph values (0,1,2),(1,0,2),(0,4,10),(4,0,10),(1,3,3),(3,1,3),(1,4,7),(4,1,7),(2,3,4),(3,2,4),(3,4,5),(4,3,5),(4,2,6),(2,4,6);


create table paths(A int, D int, l int);

/* new_paths_pairs(x int) - to find all the children of node/ancestor x and its length from x. A node n will be paired 
twice with x if they can be reached with 2 different path length*/

CREATE OR REPLACE FUNCTION new_paths_pairs(x int)
RETURNS TABLE (A integer, D integer, l integer) 
AS $$
SELECT distinct c.A, p.target, c.l+p.weight as wt
FROM paths c, graph p 
WHERE c.D = p.Source and c.A = x and p.target != x
and not exists
(SELECT 1 FROM paths t where t.A = x and t.D = p.target AND t.l<=(c.l+p.weight));
$$ language sql;

/* Dijkstra(s int) - after finding distance of each node from s, return the minimum distance from each node*/

CREATE OR REPLACE FUNCTION Dijkstra(s int)
RETURNS TABLE (D integer, l integer) as
$$
BEGIN
	DROP TABLE IF EXISTS paths;
	CREATE TABLE paths(A integer, D integer,l int);
	INSERT INTO paths (SELECT source,target, weight FROM graph where source = s);
	while exists (SELECT * FROM new_paths_pairs(s))
	loop
		INSERT INTO paths SELECT * FROM new_paths_pairs(s);
	end loop;
	INSERT INTO paths values(s,s,0);
	return QUERY select a.target, a.distance from (SELECT a.A , a.D as "target", MIN(a.l) as "distance" from paths a group by a.A,a.D)a where a.A = s;
END;
$$ language plpgsql;

\! echo "Dijkstra";

select * from Dijkstra(0);









----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



\! echo "mapreduce"


\! echo "10 a - projection"

/* 
mapper - (a,a), (b,b), (a,a),....
grouper - (a, {a,a}), (b, {b}),....
reducer - (a,a), (b,b) 
*/


create table R(A int, B int);
insert into R values (1,2),(1,3),(4,5),(3,0),(4,9),(2,9),(2,6);


CREATE OR REPLACE FUNCTION mapper(A int)
RETURNS TABLE (A integer, pair integer) 
AS $$
SELECT A, A as pair;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION reducer(A int, pairs INTEGER[])
RETURNS TABLE(A int, pair INTEGER) 
AS $$ 
SELECT A, pairs[1];
$$ LANGUAGE SQL;

WITH 
map_output AS
(SELECT q.A, q.pair FROM R r, LATERAL(SELECT p.A, p.pair FROM mapper(r.A) p) q),
group_output AS
(SELECT p.A, array_agg(p.pair) as pairs FROM map_output p GROUP BY (p.A)),
reduce_output AS 
(SELECT t.A, t.pair FROM group_output r, LATERAL(SELECT s.A, s.pair FROM reducer(r.A, r.pairs) s) t)

\! echo "OUTPUT"

SELECT pair as "A" FROM reduce_output;



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




\! echo "10 b R-S"

/* 
mapper - (a,R1), (b,R1), (c,S1), (b,S1),...
grouper - (a, {R1}), (b, {R,S1}), (c, {S1}),....
reducer - (a,a)
result -> a
*/


create table R1(A int);
create table S1(A int);
insert into R1 values (1),(9),(0),(8),(100);
insert into S1 values (1),(0),(3),(8);

CREATE OR REPLACE FUNCTION mapper(x int, tab varchar(2))
RETURNS TABLE (A int, n varchar(2)) 
AS $$
SELECT x, tab;
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION reducer(A int, pairs varchar[])
RETURNS TABLE(A int, pair int) 
AS $$ 
SELECT A, A where '{"R1"}' <@ pairs and pairs <@ '{"R1"}';
$$ LANGUAGE SQL;

WITH 
map_output AS
(SELECT q.A, q.n FROM R1 r, LATERAL(SELECT p.A, p.n FROM mapper(r.A, 'R1') p)q UNION SELECT q.A, q.n FROM S1 r, LATERAL(SELECT p.A, p.n FROM mapper(r.A, 'S1') p)q),
group_output AS
(SELECT p.A, array_agg(p.n) as pairs FROM map_output p GROUP BY (p.A)),
reduce_output AS 
(SELECT t.A, t.pair FROM group_output r, LATERAL(SELECT s.A, s.pair FROM reducer(r.A, r.pairs) s) t)

SELECT pair FROM reduce_output;





----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




\! echo "10 c - join"

/* 
0 <- Relation R
1 <- Relation S
mapper - (b1,{0,a1}), (b1,{1,c1}), (b2,{0,a2}), (b2,{1,c1}),.....
grouper - (b1, {0,a1,1,c1}), (b2, {0,a2,1,c1}),....
reducer - (a1,b1,c1), (a2,b2,c1)
*/

--create table R(A int, B int);

--insert into R values (1,2),(1,3),(4,5),(3,0),(4,9),(2,9),(2,6);
create table S(B int, C int);

insert into S values (2,3),(3,4),(0,6),(9,6),(9,3),(2,6);

CREATE OR REPLACE FUNCTION mapper(x int, t int, y int)
RETURNS TABLE (B int, n int[]) 
AS $$
SELECT x, (array[t]||array[y]);
$$ LANGUAGE SQL;


\! echo "10 c - map-output"


WITH 
map AS
(SELECT q.B, q.n FROM R r, LATERAL(SELECT p.B, p.n FROM mapper(r.B, 0 ,r.A) p)q UNION SELECT q.B, q.n FROM S r, LATERAL(SELECT p.B, p.n FROM mapper(r.B, 1 , r.C) p)q)
select * from map;

create table map_output(B int, n int[]);
create table group_output(B int, p int[]);



insert into
map_output
(SELECT q.B, q.n FROM R r, LATERAL(SELECT p.B, p.n FROM mapper(r.B, 0 ,r.A) p)q UNION SELECT q.B, q.n FROM S r, LATERAL(SELECT p.B, p.n FROM mapper(r.B, 1 , r.C) p)q);






CREATE OR REPLACE FUNCTION grouping_p()
RETURNS TABLE(A int, pair int[]) AS $$
DECLARE
	r RECORD;
	s RECORD;
begin
	for r in (select * from map_output where n[1]=0)
	loop
		for s in (select * from map_output where n[1]=1)
		loop
			if s.B = r.B then
				insert into group_output values (r.B, r.n||s.n);
			end if;
		end loop;
	end loop;
END;
$$ language plpgsql;


select * from grouping_p();

\! echo "10 c - group-output"
select * from group_output;

create table reduce_output(a int, b int, c int);

CREATE OR REPLACE FUNCTION reducer()
RETURNS void 
AS $$ 
insert into reduce_output (SELECT p[2],b,p[4] from group_output);
$$ LANGUAGE SQL;


select * from reducer();

\! echo "10 c - reduce-output"
SELECT * FROM reduce_output;



\! echo "10 d - selection"

/* 
mapper - (a,a), (b,b), (a,a),....
grouper - (a, {a,a}), (b, {b}),....
reducer - (a,a), (b,b) 
*/

drop table R;
create table R(A int, B int, C int);
insert into R values (1,2,3),(1,3,6),(4,5,10),(3,0,0),(4,9,4),(2,9,9),(2,6,2);


CREATE OR REPLACE FUNCTION mapper1(A int,B int,C int)
RETURNS TABLE (A integer, pair integer []) 
AS $$
SELECT A, (array[B]||array[C]) as pair where B <6;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION reducer1(A int, pairs INTEGER[])
RETURNS TABLE(A int, B INTEGER, C INTEGER) 
AS $$ 
SELECT A, pairs[1] as B, pairs[2] as C;
$$ LANGUAGE SQL;

WITH 
map_output AS
(SELECT q.A, q.pair FROM R r, LATERAL(SELECT p.A, p.pair FROM mapper1(r.A,r.B,r.C) p) q),
reduce_output AS 
(SELECT t.A, t.B, t.C FROM map_output r, LATERAL(SELECT s.A, s.B, s.C FROM reducer1(r.A, r.pair) s) t)

\! echo "OUTPUT"

SELECT * FROM reduce_output;

--intersection

CREATE OR REPLACE FUNCTION mapper2(x int, tab varchar(2))
RETURNS TABLE (A int, n varchar(2)) 
AS $$
SELECT x, tab;
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION reducer2(A int, pairs varchar[])
RETURNS TABLE(A int, pair int) 
AS $$ 
SELECT A, A where '{"R1"}' <@ pairs and pairs <@ '{"R1"}';
$$ LANGUAGE SQL;

WITH 
map_output AS
(SELECT q.A, q.n FROM R1 r, LATERAL(SELECT p.A, p.n FROM mapper2(r.A, 'R1') p)q UNION SELECT q.A, q.n FROM S1 r, LATERAL(SELECT p.A, p.n FROM mapper(r.A, 'S1') p)q),
group_output AS
(SELECT p.A, array_agg(p.n) as pairs FROM map_output p GROUP BY (p.A)),
reduce_output AS 
(SELECT t.A, t.pair FROM group_output r, LATERAL(SELECT s.A, s.pair FROM reducer2(r.A, r.pairs) s) t)

SELECT pair FROM reduce_output;


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




\c postgres
DROP DATABASE w6;


