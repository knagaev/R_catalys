
declare @term nvarchar(255)
set @term = 'white'

/*
select earliest_publn_year, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, @term) ab
inner join appln_years ay
on ay.appln_id = ab.[KEY]
group by earliest_publn_year
-- OPTION (JOIN)
 ;
 */
select earliest_publn_year, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, @term) ab
inner merge join appln_years ay
on ay.appln_id = ab.[KEY]
group by earliest_publn_year
-- OPTION (JOIN)
 ;
 
select earliest_publn_year, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, @term) ab
inner hash join appln_years ay
on ay.appln_id = ab.[KEY]
group by earliest_publn_year
-- OPTION (JOIN)
 ;
  
select earliest_publn_year, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, @term) ab
inner loop join appln_years ay
on ay.appln_id = ab.[KEY]
group by earliest_publn_year
-- OPTION (JOIN)
 ;

