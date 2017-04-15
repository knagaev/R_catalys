/*select earliest_publn_year, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, ' compatible') ab
inner loop join appln_search ay
on ay.appln_id = ab.[KEY]
group by earliest_publn_year
order by count(*) desc
-- OPTION (JOIN)
 ;
 */
select earliest_publn_year, appln_auth, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, 'solution') ab
inner loop join appln_search ay
on ay.appln_id = ab.[KEY]
--where earliest_publn_year in ('2011', '1996', '2006')
group by earliest_publn_year, appln_auth
order by count(*) desc
-- OPTION (JOIN)
 ;

select appln_auth, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, 'decision') ab
inner loop join appln_search ay
on ay.appln_id = ab.[KEY]
--where earliest_publn_year in ('2011', '1996', '2006')
group by appln_auth
order by count(*) desc
-- OPTION (JOIN)
 ;

