update tls203_appln_abstr
set earliest_publn_year = 
(select earliest_publn_year from tls201_appln t1 where tls203_appln_abstr.appln_id = t1.appln_id);	