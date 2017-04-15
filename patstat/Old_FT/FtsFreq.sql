SELECT display_term, count(*)
FROM sys.dm_fts_index_keywords_by_document(db_id('patstat2016a'), object_id('tls203_appln_abstr'))
group by display_term
order by count(*) desc
; 