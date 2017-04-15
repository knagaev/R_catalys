-- создание FT индекса с оптимизацией (на другом диске)
USE master
GO
ALTER DATABASE patstat2016a ADD FILEGROUP FT_FILEGROUP
GO

-- здесь указать каталог на другом диске по отношению к диску, на котором лежит основная база patstat2016a
ALTER DATABASE patstat2016a 
	ADD FILE (NAME = N'patstat2016a_ft', 
				FILENAME = N'E:\MSSQL\FT\patstat2016a_ft.mdf', 
				SIZE = 5120KB, 
				FILEGROWTH = 1024KB) 
	TO FILEGROUP FT_FILEGROUP
GO

USE patstat2016a
GO

CREATE FULLTEXT CATALOG abstr_ft 
	ON FILEGROUP FT_FILEGROUP
	WITH ACCENT_SENSITIVITY = OFF
	AS DEFAULT
GO

CREATE FULLTEXT INDEX ON tls203_appln_abstr
  KEY INDEX PK__tls203_a__D7B38CD4DC64B0B6
  ON abstr_ft
  WITH STOPLIST = SYSTEM, CHANGE_TRACKING OFF, NO POPULATION;
GO

ALTER FULLTEXT INDEX ON tls203_appln_abstr 
   START FULL POPULATION;
GO

-- проверка статуса процесса создания FT индекса
DECLARE @CatalogName VARCHAR(MAX)
SET     @CatalogName = 'abstr_ft'

SELECT
    DATEADD(ss, FULLTEXTCATALOGPROPERTY(@CatalogName,'PopulateCompletionAge'), '1/1/1990') AS LastPopulated
    ,(SELECT CASE FULLTEXTCATALOGPROPERTY(@CatalogName,'PopulateStatus')
        WHEN 0 THEN 'Idle'
        WHEN 1 THEN 'Full Population In Progress'
        WHEN 2 THEN 'Paused'
        WHEN 3 THEN 'Throttled'
        WHEN 4 THEN 'Recovering'
        WHEN 5 THEN 'Shutdown'
        WHEN 6 THEN 'Incremental Population In Progress'
        WHEN 7 THEN 'Building Index'
        WHEN 8 THEN 'Disk Full.  Paused'
        WHEN 9 THEN 'Change Tracking' END) AS PopulateStatus;
--SELECT OBJECTPROPERTY(object_id('tls203_appln_abstr'), 'TableFulltextPopulateStatus')

select * from sys.dm_fts_index_population;
GO

--статистика термов в FT индексе
SELECT display_term, count(*)
FROM sys.dm_fts_index_keywords_by_document(db_id('patstat2016a'), object_id('tls203_appln_abstr'))
group by display_term
order by count(*) desc; 

-- создание "тонкой" таблицы
USE patstat2016a
GO

select appln_id, YEAR(publn_date) publn_year, publn_auth
 into appln_search 
from tls211_pat_publn
GO

CREATE UNIQUE CLUSTERED INDEX ndx_appln_search ON appln_search
(
  appln_id ASC,
  publn_year ASC,
  publn_auth ASC
)
WITH (PAD_INDEX = ON, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, 
  DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = OFF, ALLOW_PAGE_LOCKS = OFF, FILLFACTOR = 100) 
ON [PRIMARY]
GO

UPDATE STATISTICS appln_search;
GO

-- запросы

-- статистика по годам
select publn_year, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, 'laser') ab
inner 
-- loop -- хинт, вроде бы с ним самые быстрые запросы получаются (альтернатива hash, merge) 
join appln_search ayc
on ayc.appln_id = ab.[KEY]
group by publn_year
order by count(*) desc;

-- статистика по годам и странам
select publn_year, publn_auth, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, 'laser') ab
inner 
-- loop
join appln_search ayc
on ayc.appln_id = ab.[KEY]
group by publn_year, publn_auth
order by count(*) desc
 ;

-- статистика по странам с фильтром по годам
select publn_auth, count(*)
from CONTAINSTABLE(tls203_appln_abstr, appln_abstract, 'laser') ab
inner 
-- loop -- хинт, вроде бы с ним самые быстрые запросы получаются (альтернатива hash, merge) 
join appln_search ayc
on ayc.appln_id = ab.[KEY]
where publn_year in ('2011', '1996', '2006')
group by publn_auth
order by count(*) desc
 ;
