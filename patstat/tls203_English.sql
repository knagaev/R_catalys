USE [patstat2016b]

/****** Object:  Table [dbo].[tls203_appln_title]    Script Date: 07/07/2015 08:16:03 ******/
/*

--DROP TABLE [dbo].[tls203_appln_abstr_en]
GO
CREATE TABLE [dbo].[tls203_appln_abstr_en](
	[appln_id] [int] NOT NULL DEFAULT ('0'),
	--[appln_abstract_lg] [char](2) NOT NULL DEFAULT (''),
	[appln_abstract] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
) ON [PRIMARY]

ALTER TABLE [dbo].tls203_appln_abstr_en
ADD CONSTRAINT PK_tls203_appln_abstr_en PRIMARY KEY CLUSTERED (appln_id);

truncate table [dbo].tls203_appln_abstr_en
GO

--declare @datafile varchar
--select @datafile = 'C:\work\others\Patstat\!1\processed\tls203_part01_en.txt'

declare @begin datetime
select @begin = getdate()
select @begin

BULK INSERT [dbo].tls203_appln_abstr_en
FROM 'G:\Patstat\Patstat Biblio\data_PATSTAT_Biblio_2016_Autumn\tls203\processed\tls203_en.txt'
WITH
(
BATCHSIZE = 20000, 
-- 100000 0,00134355709876543
-- 50000  0,00143587962962963
-- 20000  0,00125435956790123
-- 10000  0,00130119598765432
-- 1000   0,00194074074074074
DATAFILETYPE = 'char',
FORMATFILE = 'C:\PLR\R_catalys\patstat\tls203.fmt'
)

select getdate()
select cast((getdate() - @begin) as float)
go


*/




GO

SELECT TOP 10 * FROM [patstat2016b].[dbo].tls203_appln_abstr_en;
GO

CREATE FULLTEXT INDEX ON [patstat2016b].[dbo].tls203_appln_abstr_en
 (   
  appln_abstract  
     Language English     
	STATISTICAL_SEMANTICS 
 )   
  KEY INDEX PK_tls203_appln_abstr_en   
      ON FT_CATALOG;
GO

select * from [patstat2016b].[dbo].tls203_appln_abstr_en
where CONTAINS(appln_abstract, 'FORMSOF(INFLECTIONAL, "information")');
GO

