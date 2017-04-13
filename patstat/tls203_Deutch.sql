USE [patstat2016b]

/****** Object:  Table [dbo].[tls203_appln_title]    Script Date: 07/07/2015 08:16:03 ******/
/*

--DROP TABLE [dbo].[tls203_appln_abstr_de]
GO
CREATE TABLE [dbo].[tls203_appln_abstr_de](
	[appln_id] [int] NOT NULL DEFAULT ('0'),
	--[appln_abstract_lg] [char](2) NOT NULL DEFAULT (''),
	[appln_abstract] [varchar](max) COLLATE German_PhoneBook_100_CI_AS NOT NULL ,
) ON [PRIMARY]

ALTER TABLE [dbo].tls203_appln_abstr_de
ADD CONSTRAINT PK_tls203_appln_abstr_de PRIMARY KEY CLUSTERED (appln_id);

truncate table [dbo].tls203_appln_abstr_de
GO

declare @begin datetime
select @begin = getdate()

BULK INSERT [dbo].tls203_appln_abstr_de
FROM 'C:\work\others\Patstat\test_de.txt'
WITH
(
BATCHSIZE = 10000,
--FIELDTERMINATOR ='"',  
--ROWTERMINATOR = '\n',  
--FIRSTROW = 3,
DATAFILETYPE = 'char',
FORMATFILE = 'C:\work\others\Patstat\tls203.fmt'
--,TABLOCK
)

select cast((getdate() - @begin) as float)
go

*/




GO

SELECT TOP 10 * FROM [patstat2016b].[dbo].tls203_appln_abstr_de;
GO

CREATE FULLTEXT INDEX ON [patstat2016b].[dbo].tls203_appln_abstr_de
 (   
  appln_abstract  
     Language German     
 )   
  KEY INDEX PK_tls203_appln_abstr_de   
      ON FT_CATALOG;
GO

select * from [patstat2016b].[dbo].tls203_appln_abstr_de
where CONTAINS(appln_abstract, 'FORMSOF(INFLECTIONAL, "information")');
GO

