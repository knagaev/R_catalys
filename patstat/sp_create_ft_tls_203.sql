SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		Константин Нагаев
-- Create date: 15.04.2014
-- Description:	Загрузка tls_203 Patstat
-- =============================================
CREATE PROCEDURE SP_CREATE_FT_TLS_203 
	@lang varchar(2),
	@db varchar(50) = 'patstat2016b' 
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @ft_lang varchar(50);

	SELECT @ft_lang =  CASE LOWER(@lang)
						WHEN 'ar' THEN '1025'			-- Arabic		
						WHEN 'bg' THEN '1026'			-- Bulgarian
						WHEN 'cs' THEN '1029'			-- Czech
						WHEN 'da' THEN '1030'			-- Danish
						WHEN 'de' THEN '1031'			-- German
						WHEN 'el' THEN '1032'			-- Greek (modern)	
						WHEN 'en' THEN '1033'			-- English (United States)
						WHEN 'es' THEN '3082'			-- Spanish
						WHEN 'et' THEN '1033'			-- Estonian like English
						WHEN 'fr' THEN '1036'			-- French
						WHEN 'hr' THEN '1050'			-- Croatian
						WHEN 'it' THEN '1040'			-- Italian
						WHEN 'ja' THEN '1041'			-- Japanese
						WHEN 'ko' THEN '1042'			-- Korean
						WHEN 'lt' THEN '1063'			-- Lithuanian
						WHEN 'lv' THEN '1062'			-- Latvian
						WHEN 'nl' THEN '1043'			-- Dutch (Netherlands)
						WHEN 'no' THEN '1044'			-- Norwegian
						WHEN 'pl' THEN '1045'			-- Polish
						WHEN 'pt' THEN '2070'			-- Portuguese
						WHEN 'ro' THEN '1048'			-- Romanian
						WHEN 'ru' THEN '1049'			-- Russian
						WHEN 'sh' THEN '1050'			-- Serbo-Croatian
						WHEN 'sk' THEN '1051'			-- Slovak
						WHEN 'sl' THEN '1060'			-- Slovene
						WHEN 'sr' THEN '2074'			-- Serbian
						WHEN 'sv' THEN '1053'			-- 	Swedish
						WHEN 'tr' THEN '1055'			-- Turkish
						WHEN 'uk' THEN '1058'			-- Ukrainian
						WHEN 'zh' THEN '3076'			-- Chinese
						ELSE '1033'
					END;

	DECLARE @SQLString VARCHAR(MAX)
	SET @SQLString = 'CREATE FULLTEXT INDEX ON tls203_appln_abstr_' + @lang + '
							(   
							  appln_abstract  
								 Language ' + @ft_lang + '     
								--STATISTICAL_SEMANTICS 
							 ) 
							  KEY INDEX PK_tls203_appln_abstr_' + @lang + '
							  ON tls_ft
							  WITH STOPLIST = SYSTEM, CHANGE_TRACKING OFF, NO POPULATION';

	--select @SQLString
	EXEC (@SQLString)    


END
GO
