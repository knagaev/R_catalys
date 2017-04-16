SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		Константин Нагаев
-- Create date: 15.04.2014
-- Description:	Загрузка tls_203 Patstat
-- =============================================
CREATE PROCEDURE SP_CREATE_TABLE_TLS_203 
	@lang varchar(2),
	@db varchar(50) = 'patstat2016b' 
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @collate varchar(50);

	SELECT @collate =  CASE LOWER(@lang)
						WHEN 'ar' THEN 'Arabic_CI_AS'					-- Arabic		
						WHEN 'bg' THEN 'Cyrillic_General_CI_AS'			-- Bulgarian
						WHEN 'cs' THEN 'Czech_CI_AS'					-- Czech
						WHEN 'da' THEN 'Danish_Norwegian_CS_AS'			-- Danish
						WHEN 'de' THEN 'Latin1_General_CI_AS'			-- German
						WHEN 'el' THEN 'Greek_CI_AS'					-- Greek (modern)	
						WHEN 'en' THEN 'SQL_Latin1_General_CP1_CI_AS'	-- English (United States)
						WHEN 'es' THEN 'Modern_Spanish_CS_AS'			-- Spanish
						WHEN 'et' THEN 'Estonian_CI_AS'					-- Estonian
						WHEN 'fr' THEN 'French_CI_AS'					-- French
						WHEN 'hr' THEN 'Croatian_CS_AS'					-- Croatian
						WHEN 'it' THEN 'Latin1_General_CI_AS'			-- Italian
						WHEN 'ja' THEN 'Japanese_CI_AS'					-- Japanese
						WHEN 'ko' THEN 'Korean_Wansung_CI_AS'			-- Korean
						WHEN 'lt' THEN 'Lithuanian_CS_AS'				-- Lithuanian
						WHEN 'lv' THEN 'Latvian_CS_AS'					-- Latvian
						WHEN 'nl' THEN 'Latin1_General_CI_AS'			-- Dutch (Netherlands)
						WHEN 'no' THEN 'Danish_Norwegian_CS_AS'			-- Norwegian
						WHEN 'pl' THEN 'Polish_CI_AS'					-- Polish
						WHEN 'pt' THEN 'Latin1_General_CI_AS'			-- Portuguese
						WHEN 'ro' THEN 'Romanian_CS_AS'					-- Romanian
						WHEN 'ru' THEN 'Cyrillic_General_CI_AS'			-- Russian
						WHEN 'sh' THEN 'Latin1_General_CI_AS'			-- Serbo-Croatian
						WHEN 'sk' THEN 'Slovak_CI_AS'					-- Slovak
						WHEN 'sl' THEN 'Slovenian_CI_AS'				-- Slovene
						WHEN 'sr' THEN 'Cyrillic_General_CI_AS'			-- Serbian
						WHEN 'sv' THEN 'Finnish_Swedish_CI_AS'			-- 	Swedish
						WHEN 'tr' THEN 'Turkish_CI_AS'					-- Turkish
						WHEN 'uk' THEN 'Ukrainian_CI_AS'				-- Ukrainian
						WHEN 'zh' THEN 'Chinese_PRC_CI_AS'				-- Chinese
						ELSE 'SQL_Latin1_General_CP1_CI_AS'
					END;

	DECLARE @SQLString VARCHAR(MAX)
	SET @SQLString = 'CREATE TABLE [tls203_appln_abstr_' + @lang + '](
		[appln_id] [int] NOT NULL,
		[appln_abstract] [varchar](max) COLLATE ' + @collate + ' NOT NULL,
		[tls203_timestamp] [timestamp] NOT NULL,
		CONSTRAINT PK_tls203_appln_abstr_' + @lang + ' PRIMARY KEY CLUSTERED (appln_id)  
				   WITH (IGNORE_DUP_KEY = OFF)
	) ON [PRIMARY]';

	--select @SQLString
	EXEC (@SQLString)    

END
GO
