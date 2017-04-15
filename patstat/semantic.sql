/*** создание FT каталога с оптимизацией (на другом диске) ***/
USE master
GO
CREATE DATABASE semanticsdb  
            ON ( FILENAME = 'E:\MSSQL\semanticsDB.mdf' )  
            LOG ON ( FILENAME = 'E:\MSSQL\semanticsdb_log.ldf' )  
            FOR ATTACH;  
GO  

EXEC sp_fulltext_semantic_register_language_statistics_db @dbname = N'semanticsdb';  
GO  

SELECT * FROM sys.fulltext_semantic_language_statistics_database;

