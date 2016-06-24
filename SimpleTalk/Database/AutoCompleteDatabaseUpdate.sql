USE [AutoComplete]
GO

/****** Object:  Table [dbo].[Letters]    Script Date: 01/19/2010 00:06:21 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Letters](
	[LetterID] [int] IDENTITY(1,1) NOT NULL,
	[Letter] [char](1) NOT NULL,
	[Count] [int] NOT NULL,
 CONSTRAINT [PK_Letters] PRIMARY KEY CLUSTERED 
(
	[LetterID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO

USE [AutoComplete]
GO

/****** Object:  Table [dbo].[Phrases]    Script Date: 01/19/2010 00:06:21 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Phrases](
	[PhraseID] [int] IDENTITY(1,1) NOT NULL,
	[DateTime] [datetime] NOT NULL,
	[Phrase] [varchar](max) NOT NULL,
 CONSTRAINT [PK_Phrases] PRIMARY KEY CLUSTERED 
(
	[PhraseID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO




USE [AutoComplete]
GO

/****** Object:  StoredProcedure [dbo].[Letters_Add]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Adds a Letter to the database */
CREATE PROCEDURE [dbo].[Letters_Add] (
		@LetterID                         int OUTPUT,
		@Letter                           char(1),
		@Count                            int
) 
AS
	INSERT INTO Letters (
		[Letter],
		[Count]
) VALUES (
		@Letter,
		@Count
)
	SELECT @LetterID= SCOPE_IDENTITY();


GO

/****** Object:  StoredProcedure [dbo].[Letters_GetAll]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Retrieves all Letters from the database */
CREATE PROCEDURE [dbo].[Letters_GetAll]
AS
SELECT *
FROM [Letters]



GO

/****** Object:  StoredProcedure [dbo].[Letters_GetById]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Retrieves one Letter from the database using its primary key */
CREATE PROCEDURE [dbo].[Letters_GetById] (
		@LetterID                         int
)
AS
	SELECT *
	FROM [Letters]
	WHERE
		[Letters].[LetterID] = @LetterID


GO

/****** Object:  StoredProcedure [dbo].[Letters_Modify]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




/* Modifies an Letter in the database */
CREATE PROCEDURE [dbo].[Letters_Modify] (
		@LetterID                         int,
		@Letter                           char(1),
		@Count                            int
)
AS
	UPDATE [Letters] SET
		[Letters].[Letter] = @Letter, 
		[Letters].[Count] = @Count
	WHERE
		[Letters].[LetterID] = @LetterID


GO

/****** Object:  StoredProcedure [dbo].[Letters_Reset]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Retrieves one AutoWord from the database using its primary key */
CREATE PROCEDURE [dbo].[Letters_Reset]
AS
	UPDATE 
		Letters
	SET
		[Count] = 0


GO

/****** Object:  StoredProcedure [dbo].[Letters_UpdateLetterCount]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Deletes a AutoWord from the database */
CREATE PROCEDURE [dbo].[Letters_UpdateLetterCount] (
		@Letter                           char(1)
) 
AS
	UPDATE 
		Letters 
	SET
		[Count] = [Count] + 1
	WHERE 
		([Letter] = @Letter)


GO

/****** Object:  StoredProcedure [dbo].[Phrases_Add]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Adds a Phrase to the database */
CREATE PROCEDURE [dbo].[Phrases_Add] (
		@PhraseID                         int OUTPUT,
		@DateTime                         datetime,
		@Phrase                           varchar(MAX)
) 
AS
	INSERT INTO Phrases (
		[DateTime],
		[Phrase]
) VALUES (
		@DateTime,
		@Phrase
)
	SELECT @PhraseID= SCOPE_IDENTITY();


GO

/****** Object:  StoredProcedure [dbo].[Phrases_CountRecords]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Deletes a Phrase from the database */
CREATE PROCEDURE [dbo].[Phrases_CountRecords]
AS
	SELECT COUNT(*) AS [Count] FROM Phrases


GO

/****** Object:  StoredProcedure [dbo].[Phrases_Delete]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Deletes a Phrase from the database */
CREATE PROCEDURE [dbo].[Phrases_Delete] (
		@PhraseID                         int
) 
AS
	DELETE FROM Phrases WHERE (
		[PhraseID] = @PhraseID
)


GO

/****** Object:  StoredProcedure [dbo].[Phrases_DeleteByDateTime]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Deletes a Phrase from the database */
CREATE PROCEDURE [dbo].[Phrases_DeleteByDateTime] (
		@DateTime                         DATETIME
) 
AS
	DELETE FROM Phrases WHERE (
		[DateTime] = @DateTime
)


GO

/****** Object:  StoredProcedure [dbo].[Phrases_GetAll]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Retrieves all Phrases from the database */
CREATE PROCEDURE [dbo].[Phrases_GetAll]
AS
SELECT *
FROM [Phrases]



GO

/****** Object:  StoredProcedure [dbo].[Phrases_GetById]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Retrieves one Phrase from the database using its primary key */
CREATE PROCEDURE [dbo].[Phrases_GetById] (
		@PhraseID                         int
)
AS
	SELECT *
	FROM [Phrases]
	WHERE
		[Phrases].[PhraseID] = @PhraseID


GO

/****** Object:  StoredProcedure [dbo].[Phrases_GetPhraseList]    Script Date: 01/19/2010 00:05:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[Phrases_GetPhraseList]
(
	@DateTime AS DATETIME
)
AS
	SELECT TOP 10
	    [DateTime], 
		[Phrase]
	FROM 
		[Phrases]
	WHERE
		([DateTime] <= @DateTime)
	ORDER BY
	    [Phrases].[DateTime] DESC


GO

/****** Object:  StoredProcedure [dbo].[Phrases_Modify]    Script Date: 01/19/2010 00:05:47 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




/* Modifies an Phrase in the database */
CREATE PROCEDURE [dbo].[Phrases_Modify] (
		@PhraseID                         int,
		@DateTime                         datetime,
		@Phrase                           varchar(MAX)
)
AS
	UPDATE [Phrases] SET
		[Phrases].[DateTime] = @DateTime, 
		[Phrases].[Phrase] = @Phrase
	WHERE
		[Phrases].[PhraseID] = @PhraseID


GO

/****** Object:  StoredProcedure [dbo].[Phrases_Reset]    Script Date: 01/19/2010 00:05:47 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



/* Retrieves one AutoWord from the database using its primary key */
CREATE PROCEDURE [dbo].[Phrases_Reset]
AS
	DELETE Phrases
	
	DBCC CHECKIDENT (Phrases, RESEED, 0)


GO


