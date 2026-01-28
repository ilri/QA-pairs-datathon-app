/*
 * questions.sql	Beginner's database
 */

PRAGMA foreign_keys = ON;
BEGIN TRANSACTION;

DROP TABLE IF EXISTS Questions;

CREATE TABLE Questions(
	ID INTEGER PRIMARY KEY,
	team	TEXT,
	persona	TEXT,
	topic	TEXT,
	question	TEXT NOT NULL,
	answered INTEGER DEFAULT 0 NOT NULL
);

DROP TABLE IF EXISTS Answers;

CREATE TABLE Answers(
	ID INTEGER PRIMARY KEY,
	question_ID INTEGER NOT NULL,
	team_answering TEXT,
	answer	TEXT NOT NULL,
	sources	TEXT,
	FOREIGN KEY (question_ID) REFERENCES Questions(ID)
);


CREATE VIEW Complete AS
SELECT Questions.ID, team, persona, topic, question, answer, sources, team_answering
FROM Questions LEFT OUTER JOIN Answers
ON Questions.ID = Answers.question_ID;

COMMIT;					
