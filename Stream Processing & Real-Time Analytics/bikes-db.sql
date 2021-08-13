DROP DATABASE IF EXISTS bikes;
CREATE DATABASE bikes;
USE bikes;
DROP TABLE IF EXISTS Availability;
CREATE TABLE Availability (
  id VARCHAR(50),
  commonName VARCHAR(50),
  NbBikes INT,
  NbEmptyDocks INT,
  NbDocks INT,
  PRIMARY KEY(id)
);