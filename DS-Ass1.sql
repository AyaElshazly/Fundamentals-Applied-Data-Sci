-- PART A
-- create the Flight_System database
CREATE DATABASE Flight_System;
-- a)
USE Flight_System;
SELECT 
    COUNT(DISTINCT TAIL_NUMBER) AS num_aircrafts,
    COUNT(*) AS total_flights,
    MIN(DEPARTURE_DELAY) AS min_departure_delay,
    MAX(DEPARTURE_DELAY) AS max_departure_delay,
    AVG(DEPARTURE_DELAY) AS avg_departure_delay
FROM FLIGHTS;
-- b) 
CREATE VIEW FlightSummaryView AS
SELECT 
  CONVERT(date, CONVERT(varchar, YEAR) + '-' + CONVERT(varchar, MONTH) + '-' + CONVERT(varchar, DAY)) AS date,
  ORIGIN_AIRPORT AS iata_code,
  AIRPORT AS origin_airport,
  CITY + ', ' + STATE + ', ' + COUNTRY AS Address,
  COUNT(*) AS total_flights
FROM FLIGHTS f
JOIN AIRPORTS a ON f.ORIGIN_AIRPORT = a.IATA_CODE
WHERE YEAR = 2015 AND MONTH = 1 AND DAY BETWEEN 1 AND 7
GROUP BY CONVERT(date, CONVERT(varchar, YEAR) + '-' + CONVERT(varchar, MONTH) + '-' + CONVERT(varchar, DAY)), 
ORIGIN_AIRPORT, AIRPORT, CITY, STATE, COUNTRY;
SELECT * FROM FlightSummaryView ORDER BY iata_code desc;

--c)
WITH rank_routes AS (
SELECT
  origin_airport,
  destination_airport,
  COUNT(*) AS nb_flights
FROM FLIGHTS
GROUP BY origin_airport,destination_airport),
ranked_routes AS (
SELECT
  origin_airport,
  destination_airport,
  ROW_NUMBER() OVER(PARTITION BY origin_airport ORDER BY nb_flights DESC) AS rank FROM rank_routes)
SELECT
  origin_airport,
  destination_airport,
  rank
FROM ranked_routes
WHERE rank <= 3
ORDER BY origin_airport,rank;
--d)
SELECT AIRPORTS.IATA_CODE AS "airport iata_code", FLIGHTS.ORIGIN_AIRPORT AS "airport name",
airlines.IATA_CODE AS "airline iata_code" , airlines.AIRLINE AS "airline name",
FLIGHTS.FLIGHT_NUMBER, FLIGHTS.TAIL_NUMBER,  
FLIGHTS.DESTINATION_AIRPORT, 
FLIGHTS.DEPARTURE_TIME, FLIGHTS.ARRIVAL_TIME
FROM  FLIGHTS
left JOIN AIRPORTS ON FLIGHTS.ORIGIN_AIRPORT = AIRPORTS.IATA_CODE
left JOIN airlines ON FLIGHTS.AIRLINE =airlines.IATA_CODE
WHERE 
DAY_OF_WEEK IN (6,7) 
AND FLIGHTS.ARRIVAL_TIME   >= 0400
AND FLIGHTS.ARRIVAL_TIME <= 0500
ORDER BY FLIGHTS.ARRIVAL_TIME;
--e) using from subset subqueryw
SELECT JFK_Flights, 
       CAST(JFK_Flights AS FLOAT) / TotalFlights.Total * 100 AS JFK_Percentage
FROM (
    SELECT COUNT(*) AS JFK_Flights
    FROM FLIGHTS
    WHERE ORIGIN_AIRPORT = 'JFK'
) AS JFKCount, (
    SELECT COUNT(*) AS Total
    FROM FLIGHTS
) AS TotalFlights
--f)
-- Retrieve the flight information for all flights
SELECT *
FROM FLIGHTS
WHERE DESTINATION_AIRPORT IN ('JFK', 'LGA', 'EWR')
  AND ELAPSED_TIME > 500
  AND CANCELLED = 0;
-- Update their cancelled status from 0 to 1.
UPDATE FLIGHTS
SET CANCELLED = 1
WHERE DESTINATION_AIRPORT IN ('JFK', 'LGA', 'EWR')
  AND ELAPSED_TIME > 500
  AND CANCELLED = 0;
--g)
WITH Departure_Delays AS (
    SELECT a.IATA_CODE, a.AIRLINE, 
           CASE 
               WHEN f.DEPARTURE_DELAY > 120 THEN 'Big Delay'
               WHEN f.DEPARTURE_DELAY BETWEEN 60 AND 30 THEN 'Medium Delay'
               ELSE 'Small Delay'
           END AS Delay_Category, 
           COUNT(*) AS Total_Delays
    FROM FLIGHTS f
    INNER JOIN AIRLINES a ON f.AIRLINE = a.IATA_CODE
    WHERE f.DEPARTURE_DELAY > 0
    GROUP BY a.IATA_CODE, a.AIRLINE, 
             CASE 
                 WHEN f.DEPARTURE_DELAY > 120 THEN 'Big Delay'
                 WHEN f.DEPARTURE_DELAY BETWEEN 60 AND 30 THEN 'Medium Delay'
                 ELSE 'Small Delay'
             END
)
SELECT * INTO Departure_Delays
FROM Departure_Delays
ORDER BY Total_Delays DESC;
select * from Departure_Delays order by Total_Delays DESC;
-- Part B
-- Create database
CREATE DATABASE Sales;
use Sales;
----
--2023
--1)star schema
--2)--a
SELECT c.CustomerID, c.CustomerName, SUM(ps.Quantity) AS Total_Quantity, SUM(ps.Total) AS Total_Amount
FROM CUSTOMER c
INNER JOIN PRODUCT_SALES ps ON c.CustomerID = ps.CustomerID
INNER JOIN TIMELINE t ON ps.TimeID = t.TimeID
WHERE t.Date BETWEEN DATEADD(day, -90, '2018-05-31') AND '2018-05-31'
GROUP BY c.CustomerID, c.CustomerName;
--b
WITH AvgOrder AS (
    SELECT AVG(Total) AS OverallAvg
    FROM PRODUCT_SALES
), CustomerAvg AS (
    SELECT c.CustomerID, c.CustomerName, AVG(ps.Total) AS Avg_Total
    FROM CUSTOMER c
    INNER JOIN PRODUCT_SALES ps ON c.CustomerID = ps.CustomerID
    GROUP BY c.CustomerID, c.CustomerName
)
SELECT ca.CustomerID, ca.CustomerName, ca.Avg_Total
FROM CustomerAvg ca
INNER JOIN AvgOrder ao ON ca.Avg_Total > ao.OverallAvg;
--c
WITH ProductSalesWithDate AS (
    SELECT ps.CustomerID, p.ProductNumber, p.ProductName, t.Date, 
           LAG(t.Date) OVER (PARTITION BY ps.CustomerID, p.ProductNumber ORDER BY t.Date) AS End_Date
    FROM PRODUCT_SALES ps
    INNER JOIN PRODUCT p ON ps.ProductNumber = p.ProductNumber
    INNER JOIN TIMELINE t ON ps.TimeID = t.TimeID
), DaysBetweenProductSales AS (
    SELECT CustomerID, ProductNumber, ProductName, Date, End_Date, 
           DATEDIFF(day, End_Date, Date) AS Days_between_Product_Sales
    FROM ProductSalesWithDate
)
SELECT c.CustomerID, c.CustomerName, dbps.ProductNumber, dbps.ProductName, dbps.Date, dbps.End_Date, dbps.Days_between_Product_Sales
FROM CUSTOMER c
INNER JOIN DaysBetweenProductSales dbps ON c.CustomerID = dbps.CustomerID
ORDER BY c.CustomerID;
--d
SELECT Year, QuarterText, SUM(Total) AS Total_Sales
FROM PRODUCT_SALES ps
INNER JOIN TIMELINE t ON ps.TimeID = t.TimeID
GROUP BY ROLLUP(Year, QuarterText);
