CREATE TABLE `car` (
  `CarID` double NOT NULL,
  `Brand` varchar(8) DEFAULT NULL,
  `Modeltype` varchar(255) DEFAULT NULL,
  `Doors` varchar(2) DEFAULT NULL,
  `Year` varchar(7) DEFAULT NULL,
  `Km` varchar(7) DEFAULT NULL,
  `Range` varchar(3) DEFAULT NULL,
  `Description` text,
  `DealerID` varchar(5) DEFAULT NULL,
  `PriceID` int DEFAULT NULL,
  PRIMARY KEY (`CarID`),
  KEY `fk_dealer` (`DealerID`),
  KEY `fk_priceid` (`PriceID`),
  CONSTRAINT `fk_dealer` FOREIGN KEY (`DealerID`) REFERENCES `dealer` (`DealerID`),
  CONSTRAINT `fk_priceid` FOREIGN KEY (`PriceID`) REFERENCES `car_history` (`PriceID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci

CREATE TABLE `car_history` (
  `CarID` double DEFAULT NULL,
  `Price` double DEFAULT NULL,
  `Scrapedate` datetime(6) DEFAULT NULL,
  `Solgt` tinyint DEFAULT NULL,
  `PriceID` int NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`PriceID`)
) ENGINE=InnoDB AUTO_INCREMENT=1123 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci

CREATE TABLE `dealer` (
  `Location` varchar(255) DEFAULT NULL,
  `DealerName` varchar(255) DEFAULT NULL,
  `DealerCVR` varchar(8) DEFAULT NULL,
  `DealerID` varchar(5) NOT NULL,
  `DealerAddress` varchar(255) DEFAULT NULL,
  `Link` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`DealerID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci