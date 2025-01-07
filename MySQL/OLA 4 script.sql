-- Tilføj PriceID som ny kolonne samt primær nøgle for tabellen car_history
ALTER TABLE car_history ADD PriceID INT AUTO_INCREMENT PRIMARY KEY;

-- Tilføj PriceID som ny kolonne
ALTER TABLE car ADD PriceID INT;

-- Tilføj PriceID som fremmed nøgler til tabellen 'car'
ALTER TABLE car 
ADD CONSTRAINT fk_priceid 
FOREIGN KEY (PriceID) 
REFERENCES car_history(PriceID);

-- Tilføj CarID som primær nøgle til tabellen car
ALTER TABLE car
ADD PRIMARY KEY (CarID);

-- Tilføj DealerID som primær nøgle til tabellen dealer
ALTER TABLE dealer
ADD PRIMARY KEY (DealerID);

-- Tilføj DealerID som fremmed nøgler til tabellen 'car'
ALTER TABLE car
ADD CONSTRAINT fk_dealer
FOREIGN KEY (DealerID) 
REFERENCES dealer(DealerID);

