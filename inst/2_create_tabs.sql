CREATE TABLE IF NOT EXISTS org (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  OrgNrRHF INT UNSIGNED,
  RHF VARCHAR(255),
  OrgNrHF INT UNSIGNED,
  HF VARCHAR(255),
  Hfkortnavn VARCHAR(255),
  OrgNrShus INT UNSIGNED NOT NULL,
  OrgNavnEnhetsreg VARCHAR(255),
  SykehusnavnLang VARCHAR(255),
  SykehusNavn VARCHAR(255),
  TaMed BOOLEAN DEFAULT TRUE NOT NULL,
  CONSTRAINT `unique_OrgNrShus`
    UNIQUE (OrgNrShus)
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS indicator (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  IndID VARCHAR(63) NOT NULL,
  Register VARCHAR(255) NOT NULL,
  IndTittel VARCHAR(255),
  IndNavn VARCHAR(255),
  MaalNivaaGronn DOUBLE(7,3),
  MaalNivaaGul DOUBLE(7,3),
  MaalRetn TINYINT,
  BeskrivelseKort VARCHAR(1023),
  BeskrivelseLang VARCHAR(2047),
  CONSTRAINT `unique_IndID`
    UNIQUE (IndID)
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS registry (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  Register VARCHAR(255) NOT NULL,
  full_name VARCHAR(255),
  CONSTRAINT `unique_Register`
    UNIQUE (Register)
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS user (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  user_name VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  phone VARCHAR(15),
  email VARCHAR(255) NOT NULL,
  valid BOOLEAN DEFAULT TRUE NOT NULL
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE TABLE IF NOT EXISTS user_registry (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  Register VARCHAR(255) NOT NULL,
  user_id SMALLINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_user_registry_registry`
    FOREIGN KEY (Register) REFERENCES registry (Register)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_user_registry_user`
    FOREIGN KEY (user_id) REFERENCES user (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE TABLE IF NOT EXISTS delivery (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  latest BOOLEAN NOT NULL,
  time TIMESTAMP DEFAULT CURRENT_TIMESTAMP(),
  md5_checksum CHAR(32) NOT NULL,
  user_id SMALLINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_delivery_user`
    FOREIGN KEY (user_id) REFERENCES user (id)
    ON UPDATE CASCADE
    ON DELETE RESTRICT
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS data (
  id MEDIUMINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  Aar SMALLINT UNSIGNED NOT NULL,
  ShNavn VARCHAR(127) NOT NULL,
  ReshId VARCHAR(12),
  OrgNrShus INT UNSIGNED NOT NULL,
  Variabel DOUBLE(9,3) NOT NULL,
  KvalIndID VARCHAR(63) NOT NULL,
  delivery_id SMALLINT UNSIGNED NOT NULL,
  Register VARCHAR(255) NOT NULL,
  CONSTRAINT `fk_data_delivery`
    FOREIGN KEY (delivery_id) REFERENCES delivery (id)
    ON UPDATE RESTRICT
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_org`
    FOREIGN KEY (OrgNrShus) REFERENCES org (OrgNrShus)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_indicator`
    FOREIGN KEY (KvalIndID) REFERENCES indicator (IndID)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_registry`
    FOREIGN KEY (Register) REFERENCES registry (Register)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;
