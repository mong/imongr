CREATE TABLE IF NOT EXISTS `nation` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) COLLATE utf8_danish_ci NOT NULL,
  `short_name` varchar(128) COLLATE utf8_danish_ci NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `orgnr` (`orgnr`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `rhf` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) COLLATE utf8_danish_ci NOT NULL,
  `short_name` varchar(128) COLLATE utf8_danish_ci NOT NULL,
  `nation_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `orgnr` (`orgnr`),
  CONSTRAINT `fk_nation_id`
    FOREIGN KEY (nation_id) REFERENCES nation (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `hf` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) COLLATE utf8_danish_ci NOT NULL,
  `short_name` varchar(128) COLLATE utf8_danish_ci NOT NULL,
  `rhf_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `orgnr` (`orgnr`),
  CONSTRAINT `fk_rhf_id`
    FOREIGN KEY (rhf_id) REFERENCES rhf (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `shus` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) COLLATE utf8_danish_ci NOT NULL,
  `short_name` varchar(128) COLLATE utf8_danish_ci NOT NULL,
  `hf_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `orgnr` (`orgnr`),
  CONSTRAINT `fk_hf_id`
    FOREIGN KEY (hf_id) REFERENCES hf (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

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
  indikatorType VARCHAR(127) DEFAULT NULL,
  benevning VARCHAR(127) DEFAULT NULL,
  minsteNevner TINYINT UNSIGNED DEFAULT NULL,
  minVerdi DOUBLE(7,3) DEFAULT NULL,
  maxVerdi DOUBLE(7,3) DEFAULT NULL,
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
  full_name VARCHAR(255) DEFAULT NULL,
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
  nevner INT UNSIGNED DEFAULT NULL,
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

CREATE TABLE IF NOT EXISTS `agg_data` (
  `id` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `IndID` varchar(63) COLLATE utf8_danish_ci NOT NULL,
  `unit_level` varchar(15) COLLATE utf8_danish_ci NOT NULL,
  `unit_name` varchar(255) COLLATE utf8_danish_ci NOT NULL,
  `OrgNr` int(10) unsigned NOT NULL,
  `Aar` smallint(5) unsigned NOT NULL,
  `count` int(10) unsigned NOT NULL,
  `indicator` double(9,5) unsigned NOT NULL,
  `level` varchar(15) COLLATE utf8_danish_ci NOT NULL,
  `desired_level` varchar(15) COLLATE utf8_danish_ci NOT NULL,
  `time` TIMESTAMP DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  KEY `Aar` (`Aar`),
  KEY `IndID` (`IndID`),
  KEY `unit_level` (`unit_level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;
