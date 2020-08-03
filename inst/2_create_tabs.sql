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

CREATE TABLE IF NOT EXISTS `hospital` (
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

CREATE TABLE IF NOT EXISTS registry (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  full_name VARCHAR(255) DEFAULT NULL,
  CONSTRAINT `unique_name`
    UNIQUE (name)
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS ind (
  id VARCHAR(63) NOT NULL PRIMARY KEY,
  title VARCHAR(255),
  name VARCHAR(255),
  `type` VARCHAR(127) DEFAULT NULL,
  measure_unit VARCHAR(127) DEFAULT NULL,
  min_denominator TINYINT UNSIGNED DEFAULT NULL,
  min_value DOUBLE(7,3) DEFAULT NULL,
  max_value DOUBLE(7,3) DEFAULT NULL,
  level_green DOUBLE(7,3),
  level_yellow DOUBLE(7,3),
  level_direction TINYINT,
  short_description VARCHAR(1023),
  long_description VARCHAR(2047),
  registry_id SMALLINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_indicator_registry`
    FOREIGN KEY (registry_id) REFERENCES registry (id)
    ON UPDATE CASCADE
    ON DELETE RESTRICT
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
  user_id SMALLINT UNSIGNED NOT NULL,
  registry_id SMALLINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_user_registry_registry`
    FOREIGN KEY (registry_id) REFERENCES registry (id)
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
  hospital_id INT(10) UNSIGNED NOT NULL,
  year SMALLINT UNSIGNED NOT NULL,
  var DOUBLE(9,3) NOT NULL,
  denominator INT UNSIGNED DEFAULT NULL,
  registry_id SMALLINT UNSIGNED NOT NULL,
  ind_id VARCHAR(63) NOT NULL,
  delivery_id SMALLINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_data_hospital`
    FOREIGN KEY (hospital_id) REFERENCES hospital (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_delivery`
    FOREIGN KEY (delivery_id) REFERENCES delivery (id)
    ON UPDATE RESTRICT
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_ind`
    FOREIGN KEY (ind_id) REFERENCES ind (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_registry`
    FOREIGN KEY (registry_id) REFERENCES registry (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `agg_data` (
  `id` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `ind_id` varchar(63) COLLATE utf8_danish_ci NOT NULL,
  `unit_level` varchar(15) COLLATE utf8_danish_ci NOT NULL,
  `unit_name` varchar(255) COLLATE utf8_danish_ci NOT NULL,
  `orgnr` int(10) unsigned NOT NULL,
  `year` smallint(5) unsigned NOT NULL,
  `count` int(10) unsigned NOT NULL,
  `var` double(9,5) unsigned NOT NULL,
  `level` varchar(15) COLLATE utf8_danish_ci NOT NULL,
  `desired_level` varchar(15) COLLATE utf8_danish_ci NOT NULL,
  `time` TIMESTAMP DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  KEY `year` (`year`),
  KEY `ind_id` (`ind_id`),
  KEY `unit_level` (`unit_level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;
