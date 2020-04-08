CREATE TABLE IF NOT EXISTS org (
  id TINYINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  orgnumber INT UNSIGNED,
  valid BOOLEAN DEFAULT TRUE NOT NULL
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS user (
  id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  user_name VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  phone VARCHAR(15),
  email VARCHAR(255) NOT NULL,
  valid BOOLEAN DEFAULT TRUE NOT NULL,
  org_id TINYINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_user_org`
    FOREIGN KEY (org_id) REFERENCES org (id)
    ON UPDATE CASCADE
    ON DELETE RESTRICT
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
  OrgNrShus CHAR(9) NOT NULL,
  Variabel DOUBLE(9,3) NOT NULL,
  KvalIndID VARCHAR(63) NOT NULL,
  delivery_id SMALLINT UNSIGNED NOT NULL,
  CONSTRAINT `fk_data_delivery`
    FOREIGN KEY (delivery_id) REFERENCES delivery (id)
    ON UPDATE RESTRICT
    ON DELETE CASCADE
) ENGINE = InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;