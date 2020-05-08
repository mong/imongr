CREATE INDEX ind_registry_register ON registry (Register);

CREATE INDEX ind_indicator_indid ON indicator (IndID);

CREATE INDEX ind_org_orgnrshus ON org (OrgNrShus);

CREATE INDEX ind_data_shnavn ON data (ShNavn);
CREATE INDEX ind_data_orgnrshus ON data (OrgNrShus);
CREATE INDEX ind_data_kvalindid ON data (KvalIndID);
