#! /usr/bin/env nix-shell
#! nix-shell -i python -p python3 
import sqlite3
import json

# Generates some JSON dumps from an EVE Online Static Database in sqlite format
# The data is used under the EVE Online Third Party Developer License

if __name__ == "__main__":
    con = sqlite3.connect("sqlite-latest.sqlite")
    cur = con.cursor()
    with open("citadels.json", "w") as fh:
        for (typeID, typeName) in cur.execute("""SELECT
  it.typeID,
  it.typeName
FROM invTypes it
WHERE
  it.groupID IN (1657, 1404, 1406)"""):
            fh.write(json.dumps({"typeID": typeID, "typeName": typeName})+"\n")

    with open("rigsizes.json", "w") as fh:
        for (typeID, typeName, size) in cur.execute("""SELECT
  it.typeID,
  it.typeName,
  dta.valueFloat
FROM invTypes it
LEFT JOIN dgmTypeAttributes dta
  ON dta.typeID = it.typeID
WHERE
  dta.attributeID = 1547"""):
            fh.write(json.dumps({"typeID": typeID, "typeName": typeName, "size": size})+"\n")

    with open("capitals.json", "w") as fh:
        for (typeID, typeName, techType) in cur.execute("""SELECT
  it.typeID,
  it.typeName,
  imt.metaGroupID
FROM dgmTypeAttributes dta
LEFT JOIN invTypes it
  ON dta.typeID= it.typeID
LEFT JOIN invMetaTypes imt
  ON it.typeID = imt.typeID
WHERE attributeID=1785
  AND valueFloat=1.0"""):
            fh.write(json.dumps({"typeID": typeID, "typeName": typeName, "techType": techType or 1 })+"\n")
