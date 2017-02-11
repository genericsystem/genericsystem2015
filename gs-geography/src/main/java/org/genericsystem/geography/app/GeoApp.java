package org.genericsystem.geography.app;

import java.io.BufferedReader;
import java.io.FileReader;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.geography.app.GeoApp.GeoScript;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm1;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm2;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm3;
import org.genericsystem.geography.model.Building;
import org.genericsystem.geography.model.City;
import org.genericsystem.geography.model.Continent;
import org.genericsystem.geography.model.Country;
import org.genericsystem.geography.model.Subdivision;
import org.genericsystem.geography.model.Subdivision.SubdivisionName;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents.Monitor;
import org.genericsystem.reactor.gscomponents.Responsive;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

@RunScript(GeoScript.class)
@DependsOnModel({ Subdivision.class, AdministrativeTerritory.class, Continent.class, Country.class, City.class,
		Building.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class,
		TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class })
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Continent.class, Country.class,
		Adm1.class, Adm2.class, Adm3.class })
public class GeoApp extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoApp.class, "/GeoApp");
	}

	public static class GeoScript implements Script {

		Generic continent, adm, country, adm1, adm2, adm3, city, name;
		Generic continentInstance, admInstance, countryInstance, adm1Instance, adm2Instance, adm3Instance, cityInstance;

		@Override
		public void run(Root engine) {

			// try {
			// Runtime r = Runtime.getRuntime();
			// Process p = r.exec("rm -rf /home/middleware/genericsystem/GeoApp");
			// p.waitFor();
			// } catch (IOException e1) {
			// // TODO Auto-generated catch block
			// e1.printStackTrace();
			// } catch (InterruptedException e1) {
			// // TODO Auto-generated catch block
			// e1.printStackTrace();
			// }

			continent = engine.find(Continent.class);
			adm = engine.find(AdministrativeTerritory.class);
			country = engine.find(Country.class);
			adm1 = engine.find(Adm1.class);
			adm2 = engine.find(Adm2.class);
			adm3 = engine.find(Adm3.class);
			city = engine.find(City.class);
			name = engine.find(SubdivisionName.class);

			// Remove directory

			String line;
			BufferedReader reader;
			String[] parts;
			// Continents
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/continents.csv"));
				while ((line = reader.readLine()) != null) {
					parts = line.split("\\|");
					System.out.println("continent" + parts[0] + "@" + parts[1]);
					continentInstance = continent.addInstance("CTNT_" + parts[0]);
					continentInstance.addHolder(name, parts[1]);
					engine.getCurrentCache().flush();
				}
				reader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			// Countries
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/countries.csv"));
				while ((line = reader.readLine()) != null) {
					parts = line.split("\\|");
					System.out.println("country" + parts[0] + "@" + parts[1] + "@" + parts[2]);
					continentInstance = continent.getInstance("CTNT_" + parts[2]);
					countryInstance = country.addInstance("CTRY_" + parts[0], continentInstance);
					countryInstance.addHolder(name, parts[1]);
					engine.getCurrentCache().flush();
				}
				reader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			// First level administrations (only France)
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/adm1.csv"));
				while ((line = reader.readLine()) != null) {
					try {
						parts = line.split("\\|");
						if (parts[0].equals("FR")) {
							System.out.println("adm1" + parts[0] + "@" + parts[1] + "@" + parts[2]);
							countryInstance = country.getInstance("CTRY_" + parts[0]);
							adm1Instance = adm1.addInstance("ADM1_" + parts[0] + "_" + parts[1], countryInstance);
							adm1Instance.addHolder(name, parts[2]);
							engine.getCurrentCache().flush();
						}
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				reader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			// Second level administrations (only France)
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/adm2.csv"));
				while ((line = reader.readLine()) != null) {
					try {
						parts = line.split("\\|");
						if (parts[0].equals("FR")) {
							System.out.println("adm2" + parts[0] + "@" + parts[1] + "@" + parts[2] + "@" + parts[3]);
							adm1Instance = adm1.getInstance("ADM1_" + parts[0] + "_" + parts[1]);
							adm2Instance = adm2.addInstance("ADM2_" + parts[0] + "_" + parts[1] + "_" + parts[2],
									adm1Instance);
							adm2Instance.addHolder(name, parts[3]);
							engine.getCurrentCache().flush();
						}
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				reader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			// Third level administrations (only France)
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/adm3.csv"));
				while ((line = reader.readLine()) != null) {
					try {
						parts = line.split("\\|");
						if (parts[0].equals("FR")) {
							System.out.println("adm3" + parts[0] + "@" + parts[1] + "@" + parts[2] + "@" + parts[3]
									+ "@" + parts[4]);
							adm2Instance = adm2.getInstance("ADM2_" + parts[0] + "_" + parts[1] + "_" + parts[2]);
							adm3Instance = adm3.addInstance(
									"ADM3_" + parts[0] + "_" + parts[1] + "_" + parts[2] + "_" + parts[3],
									adm2Instance);
							adm3Instance.addHolder(name, parts[4]);
							engine.getCurrentCache().flush();
						}
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				reader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			// Populated places (only France)
			String stradm;
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/populated_fr.csv"));
				while ((line = reader.readLine()) != null) {
					try {
						parts = line.split("\\|");
						// if (parts[0].equals("FR")) {
						if (parts[3].length() != 0) {
							stradm = "ADM3_" + parts[0] + "_" + parts[1] + "_" + parts[2] + "_" + parts[3];
							admInstance = adm3.getInstance(stradm);
						} else if (parts[2].length() != 0) {
							stradm = "ADM2_" + parts[0] + "_" + parts[1] + "_" + parts[2];
							admInstance = adm2.getInstance(stradm);
						} else if (parts[1].length() != 0) {
							stradm = "ADM1_" + parts[0] + "_" + parts[1];
							admInstance = adm1.getInstance(stradm);
						} else {
							stradm = "CTRY_" + parts[0];
							admInstance = country.getInstance(stradm);
						}
						cityInstance = city.addInstance(stradm + "_" + parts[4] + "_" + parts[5] + "_" + parts[6],
								admInstance);
						cityInstance.addHolder(name, parts[4]);
						engine.getCurrentCache().flush();
						System.out.println(cityInstance);
						// }
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}

	}

}
