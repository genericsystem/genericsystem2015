package org.genericsystem.geography.app;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.List;
import java.util.Objects;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.geography.app.GeoApp4.GeoScript;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm1;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm2;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm3;
import org.genericsystem.geography.model.AdministrativeTerritory.AdmCode;
import org.genericsystem.geography.model.Building;
import org.genericsystem.geography.model.City;
import org.genericsystem.geography.model.Continent;
import org.genericsystem.geography.model.Continent.ContinentCode;
import org.genericsystem.geography.model.Country;
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
@DependsOnModel({ AdministrativeTerritory.class, Continent.class, Country.class, City.class, Building.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class,
		TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class })
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Continent.class, Country.class,
		Adm1.class, Adm2.class, Adm3.class })
public class GeoApp4 extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoApp4.class, "/GeoApp");
	}

	public static class GeoScript implements Script {

		public static List<Generic> findInstanceByHolder(Generic type, Generic attribute, String holderValue) {
			return type.getInstances().filter(cont -> Objects.equals(cont.getHolder(attribute).getValue(), holderValue))
					.toList();
		}

		@Override
		public void run(Root engine) {

			Generic continent, adm, country, adm1, adm2, adm3, city, name, admCode, continentCode;
			Generic continentInstance, admInstance, countryInstance, adm1Instance, adm2Instance, adm3Instance,
					cityInstance;

			continent = engine.find(Continent.class);
			continentCode = engine.find(ContinentCode.class);
			adm = engine.find(AdministrativeTerritory.class);
			admCode = engine.find(AdmCode.class);
			country = engine.find(Country.class);
			adm1 = engine.find(Adm1.class);
			adm2 = engine.find(Adm2.class);
			adm3 = engine.find(Adm3.class);
			city = engine.find(City.class);

			// Remove directory
			String line;
			BufferedReader reader;
			String[] parts;

			// Continents
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/continents.csv"));
				while ((line = reader.readLine()) != null) {
					parts = line.split("\\|");
					System.out.println(parts[1]);
					continentInstance = continent.addInstance(parts[1]);
					continentInstance.addHolder(continentCode, parts[0]);
				}
				reader.close();
				engine.getCurrentCache().flush();
			} catch (Exception e) {
				e.printStackTrace();
			}

			// Countries
			try {
				reader = new BufferedReader(new FileReader("src/main/resources/countries.csv"));
				while ((line = reader.readLine()) != null) {
					parts = line.split("\\|");
					System.out.println("country " + "@" + parts[1]);
					continentInstance = findInstanceByHolder(continent, continentCode, parts[2]).get(0);
					countryInstance = country.addInstance(parts[1], continentInstance);
					countryInstance.addHolder(admCode, parts[0]);
				}
				engine.getCurrentCache().flush();
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
							System.out.println("adm1 " + parts[0] + " " + parts[2]);
							countryInstance = findInstanceByHolder(country, admCode, parts[0]).get(0);
							adm1Instance = adm1.addInstance(parts[2], countryInstance);
							adm1Instance.addHolder(admCode, parts[0] + "_" + parts[1]);
						}
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				engine.getCurrentCache().flush();
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
							System.out.println("adm2 " + parts[0] + " " + parts[3]);
							countryInstance = findInstanceByHolder(country, admCode, parts[0]).get(0);
							adm1Instance = findInstanceByHolder(adm1, admCode, parts[0] + "_" + parts[1]).get(0);
							adm2Instance = adm2.addInstance(parts[3], adm1Instance);
							adm2Instance.addHolder(admCode, parts[0] + "_" + parts[1] + "_" + parts[2]);
						}
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				engine.getCurrentCache().flush();
				reader.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
			// Third level administrations (only France)
			// long start = System.currentTimeMillis();
			// try {
			// reader = new BufferedReader(new FileReader("src/main/resources/adm3.csv"));
			// while ((line = reader.readLine()) != null) {
			// try {
			// parts = line.split("\\|");
			// if (parts[0].equals("FR")) {
			// System.out.println("adm3" + parts[0] + "@" + parts[1] + "@" + parts[2] + "@" + parts[3]
			// + "@" + parts[4]);
			//
			// countryInstance = country.getInstance(parts[0]);
			// adm1Instance = adm1.getInstance(parts[1], countryInstance);
			// adm2Instance = adm2.getInstance(parts[2], adm1Instance);
			//
			// // adm3Instance = adm3.addInstance(
			// // "ADM3_" + parts[0] + "_" + parts[1] + "_" + parts[2] + "_" + parts[3],
			// // adm2Instance);
			//
			// adm3Instance = engine.getCurrentCache().buildAndPlug(null, adm3, Collections.emptyList(),
			// parts[3], Collections.singletonList(adm2Instance));
			//
			// // adm3Instance.addHolder(name, parts[4]);
			// // engine.getCurrentCache().buildAndPlug(null, name, Collections.emptyList(), parts[4],
			// // Collections.singletonList(adm3Instance));
			//
			// engine.getCurrentCache().flush();
			// }
			// } catch (Exception e) {
			// // TODO Auto-generated catch block
			// e.printStackTrace();
			// }
			// }
			// reader.close();
			// } catch (Exception e) {
			// e.printStackTrace();
			// }
			// System.out.println(System.currentTimeMillis() - start);
			// // Populated places (only France)
			// try {
			// long i = 0;
			// reader = new BufferedReader(new FileReader("src/main/resources/populated_fr.csv"));
			// while ((line = reader.readLine()) != null) {
			// try {
			// parts = line.split("\\|");
			// // if (parts[0].equals("FR")) {
			// start = System.currentTimeMillis();
			// countryInstance = country.getInstance(parts[0]);
			// if (parts[3].length() != 0) {
			// adm1Instance = adm1.getInstance(parts[1], countryInstance);
			// adm2Instance = adm2.getInstance(parts[2], adm1Instance);
			// admInstance = adm3.getInstance(parts[3], adm2Instance);
			// } else if (parts[2].length() != 0) {
			// adm1Instance = adm1.getInstance(parts[1], countryInstance);
			// admInstance = adm2.getInstance(parts[2], adm1Instance);
			// } else if (parts[1].length() != 0) {
			// admInstance = adm1.getInstance(parts[1], countryInstance);
			// } else {
			// admInstance = countryInstance;
			// }
			// String truc = parts[4] + "_" + parts[5] + "_" + parts[6];
			// List<Generic> sglList = Collections.singletonList(admInstance);
			// List<Generic> sglList0 = Collections.emptyList();
			// AbstractCache cache = engine.getCurrentCache();
			// start = System.currentTimeMillis();
			//
			// // cityInstance = city.addInstance(parts[4] + "_" + parts[5] + "_" + parts[6], admInstance);
			// cityInstance = cache.buildAndPlug(null, city, sglList0, truc, sglList);
			//
			// i++;
			// if (i == 100) {
			// System.out.println(parts[4]);
			// engine.getCurrentCache().flush();
			// i = 0;
			// }
			// } catch (Exception e) {
			// // TODO Auto-generated catch block
			// e.printStackTrace();
			// }
			// }
			// } catch (Exception e) {
			// // TODO Auto-generated catch block
			// e.printStackTrace();
			// //
			// }

		}

	}

}
