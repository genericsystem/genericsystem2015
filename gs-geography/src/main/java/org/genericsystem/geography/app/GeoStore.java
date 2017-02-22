package org.genericsystem.geography.app;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Collections;
import java.util.HashMap;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.geography.app.GeoStore.GeoScript;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm1;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm2;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm3;
import org.genericsystem.geography.model.Building;
import org.genericsystem.geography.model.City;
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
@DependsOnModel({ AdministrativeTerritory.class, Country.class, City.class, Building.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class,
		TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class })
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Country.class, Adm1.class, Adm2.class,
		Adm3.class })
public class GeoStore extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoStore.class, "/GeoApp");
	}

	public static void readFile(String filename, Root engine, Generic returnType) {
		try {
			BufferedReader reader;
			String line;
			reader = new BufferedReader(new FileReader(filename));
			while ((line = reader.readLine()) != null)
				readLine(line, engine, returnType);
			reader.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		engine.getCurrentCache().flush();
		System.out.println("ok");
	}

	public static String componentCode(String[] parts) {
		String code = parts[0];
		for (int i = 1; i < parts.length - 2; i++)
			code += "_" + parts[i];
		return code;
	}

	private static HashMap<String, Generic> hm;

	public static void readLine(String line, Root engine, Generic returnType) {
		System.out.println("@" + line);
		String[] parts = line.split("\\|");
		// if (!"CD".equals(parts[0]) && !"CV".equals(parts[0]) && !"GE".equals(parts[0]) && !"TJ".equals(parts[0])) {
		Generic currentInstance;
		String currentCode;
		if (returnType.getBaseComponent() == null) {
			currentCode = parts[parts.length - 2];
			currentInstance = returnType.addInstance(parts[parts.length - 1]);
			hm.put(currentCode, currentInstance);
		} else {
			String componentCode = componentCode(parts);
			currentCode = componentCode + "_" + parts[parts.length - 2];
			Generic componentGeneric = hm.get(componentCode);
			// currentGeneric = returnType.addInstance(parts[parts.length - 1], componentGeneric);
			if (componentGeneric != null) {
				currentInstance = engine.getCurrentCache().buildAndPlug(null, returnType, Collections.emptyList(),
						parts[parts.length - 1], Collections.singletonList(componentGeneric));
				hm.put(currentCode, currentInstance);
			}
		}
		// }
	}

	public static class GeoScript implements Script {

		@Override
		public void run(Root engine) {

			hm = new HashMap<>();

			readFile("src/main/resources/countries.csv", engine, engine.find(Country.class));

			readFile("src/main/resources/adm1.csv", engine, engine.find(Adm1.class));

			readFile("src/main/resources/adm2.csv", engine, engine.find(Adm2.class));

			readFile("src/main/resources/adm3.csv", engine, engine.find(Adm3.class));

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
