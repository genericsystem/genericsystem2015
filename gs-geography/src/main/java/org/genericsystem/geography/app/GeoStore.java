package org.genericsystem.geography.app;

import java.io.BufferedReader;
import java.io.FileReader;
import java.lang.invoke.MethodHandles;
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
import org.genericsystem.geography.model.Place;
import org.genericsystem.geography.model.Place.Latitude;
import org.genericsystem.geography.model.Place.Longitude;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunScript(GeoScript.class)
@DependsOnModel({ AdministrativeTerritory.class, Country.class, Place.class, City.class, Building.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class,
		TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class })
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Country.class, Adm1.class, Adm2.class,
		Adm3.class, City.class })
public class GeoStore extends RootTagImpl {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoStore.class, "/GeoApp");
	}

	public static void readFile(String filename, Root engine, Generic returnType, int n) {
		try {
			BufferedReader reader;
			String line;
			reader = new BufferedReader(new FileReader(filename));
			while ((line = reader.readLine()) != null)
				readLine(line, engine, returnType, n);
			reader.close();
		} catch (Exception e) {
			logger.warn("Exception while reading file {}.", filename, e);
		}
		engine.getCurrentCache().flush();
	}

	public static String componentCode(String[] parts, int n) {
		String code = parts[0];
		for (int i = 1; i < n - 1; i++)
			code += "_" + parts[i];
		return code;
	}

	private static HashMap<String, Generic> hm;

	public static void readLine(String line, Root engine, Generic returnType, int n) {
		String[] parts = line.split("\\|");
		if ("FR".equals(parts[0])) {
			System.out.println("@" + line);
			Generic currentInstance;
			String currentCode;
			if (returnType.getBaseComponent() == null) {
				currentCode = parts[parts.length - 2];
				currentInstance = returnType.addInstance(parts[n]);
				hm.put(currentCode, currentInstance);
			} else {
				String componentCode = componentCode(parts, n);
				currentCode = componentCode + "_" + parts[n - 1];
				Generic componentInstance = hm.get(componentCode);
				if (componentInstance != null) {
					// currentGeneric = returnType.addInstance(parts[parts.length - 1], componentGeneric);
					String currentValue;
					if (returnType == engine.find(City.class)) {
						if ("44".equals(parts[2])) {
							currentInstance = engine.getCurrentCache().buildAndPlug(null, returnType,
									Collections.emptyList(), parts[n - 1],
									Collections.singletonList(componentInstance));
							Generic latInstance = engine.getCurrentCache().buildAndPlug(null,
									engine.find(Latitude.class), Collections.emptyList(), Double.valueOf(parts[n]),
									Collections.singletonList(currentInstance));
							Generic lonInstance = engine.getCurrentCache().buildAndPlug(null,
									engine.find(Longitude.class), Collections.emptyList(), Double.valueOf(parts[n + 1]),
									Collections.singletonList(currentInstance));
							hm.put(currentCode, currentInstance);
						}
					} else {
						currentValue = parts[n];
						currentInstance = engine.getCurrentCache().buildAndPlug(null, returnType,
								Collections.emptyList(), parts[n], Collections.singletonList(componentInstance));
						hm.put(currentCode, currentInstance);
					}
				}
			}
		}
	}

	public static class GeoScript implements Script {

		@Override
		public void run(Root engine) {

			hm = new HashMap<>();

			readFile("src/main/resources/countries.csv", engine, engine.find(Country.class), 1);

			readFile("src/main/resources/adm1.csv", engine, engine.find(Adm1.class), 2);

			readFile("src/main/resources/adm2.csv", engine, engine.find(Adm2.class), 3);

			readFile("src/main/resources/adm3.csv", engine, engine.find(Adm3.class), 4);

			readFile("src/main/resources/populated_fr.csv", engine, engine.find(City.class), 5);

			// Generic castle = engine.find(Castle.class);
			// System.out.println(castle);
			//
			// Generic cityInstance = engine.find(City.class).getInstance("Nantes", hm.get("FR_52_44_442"));
			// System.out.println(cityInstance.info());
			// Generic castleInstance = castle.addInstance("château des ducs de Bretagne", cityInstance);
			// Generic latInstance = engine.getCurrentCache().buildAndPlug(null, engine.find(Latitude.class),
			// Collections.emptyList(), 47.2162, Collections.singletonList(castleInstance));
			// Generic lonInstance = engine.getCurrentCache().buildAndPlug(null, engine.find(Longitude.class),
			// Collections.emptyList(), -1.5497, Collections.singletonList(castleInstance));
			//
			// engine.getCurrentCache().flush();
			//
			// System.out.println(castleInstance.info());
			// System.out.println(engine.find(Castle.class).getInstances().stream().collect(Collectors.toList()));

		}

	}

}
