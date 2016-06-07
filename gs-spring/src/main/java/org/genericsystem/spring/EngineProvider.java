package org.genericsystem.spring;

import java.util.Arrays;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Power;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

//@ApplicationScoped
@Component
// @Configuration
public class EngineProvider {

	protected static Logger log = LoggerFactory.getLogger(EngineProvider.class);

	private transient Engine engine;

	@Autowired
	private UserClassesProvider userClassesProvider;

	@Autowired
	private PersistentDirectoryProvider persistentDirectoryProvider;

	@Autowired
	private CacheRequestProvider cacheRequestProvider;

	@PostConstruct
	public void init() {
		String logo = "\n";
		logo += ("____________________________________________________________________________________________________________\n");
		logo += ("|___________________________________________________________________________________________________________|\n");
		logo += ("|___________________________________________________________________________________________________________|\n");
		logo += ("|____________|         ____                      _      ____             __                  /______________|\n");
		logo += ("|____________|        / ___)___  _  _____  ___  /_)__  / ___)_  __ ___  / /  ___  ____      /_______________|\n");
		logo += ("|____________|       / /___/ __)/ \\/ / __)/ _ )/ |/ _)/___ \\/ \\/  ) __)/___)/ __)/    )    /________________|\n");
		logo += ("|____________|      / /_  / __)/    / __)/   \\/  / /_ ___/ /\\    (__  / /_ / __)/ / / /   /_________________|\n");
		logo += ("|____________|      \\____(____(_/\\_(____(_/\\_(__(____(____/  \\  (____(____(____(_/_/_/   /__________________|\n");
		logo += ("|____________|                                               /_/                        /___________________|\n");
		logo += ("|____________|_________________________________________________________________________/____________________|\n");
		logo += ("|___________________________________________________________________________________________________________|\n");
		logo += ("|___________________________________________________________________________________________________________|  \n");

		log.info(logo);
		log.info("-----------------------------------------------------------------------------------------------");
		log.info("-  directory path : " + persistentDirectoryProvider.getDirectoryPath());
		log.info("-  userClasses : " + Arrays.toString(userClassesProvider.getUserClassesArray()));
		log.info("-----------------------------------------------------------------------------------------------");

		engine = new Engine(() -> cacheRequestProvider.getCurrentCache(), persistentDirectoryProvider.getDirectoryPath(),
		// userClassesProvider.getUserClassesArray());
				Car.class, CarColor.class, Power.class);
	}

	// @Bean
	public Engine getEngine() {
		return engine;
	}

	@PreDestroy
	public void destroy() {
		log.info("Generic System is currently stopping...");
		engine.close();
		engine = null;
		log.info("Generic System is stopped");
	}
}
