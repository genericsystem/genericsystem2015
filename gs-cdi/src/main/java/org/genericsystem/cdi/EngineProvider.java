package org.genericsystem.cdi;

import java.util.Arrays;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.inject.Produces;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ApplicationScoped
public class EngineProvider {

	protected static Logger log = LoggerFactory.getLogger(EngineProvider.class);

	private transient Engine engine;

	@Inject
	private UserClassesProvider userClassesProvider;

	@Inject
	private PersistentDirectoryProvider persistentDirectoryProvider;

	@Inject
	private CacheRequestProvider cacheRequestProvider;

	@PostConstruct
	public void init() {
		log.info("$$$$$$$$$$$$$$ START GS ENGINE V3 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");

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

		engine = new Engine(() -> cacheRequestProvider.getCurrentCache(), persistentDirectoryProvider.getEngineValue(), persistentDirectoryProvider.getDirectoryPath(), userClassesProvider.getUserClassesArray());
	}

	@Produces
	public Engine getEngine() {
		return engine;
	}

	@PreDestroy
	public void destroy() {
		log.info("$$$$$$$$$$$$$$ STOP GS ENGINE $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
		engine.close();
		engine = null;
	}
}
