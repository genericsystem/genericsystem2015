package org.genericsystem.cdi;

import java.io.Serializable;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import org.genericsystem.common.HeavyCache;

@SessionScoped
public class CacheSessionProvider implements Serializable {

	private static final long serialVersionUID = 5201003234496546928L;

	@Inject
	private transient Engine engine;

	private transient HeavyCache currentCache;

	@PostConstruct
	public void init() {
		currentCache = engine.newCache();
	}

	public HeavyCache getCurrentCache() {
		return currentCache;
	}

	@PreDestroy
	public void preDestroy() {
		currentCache = null;
	}

}
