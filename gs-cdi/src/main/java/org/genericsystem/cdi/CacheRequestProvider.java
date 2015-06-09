package org.genericsystem.cdi;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.enterprise.context.RequestScoped;
import javax.enterprise.inject.Produces;
import javax.inject.Inject;
import org.genericsystem.mutability.Cache;

@RequestScoped
public class CacheRequestProvider {

	@Inject
	private transient CacheSessionProvider cacheSessionProvider;

	@PostConstruct
	public void init() {
		cacheSessionProvider.getCurrentCache().start();
	}

	@Produces
	public Cache getCurrentCache() {
		return cacheSessionProvider.getCurrentCache();
	}

	@PreDestroy
	public void preDestroy() {
		cacheSessionProvider.getCurrentCache().stop();
	}

}
