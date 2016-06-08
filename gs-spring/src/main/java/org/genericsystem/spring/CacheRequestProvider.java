package org.genericsystem.spring;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.genericsystem.kernel.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

@Configuration
@Component
@Lazy
// @Scope("request")
public class CacheRequestProvider {

	@Lazy
	@Autowired
	private transient CacheSessionProvider cacheSessionProvider;

	@PostConstruct
	public void init() {
		cacheSessionProvider.getCurrentCache().start();
	}

	// @Produces
	@Bean
	@Lazy
	public Cache getCurrentCache() {
		return cacheSessionProvider.getCurrentCache();
	}

	@PreDestroy
	public void preDestroy() {
		cacheSessionProvider.getCurrentCache().stop();
	}

}
