package org.genericsystem.spring;

import java.io.Serializable;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.genericsystem.kernel.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Scope("prototype")
@Component
@Lazy
public class CacheSessionProvider implements Serializable {

	private static final long serialVersionUID = 5201003234496546928L;

	@Autowired
	@Lazy
	private transient Engine engine;

	private transient Cache currentCache;

	@PostConstruct
	public void init() {
		currentCache = engine.newCache();
	}

	public Cache getCurrentCache() {
		return currentCache;
	}

	@PreDestroy
	public void preDestroy() {
		currentCache = null;
	}

}
