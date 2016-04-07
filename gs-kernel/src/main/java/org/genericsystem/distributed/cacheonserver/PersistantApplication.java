package org.genericsystem.distributed.cacheonserver;

import org.genericsystem.distributed.Closable;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.kernel.Engine;

public class PersistantApplication implements Closable {
	private final Class<? extends HtmlApp> clazz;
	private Engine engine;

	public PersistantApplication(Class<? extends HtmlApp> clazz, Engine engine) {
		this.clazz = clazz;
		this.engine = engine;
	}

	public Engine getEngine() {
		return engine;
	}

	public Class<? extends HtmlApp> getApplicationClass() {
		return clazz;
	}

	@Override
	public void close() {
		engine.close();
	}

}
