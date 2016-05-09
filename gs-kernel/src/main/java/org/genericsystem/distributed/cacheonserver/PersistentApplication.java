package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.kernel.Engine;

public class PersistentApplication {
	private final Class<? extends HtmlApp> clazz;
	private AbstractRoot engine;

	public PersistentApplication(Class<? extends HtmlApp> clazz, AbstractRoot engine) {
		this.clazz = clazz;
		this.engine = engine;
		assert engine != null;
	}

	public AbstractRoot getEngine() {
		return engine;
	}

	public Class<? extends HtmlApp> getApplicationClass() {
		return clazz;
	}

	public void close() {
		engine.close();
	}

	public HtmlApp newHtmlApp(ServerWebSocket socket) {
		try {
			return getApplicationClass().getConstructor(Engine.class, ServerWebSocket.class).newInstance(getEngine(), socket);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

}
