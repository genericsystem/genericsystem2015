package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.distributed.cacheonserver.ui.exemple.AppHtml;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.kernel.Engine;

public class PersistentApplication {
	private final Class<? extends AppHtml> clazz;
	private AbstractRoot engine;

	public PersistentApplication(Class<? extends AppHtml> clazz, AbstractRoot engine) {
		this.clazz = clazz;
		this.engine = engine;
		assert engine != null;
	}

	public AbstractRoot getEngine() {
		return engine;
	}

	public Class<? extends AppHtml> getApplicationClass() {
		return clazz;
	}

	public void close() {
		engine.close();
	}

	public HtmlElement newHtmlApp(ServerWebSocket socket) {
		try {
			return getApplicationClass().getConstructor(Engine.class, ServerWebSocket.class).newInstance(getEngine(), socket).init();
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

}
