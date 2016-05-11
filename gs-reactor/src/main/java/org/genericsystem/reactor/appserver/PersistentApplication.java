package org.genericsystem.reactor.appserver;

import io.vertx.core.http.ServerWebSocket;
import java.lang.reflect.InvocationTargetException;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlApp;

public class PersistentApplication {
	private final Class<? extends HtmlApp<?>> clazz;
	private AbstractRoot engine;

	public PersistentApplication(Class<? extends HtmlApp<?>> clazz, AbstractRoot engine) {
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

	public HtmlElement newHtmlApp(ServerWebSocket socket) {
		try { KK
			return getApplicationClass().getConstructor(Model.class, ServerWebSocket.class).newInstance(getEngine(), socket).init();
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException
				| SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
