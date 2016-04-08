package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.http.ServerWebSocket;
import java.lang.reflect.InvocationTargetException;
import org.genericsystem.distributed.Closable;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.kernel.AbstractServer;
import org.genericsystem.kernel.Engine;

public class PersistantApplication implements Closable {
	private final Class<? extends HtmlApp> clazz;
	private AbstractServer engine;

	public PersistantApplication(Class<? extends HtmlApp> clazz, AbstractServer engine) {
		this.clazz = clazz;
		this.engine = engine;
	}

	public AbstractServer getEngine() {
		return engine;
	}

	public Class<? extends HtmlApp> getApplicationClass() {
		return clazz;
	}

	@Override
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
