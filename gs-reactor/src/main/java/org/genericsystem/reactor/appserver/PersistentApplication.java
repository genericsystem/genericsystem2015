package org.genericsystem.reactor.appserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.html.HtmlApp;

public class PersistentApplication {
	private final Class<? extends HtmlApp<?>> htmlAppClass;
	private final AbstractRoot engine;
	private final Class<? extends ModelContext> modelClass;

	public PersistentApplication(Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends ModelContext> modelClass, AbstractRoot engine) {
		this.htmlAppClass = htmlAppClass;
		this.modelClass = modelClass;
		this.engine = engine;
	}

	public AbstractRoot getEngine() {
		return engine;
	}

	public Class<? extends HtmlApp<?>> getApplicationClass() {
		return htmlAppClass;
	}

	public void close() {
		engine.close();
	}

	@SuppressWarnings("unchecked")
	public Tag<?> newHtmlApp(ServerWebSocket socket) {
		try {
			return ((HtmlApp<ModelContext>) getApplicationClass().getConstructor(AbstractRoot.class, ServerWebSocket.class).newInstance(getEngine(), socket)).init(modelClass.getConstructor(AbstractRoot.class).newInstance(engine));
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
