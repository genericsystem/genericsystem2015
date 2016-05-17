package org.genericsystem.reactor.appserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlApp;

public class PersistentApplication {
	private final Class<? extends HtmlApp<?>> htmlAppClass;
	private final AbstractRoot engine;
	private final Class<? extends Model> modelClass;

	public PersistentApplication(Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends Model> modelClass,
			AbstractRoot engine) {
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
	public HtmlElement<?, ?, ?> newHtmlApp(ServerWebSocket socket) {
		System.out.println("newHtmlApp socket: " + socket.getClass().getSimpleName());
		try {
			return ((HtmlApp<Model>) getApplicationClass().getConstructor(AbstractRoot.class, ServerWebSocket.class)
					.newInstance(socket)).init(modelClass.getConstructor(AbstractRoot.class).newInstance(engine));
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
