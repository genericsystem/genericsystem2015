package org.genericsystem.reactor.flex;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.model.EngineModel;

public class GenericApp extends HtmlApp<EngineModel> {

	public GenericApp(ServerWebSocket webSocket) {
		super(webSocket);
	}

}
