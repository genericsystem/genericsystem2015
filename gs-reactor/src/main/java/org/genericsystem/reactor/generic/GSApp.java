package org.genericsystem.reactor.generic;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.model.EngineModel;

public class GSApp extends HtmlApp<EngineModel> {

	public GSApp(ServerWebSocket webSocket) {
		super(webSocket);
	}

}
