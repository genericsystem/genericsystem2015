package org.genericsystem.cache;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import org.genericsystem.kernel.Root;
import org.genericsystem.kernel.Statics;

public abstract class AbstractGSServer extends AbstractVerticle {

	private Map<String, Root> roots;// must be shared if several verticles instances

	@SuppressWarnings("unchecked")
	@Override
	public void start() {
		roots = new HashMap<String, Root>();
		for (JsonObject engineJson : (List<JsonObject>) config().getJsonArray("engines").getList()) {
			String engineName = engineJson.getString("engineName");
			engineName = engineName == null ? Statics.ENGINE_VALUE : engineName;
			Root root = new Root(engineName, engineJson.getString("persistanceRepositoryPath"));
			roots.put("/" + engineName, root);
			System.out.println("Mount engine : " + "/" + engineName);
		}
	}

	protected Map<String, Root> getRoots() {
		return roots;
	}

	protected Handler<Buffer> getHandler(Root root, Consumer<Buffer> sender) {
		return buffer -> {
			GSBuffer gsBuffer = new GSBuffer(buffer);
			int id = gsBuffer.getInt();
			int methodId = gsBuffer.getInt();
			GSBuffer replyBuffer = new GSBuffer(Buffer.buffer());
			replyBuffer.appendInt(id).appendInt(methodId);
			// System.out.println("Server will respond to id : " + id);
			switch (methodId) {
			case WebSocketGSClient.PICK_NEW_TS: {
				replyBuffer.appendLong(root.pickNewTs());
				break;
			}
			case WebSocketGSClient.GET_DEPENDENCIES: {
				replyBuffer.appendGSLongArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
				break;
			}
			case WebSocketGSClient.GET_VERTEX: {
				replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
				break;
			}
			case WebSocketGSClient.APPLY: {
				try {
					root.apply(gsBuffer.getLong(), gsBuffer.getGSLongArray(), gsBuffer.getGSVertexArray());
					replyBuffer.appendLong(0);
				} catch (Exception e) {
					e.printStackTrace();
					throw new IllegalStateException(e);
				}
				break;
			}
			default:
				throw new IllegalStateException("unable to find method:" + methodId + " " + "id :" + id);
			}
			sender.accept(replyBuffer);
		};
	}
}
