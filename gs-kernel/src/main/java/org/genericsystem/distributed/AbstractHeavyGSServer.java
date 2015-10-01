package org.genericsystem.distributed;

import io.vertx.core.buffer.Buffer;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.genericsystem.kernel.HeavyServerEngine;

public abstract class AbstractHeavyGSServer extends AbstractGSServer {
	public AbstractHeavyGSServer(GSDeploymentOptions options) {
		super(options);
	}

	Buffer getReplyBuffer(int methodId, HeavyServerEngine root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer();
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLong(root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.GET_VERTEX:
			return replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
		case AbstractGSClient.SHIFT_TS:
			return replyBuffer.appendLong(root.shiftTs(gsBuffer.getLong()));
		case AbstractGSClient.ADD_INSTANCE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendGSVertex(root.addInstance(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.SET_INSTANCE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendLong(root.setInstance(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.MERGE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendLong(root.merge(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.UPDATE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendGSVertex(root.update(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.REMOVE:
			return replyBuffer.appendLong(root.remove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.FORCE_REMOVE:
			return replyBuffer.appendLong(root.forceRemove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.CONSERVE_REMOVE:
			return replyBuffer.appendLong(root.conserveRemove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.TRY_FLUSH:
			return replyBuffer.appendLong(root.tryFlush(gsBuffer.getLong()));
		case AbstractGSClient.FLUSH:
			System.out.println("FLUSH !!!!!!!!!!!!!!!");
			return replyBuffer.appendLong(root.flush(gsBuffer.getLong()));
		case AbstractGSClient.MOUNT:
			return replyBuffer.appendLong(root.mount(gsBuffer.getLong()));
		case AbstractGSClient.UNMOUNT:
			return replyBuffer.appendLong(root.unmount(gsBuffer.getLong()));
		case AbstractGSClient.GET_CACHE_LEVEL:
			return replyBuffer.appendInt(root.getCacheLevel(gsBuffer.getLong()));
		case AbstractGSClient.NEW_CACHE:
			return replyBuffer.appendLong(root.newCacheId());
		default:
			throw new IllegalStateException("unable to find method : " + methodId);
		}
	}
}
