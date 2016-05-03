//package org.genericsystem.kernel;
//
//import java.util.Collections;
//import org.genericsystem.api.core.ApiStatics;
//import org.genericsystem.common.AbstractCache;
//import org.genericsystem.common.Generic;
//import org.genericsystem.common.IDifferential;
//
///**
// * @author Nicolas Feybesse
// *
// */
//public class BasicdEngine extends AbstractServer implements Generic {
//
//	@Override
//	public BasicEngine getRoot() {
//		return this;
//	}
//
//	public BasicEngine() {
//		this(new Class[] {});
//	}
//
//	public BasicEngine(Class<?>... userClasses) {
//		this(Statics.ENGINE_VALUE, userClasses);
//	}
//
//	public BasicEngine(String value, Class<?>... userClasses) {
//		this(value, null, userClasses);
//	}
//
//	public BasicEngine(String value, String persistentDirectoryPath, Class<?>... userClasses) {
//		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), value, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.SYSTEM_TS));
//		startSystemCache(userClasses);
//		archiver = new Archiver(this, persistentDirectoryPath);
//		isInitialized = true;
//	}
//
//	@Override
//	public AbstractCache newCache() {
//		return new AbstractCache(this) {
//
//			@Override
//			protected IDifferential<Generic> buildTransaction() {
//				return new Transaction(BasicEngine.this);
//			}
//
//			@Override
//			protected Generic plug(Generic generic) {
//				return ((Transaction) getTransaction()).plug(generic);
//			}
//
//			@Override
//			protected void unplug(Generic generic) {
//				((Transaction) getTransaction()).unplug(generic);
//			}
//		};
//	}
//
// }
