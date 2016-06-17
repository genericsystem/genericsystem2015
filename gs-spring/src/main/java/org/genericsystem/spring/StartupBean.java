package org.genericsystem.spring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.annotation.Scope;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.classreading.CachingMetadataReaderFactory;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.ClassUtils;
import org.springframework.util.SystemPropertyUtils;

@Scope("singleton")
@Component
public class StartupBean /* implements Extension */{

	private final Logger log = LoggerFactory.getLogger(StartupBean.class);
	private final String basePackage = "org.genericsystem.models";

	private void findMyTypes(ApplicationContext applicationContext) throws IOException, ClassNotFoundException {

		UserClassesProvider userClasses = applicationContext.getBean(UserClassesProvider.class);

		ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();
		MetadataReaderFactory metadataReaderFactory = new CachingMetadataReaderFactory(resourcePatternResolver);
		String packageSearchPath = ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX + resolveBasePackage(basePackage) + "/" + "**/*.class";
		Resource[] resources = resourcePatternResolver.getResources(packageSearchPath);
		List<Class<?>> resourcesClasses = new ArrayList<>();
		for (Resource resource : resources) {
			if (resource.isReadable()) {
				MetadataReader metadataReader = metadataReaderFactory.getMetadataReader(resource);
				try {
					Class c = Class.forName(metadataReader.getClassMetadata().getClassName());
					if (c.getAnnotation(SystemGeneric.class) != null) {
						log.info("Generic System: providing " + Class.forName(metadataReader.getClassMetadata().getClassName()));
						resourcesClasses.add(Class.forName(metadataReader.getClassMetadata().getClassName()));
					}
				} catch (Throwable e) {
				}
			}
		}
		userClasses.setUserClasse(resourcesClasses);
		// return candidates;
	}

	private String resolveBasePackage(String basePackage) {
		return ClassUtils.convertClassNameToResourcePath(SystemPropertyUtils.resolvePlaceholders(basePackage));
	}

	@EventListener
	public void onApplicationEvent(ApplicationEvent event) throws ClassNotFoundException, IOException {
		// if (event instanceof org.springframework.context.event.ContextStartedEvent)

		if (event instanceof ContextRefreshedEvent) {
			ApplicationContext applicationContext = ((ContextRefreshedEvent) event).getApplicationContext();
			findMyTypes(applicationContext);
		}

	}
}
