package org.genericsystem.spring;

import java.util.HashMap;

import org.genericsystem.common.AbstractCache;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.CustomScopeConfigurer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.support.SimpleThreadScope;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.web.context.WebApplicationContext;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = AbstractTest.TestConfig.class)
@WebAppConfiguration
public abstract class AbstractTest {

	protected static Logger log = LoggerFactory.getLogger(AbstractTest.class);

	@Autowired
	Engine engine;

	@Autowired
	@Lazy
	AbstractCache cacheProvider;

	/*
	 * protected MockHttpSession session; protected MockHttpServletRequest request;
	 * 
	 * protected void startSession() { session = new MockHttpSession(); }
	 * 
	 * protected void endSession() { session.clearAttributes(); session = null; }
	 * 
	 * protected void startRequest() { request = new MockHttpServletRequest(); request.setSession(session); RequestContextHolder.setRequestAttributes(new
	 * ServletRequestAttributes(request)); }
	 * 
	 * protected void endRequest() { ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).requestCompleted();
	 * RequestContextHolder.resetRequestAttributes(); request = null; }
	 */
	@Configuration
	@ComponentScan("org.genericsystem.spring")
	public static class TestConfig {

		@Bean
		public CustomScopeConfigurer customScopeConfigurer() {
			CustomScopeConfigurer scopeConfigurer = new CustomScopeConfigurer();

			HashMap<String, Object> scopes = new HashMap<String, Object>();
			scopes.put(WebApplicationContext.SCOPE_REQUEST, new SimpleThreadScope());
			scopes.put(WebApplicationContext.SCOPE_SESSION, new SimpleThreadScope());
			scopeConfigurer.setScopes(scopes);

			return scopeConfigurer;

		}

	}
}