package io.choerodon.agile.infra.config;

import org.hzero.core.message.MessageAccessor;
import org.springframework.beans.factory.SmartInitializingSingleton;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author superlee
 * @since 2022-08-08
 */
@Configuration
public class AgileMessagesLoaderConfig {

    @Bean
    public SmartInitializingSingleton agileMessagesLoader() {
        return () -> {
            MessageAccessor.addBasenames(new String[]{"classpath:messages/messages_agile"});
        };
    }
}
