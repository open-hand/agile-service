package io.choerodon.agile.infra.config;

import java.util.concurrent.Executor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.concurrent.DelegatingSecurityContextExecutorService;

import io.choerodon.core.config.async.ChoerodonTaskDecorator;

/**
 * @author superlee
 * @since 2020-04-29
 */
@Configuration
public class AsyncConfiguration {

    @Autowired
    private ChoerodonTaskDecorator choerodonTaskDecorator;

    /**
     * 工作列表导入线程池
     *
     * @return return
     */
    @Bean
    public Executor issueImportExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(4);
        executor.setMaxPoolSize(16);
        executor.setQueueCapacity(200);
        executor.setThreadNamePrefix("issueImportExecutor-");
        executor.setTaskDecorator(choerodonTaskDecorator);
        executor.initialize();
        return new DelegatingSecurityContextExecutorService(executor.getThreadPoolExecutor());
    }

}
