package io.choerodon.agile.infra.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurerSupport;
import org.springframework.security.concurrent.DelegatingSecurityContextExecutorService;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

/**
 * @author superlee
 * @since 2020-04-29
 */
@Configuration
public class AsyncConfig extends AsyncConfigurerSupport {

    @Override
    public Executor getAsyncExecutor() {
        return new DelegatingSecurityContextExecutorService(Executors.newFixedThreadPool(15));
    }

    /**
     * 工作列表导入线程池
     *
     * @return
     */
    @Bean
    public Executor issueImportExecutor() {
        return new DelegatingSecurityContextExecutorService(Executors.newFixedThreadPool(15));
    }
}
