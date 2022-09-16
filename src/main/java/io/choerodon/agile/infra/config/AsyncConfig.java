package io.choerodon.agile.infra.config;

import java.util.concurrent.Executor;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurerSupport;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.concurrent.DelegatingSecurityContextExecutorService;

import io.choerodon.agile.infra.aspect.MessageAspect;

/**
 * @author superlee
 * @since 2020-04-29
 */
@Configuration
public class AsyncConfig extends AsyncConfigurerSupport {

    /**
     * 异步线程池
     * <br/>
     * <p>
     * 此线程池使用了线程修饰器，在进入线程时传入了调用线程的线程变量, 用来控制消息发送
     * <p>
     * {@link org.springframework.core.task.TaskDecorator}
     *
     * @return 基于security的线程池
     */
    @Override
    public Executor getAsyncExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(5);
        executor.setMaxPoolSize(15);
        executor.setQueueCapacity(100);
        executor.setThreadNamePrefix("asyncTaskExecutor-");
        // 线程修饰器，用于屏蔽消息发送
        executor.setTaskDecorator(runnable -> {
            // 需要在原线程里先拿到变量，在塞入异步线程中生效
            Boolean sendMsgFlag = MessageAspect.SEND_MSG_FLAG.get();
            return () -> {
                MessageAspect.SEND_MSG_FLAG.set(sendMsgFlag);
                runnable.run();
                MessageAspect.SEND_MSG_FLAG.remove();
            };
        });
        executor.initialize();
        return new DelegatingSecurityContextExecutorService(executor.getThreadPoolExecutor());
    }

    /**
     * 工作列表导入线程池
     *
     * @return
     */
    @Bean
    public Executor issueImportExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(5);
        executor.setMaxPoolSize(15);
        executor.setQueueCapacity(100);
        executor.setThreadNamePrefix("issueImportExecutor-");
        executor.initialize();
        return new DelegatingSecurityContextExecutorService(executor.getThreadPoolExecutor());
    }
}
