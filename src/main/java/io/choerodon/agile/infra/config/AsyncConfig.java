package io.choerodon.agile.infra.config;

import java.util.concurrent.Executor;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskDecorator;
import org.springframework.scheduling.annotation.AsyncConfigurerSupport;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.concurrent.DelegatingSecurityContextExecutorService;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import io.choerodon.agile.infra.aspect.MessageAspect;

import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.hzero.starter.keyencrypt.core.EncryptType;

/**
 * @author superlee
 * @since 2020-04-29
 */
@Configuration
public class AsyncConfig extends AsyncConfigurerSupport {

    /**
     * 线程装饰器, 用以拷贝各种线程变量
     */
    private static final TaskDecorator TASK_DECORATOR = runnable -> {
        // 屏蔽消息发送配置
        final Boolean sendMsgFlag = MessageAspect.SEND_MSG_FLAG.get();
        // 主键加密配置
        final EncryptType encryptType = EncryptContext.encryptType();
        // HTTP请求头
        final RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        // SecurityContext
        final SecurityContext securityContext = SecurityContextHolder.getContext();
        return () -> {
            // 获取原值
            final Boolean originSendMsgFlag = MessageAspect.SEND_MSG_FLAG.get();
            final EncryptType originEncryptType = EncryptContext.encryptType();
            final RequestAttributes originRequestAttributes = RequestContextHolder.getRequestAttributes();
            final SecurityContext originSecurityContext = SecurityContextHolder.getContext();
            // 设置父线程的值
            MessageAspect.SEND_MSG_FLAG.set(sendMsgFlag);
            EncryptContext.setEncryptType(String.valueOf(encryptType));
            RequestContextHolder.setRequestAttributes(requestAttributes);
            SecurityContextHolder.setContext(securityContext);
            // 真实子线程执行
            runnable.run();
            // 清除父线程的值
            MessageAspect.SEND_MSG_FLAG.set(originSendMsgFlag);
            EncryptContext.setEncryptType(String.valueOf(originEncryptType));
            RequestContextHolder.setRequestAttributes(originRequestAttributes);
            SecurityContextHolder.setContext(originSecurityContext);
        };
    };

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
        executor.setCorePoolSize(4);
        executor.setMaxPoolSize(16);
        executor.setQueueCapacity(200);
        executor.setThreadNamePrefix("asyncTaskExecutor-");
        executor.setTaskDecorator(TASK_DECORATOR);
        executor.initialize();
        return executor;
    }

    /**
     * 工作列表导入线程池
     *
     * @return
     */
    @Bean
    public Executor issueImportExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(4);
        executor.setMaxPoolSize(16);
        executor.setQueueCapacity(200);
        executor.setThreadNamePrefix("issueImportExecutor-");
        executor.setTaskDecorator(TASK_DECORATOR);
        executor.initialize();
        return new DelegatingSecurityContextExecutorService(executor.getThreadPoolExecutor());
    }

}
