package io.choerodon.agile.infra.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.event.ApplicationFailedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * 应用启动失败，打印出错误堆栈信息，方便排错
 * onApplicationEvent事件中抛异常不会在控制台输出，只能logger打印
 *
 * @author superlee
 * @since 2020-05-15
 */
@Component
public class ApplicationFailedListener implements ApplicationListener<ApplicationFailedEvent> {

    public static final Logger logger = LoggerFactory.getLogger(ApplicationFailedListener.class);

    @Override
    public void onApplicationEvent(ApplicationFailedEvent event) {
        logger.error("application start up failed, exception: {}", event.getException());
    }
}
