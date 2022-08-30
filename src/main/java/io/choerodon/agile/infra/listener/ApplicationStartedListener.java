package io.choerodon.agile.infra.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

import io.choerodon.agile.app.service.ConfigCodeService;
import io.choerodon.agile.infra.statemachineclient.client.StateMachineClient;

/**
 * @author shinan.chen
 * @since 2019/7/25
 */
@Component
public class ApplicationStartedListener implements ApplicationListener<ApplicationStartedEvent> {

    public static final Logger LOGGER = LoggerFactory.getLogger(ApplicationStartedListener.class);
    @Autowired
    private StateMachineClient stateMachineClient;
    @Autowired
    private ConfigCodeService configCodeService;

    /**
     * 接收到ContextRefreshedEvent事件
     *
     * @param event event
     */
    @Override
    public void onApplicationEvent(ApplicationStartedEvent event) {
        LOGGER.info("ApplicationStartedListener:{}", event);
        configCodeService.handlePropertyData(stateMachineClient.getStateMachinePropertyData());
    }
}
