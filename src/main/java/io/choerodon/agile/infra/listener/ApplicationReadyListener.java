package io.choerodon.agile.infra.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

import org.hzero.core.redis.RedisHelper;

/**
 * @author superlee
 * @since 2023-01-17
 */
@Component
public class ApplicationReadyListener implements ApplicationListener<ApplicationReadyEvent> {

    public static final Logger logger = LoggerFactory.getLogger(ApplicationReadyListener.class);

    @Autowired
    private RedisHelper redisHelper;

    /**
     * 接收到ContextRefreshedEvent事件
     *
     * @param event event
     */
    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        logger.info("服务启动开始删除projectInfo redis缓存");
        redisHelper.deleteKeysWithPrefix("projectInfo");
        logger.info("服务启动删除projectInfo redis缓存成功");
    }
}