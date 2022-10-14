package io.choerodon.agile.infra.aspect;

import org.apache.commons.lang3.BooleanUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 * <p>
 * 消息切面， 用线程变量控制消息发送
 * <p>
 * 直接拦截h0原生发送消息的所有方法, 可拦截所有插件
 * {@link org.hzero.boot.message.MessageClient}
 * <p>
 * FIXME 此处可优化, 但是需要插件单独优化, 即拦截刚进入异步线程时各组件的{@link io.choerodon.agile.infra.utils.SendMsgUtil}, 可避免一些无用查询
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/9/16
 */
@Aspect
@Component
public class MessageAspect {

    private static final Logger LOGGER = LoggerFactory.getLogger(MessageAspect.class);

    /**
     * 线程变量-用来处理敏捷全局的消息发送
     * <p>
     * 默认值：true（发送）
     */
    public static final ThreadLocal<Boolean> SEND_MSG_FLAG = new ThreadLocal<>();

    @Pointcut("execution(* org.hzero.boot.message.MessageClient.send*(..))")
    public void messagePointcut() {
    }

    @Around("messagePointcut()")
    public Object messageAspect(ProceedingJoinPoint pjp) {
        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("发送消息拦截");
        }
        try {
            if (BooleanUtils.isNotFalse(SEND_MSG_FLAG.get())) {
                return pjp.proceed();
            }
        } catch (Throwable e) {
            LOGGER.error("消息发送失败", e);
        }
        return null;
    }

}
