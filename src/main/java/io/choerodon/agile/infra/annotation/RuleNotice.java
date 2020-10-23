package io.choerodon.agile.infra.annotation;

import java.lang.annotation.*;

import io.choerodon.agile.api.vo.NoticeEventVO;

/**
 * 页面规则消息通知
 * 可监听{@link NoticeEventVO}事件实现消息处理
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午2:33
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface RuleNotice {
    
    String value() default "ISSUE";

    String event();

    String fieldListName() default "";
    
    String instanceIdNameInReturn() default "id";
}
